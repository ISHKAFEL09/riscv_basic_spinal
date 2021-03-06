package cod

import SimLib._
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

case class ReqPkg(wr: Boolean, addr: BigInt, data: BigInt) extends Package {
  override def toString: String = f" ${if (wr) "write" else "read"} addr: ${addr}%x, data: ${data}%x "
}
case class RespPkg(data: BigInt) extends Package {
  override def toString: String = f" Data: ${data}%x "
}

case class CacheReqDriver(bus: Stream[CacheRequest], clk: ClockDomain) extends Driver[ReqPkg](bus, clk) {
  override def send(pkg: ReqPkg): Unit = {
    clk.waitSamplingWhere(bus.ready.toBoolean)
    bus.payload.wr #= pkg.wr
    bus.payload.addr #= pkg.addr
    bus.payload.data #= pkg.data
    bus.valid #= true
    clk.waitSamplingWhere(bus.ready.toBoolean)
    bus.valid #= false
    debug(f"[Cache Req Driver] send package, ${if (pkg.wr) "write" else "read"} addr: ${pkg.addr}%x, data: ${pkg.data}%x")
  }
}

case class CacheReqMonitor(bus: Stream[CacheRequest], clk: ClockDomain) extends Monitor[ReqPkg](bus, clk) {
  override def sample(): ReqPkg = {
    clk.waitSamplingWhere(bus.valid.toBoolean && bus.ready.toBoolean)
    ReqPkg(bus.payload.wr.toBoolean, bus.payload.addr.toBigInt, bus.payload.data.toBigInt)
  }
}

case class CacheRespMonitor(bus: Stream[CacheResponse], clk: ClockDomain) extends Monitor[RespPkg](bus, clk) {
  override def sample(): RespPkg = {
    clk.waitSamplingWhere(bus.valid.toBoolean)
    RespPkg(bus.payload.data.toBigInt)
  }
}

case class MemReqMonitor(bus: Flow[MemRequest], clk: ClockDomain) extends Monitor[ReqPkg](bus, clk) {
  override def sample(): ReqPkg = {
    clk.waitSamplingWhere(bus.fire.toBoolean)
    ReqPkg(bus.payload.wr.toBoolean, bus.payload.addr.toBigInt, bus.payload.data.toBigInt)
  }
}

case class MemRespDriver(bus: Flow[MemResponse], clk: ClockDomain) extends Driver[ReqPkg](bus, clk) {
  val Mem = mutable.Map[BigInt, BigInt]()

  override def setup(): Unit = {
    for (i <- 0 to 1023) {
      Mem(i) = (Random.nextInt(Int.MaxValue): BigInt) * 2 + (if (Random.nextBoolean()) 1 else 0)
    }
    bus.valid #= false
  }

  override def send(pkg: ReqPkg): Unit = {
    if (pkg.wr) {
      Mem(pkg.addr >> 2) = pkg.data
    } else {
      val addr = pkg.addr >> 4 << 2
      bus.payload.data #= (Mem(addr + 3) << 96) + (Mem(addr + 2) << 64) + (Mem(addr + 1) << 32) + Mem(addr)
    }
    bus.valid #= true
    clk.waitSampling()
    bus.valid #= false
  }
}

case class Rm(qin: mutable.Queue[ReqPkg],
              clk: ClockDomain,
              mem: mutable.Map[BigInt, BigInt]) extends ReferenceModel[ReqPkg, RespPkg](qin, clk) {
  override def calculate(pkg: ReqPkg): RespPkg = {
    debug(f"[RM] got new package $pkg, mem: ${mem(pkg.addr >> 2)}%x")
    if (pkg.wr) RespPkg(0)
    else RespPkg(mem(pkg.addr >> 2))
  }
}

case class CacheScoreboard(q0: mutable.Queue[RespPkg], q1: mutable.Queue[RespPkg], clk: ClockDomain)
  extends ScoreBoard[RespPkg](q0, q1, clk) {
  override def compare(i: RespPkg, j: RespPkg): Boolean = i == j
}

case class RandomSequence(drv: CacheReqDriver) extends Sequence[ReqPkg](drv) {
  override def run(): Unit = {
    (0 to 100).foreach(_ -> drv.q.enqueue(ReqPkg(Random.nextBoolean, Random.nextInt(1023) >> 2 << 2, Random.nextInt.abs)))
    debug(s"[Random Sequence] wait done begin. driver queue length: ${drv.q.length}")
    drv.waitDone()
    debug("[Random Sequence] all package send done!")
  }
}

case class Env(dut: Cache, clk: ClockDomain) extends Environment(dut) {
  import cod.CacheTest.connect
  val cacheDriver = CacheReqDriver(dut.io.cacheReq, clk)
  val cacheReqMonitor = CacheReqMonitor(dut.io.cacheReq, clk)
  val cacheMonitor = CacheRespMonitor(dut.io.cacheResp, clk)
  val memMonitor = MemReqMonitor(dut.io.memReq, clk)
  val memDriver = MemRespDriver(dut.io.memResp, clk)
  val rm = Rm(cacheReqMonitor.q, clk, memDriver.Mem)
  val sb = CacheScoreboard(cacheMonitor.q, rm.q, clk)

  override def run(): Unit = {
    val rmThread = fork (rm.run())
    val sbThread = fork (sb.run())
    val cacheMonitorThread = fork (cacheMonitor.run())
    val cacheReqMonitorThread = fork (cacheReqMonitor.run())
    val memMonitorThread = fork (memMonitor.run())
    val memDriverThread = fork (memDriver.run())
    val connectThread = fork (connect(memMonitor.q, memDriver.q, clk))
    val cacheDriverThread = fork (cacheDriver.run())
  }

  override def report(): Unit = {
    sb.report()
    rm.report()
  }

  override def setup(): Unit = {}
}

case class CacheRandomTest(env: Env) extends TestBench(env) {
  override def runSeq(): Unit = RandomSequence(env.cacheDriver).run()
}

object CacheTest extends App {
  def connect[T](q0: mutable.Queue[T], q1: mutable.Queue[T], clk: ClockDomain) = {
    while (true) {
      if (q0.nonEmpty) q1.enqueue(q0.dequeue())
      clk.waitSampling()
    }
  }

  SimConfig
    .withWave
    .workspacePath("sim")
    .compile(new Cache)
    .doSim { dut =>
      dut.io.cacheResp.ready #= true
      val watchDogEnable = true
      val clk = dut.clockDomain
      clk.forkStimulus(5)
      clk.waitSampling(10)

      val watchDog = fork {
        clk.waitSampling(1000)
        if (watchDogEnable) simFailure("watch dog timeout!!!")
      }

      val env = Env(dut, clk)
      val tb = CacheRandomTest(env)
      tb.run()
    }
}
