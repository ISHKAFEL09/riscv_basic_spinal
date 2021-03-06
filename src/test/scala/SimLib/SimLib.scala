package SimLib

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable

trait Package

abstract class Driver[T <: Package](bus: HardType[Data], clk: ClockDomain) {
  val q = mutable.Queue[T]()

  def setup(): Unit = {}

  def send(pkg: T): Unit

  def run(): Unit = {
    setup()
    while (true) {
      if (q.nonEmpty) {
        send(q.dequeue())
      }
      clk.waitSampling()
    }
  }

  def waitDone(): Unit = {
    var finish = false
    while (!finish) {
      if (q.isEmpty) finish = true
      sleep(1)
    }
  }
}

abstract class Monitor[T <: Package](bus: HardType[Data], clk: ClockDomain) {
  val q = mutable.Queue[T]()

  def sample(): T
  def run(): Unit = {
    while (true) {
      clk.waitSampling()
      q.enqueue(sample())
    }
  }
}

abstract class Sequence[T <: Package](drv: Driver[T]) {
  def run(): Unit
}

abstract class ReferenceModel[T <: Package, S <: Package](qin: mutable.Queue[T],
                                                          clk: ClockDomain) {
  val q = mutable.Queue[S]()
  var cnt: Int = 0
  def calculate(pkg: T): S
  def run() = {
    while (true) {
      if (qin.nonEmpty) {
        q.enqueue(calculate(qin.dequeue()))
        cnt += 1
      }
      clk.waitSampling()
    }
  }
  def report() = {
    println(s"[RM] total ${cnt} packages got")
  }
}

abstract class ScoreBoard[T <: Package](queueDut: mutable.Queue[T], queueRm: mutable.Queue[T], clk: ClockDomain) {
  def compare(i: T, j: T): Boolean
  def run() = {
    while (true) {
      if (queueDut.nonEmpty && queueRm.nonEmpty) {
        val d0 = queueDut.dequeue()
        val d1 = queueRm.dequeue()
        if (!compare(d0, d1)) simFailure(s"Compare package fail!!! DUT: ${d0}, RM: ${d1}")
      }
      clk.waitSampling()
    }
  }
  def report() = {
    while (queueDut.nonEmpty && queueRm.nonEmpty) {}
    clk.waitSampling(100)
    if (queueDut.nonEmpty) simFailure("Package left in DUT queue")
    if (queueRm.nonEmpty) simFailure("Package left in RM queue")
  }
}

abstract class Environment(dut: Component) {
  def setup(): Unit
  def run(): Unit
  def report(): Unit
}

abstract class TestBench[T](env: Environment) {
  def runSeq(): Unit
  def run() = {
    env.setup()
    env.run()
    runSeq()
    env.report()
  }
}

