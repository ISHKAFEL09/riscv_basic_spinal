package cod

import spinal.core._
import spinal.lib._

case class CacheRequest() extends Bundle {
  val wr = Bool
  val addr = UInt(32 bits)
  val data = UInt(32 bits)
}

case class CacheResponse() extends Bundle {
  val data = UInt(32 bits)
}

case class MemRequest() extends Bundle {
  val wr = Bool
  val addr = UInt(32 bits)
  val data = UInt(128 bits)
}

case class MemResponse() extends Bundle {
  val data = UInt(128 bits)
}

case class TagFormat() extends Bundle {
  val tag = UInt(18 bits)
  val dirty = Bool
  val valid = Bool
}

case class AddrFormat() extends Bundle {
  val byte = UInt(2 bits)
  val word = UInt(2 bits)
  val line = UInt(10 bits)
  val tag = UInt(18 bits)
}

case class CacheWrite() extends Bundle {
  val valid = Bool
  val sel = Bool // 1: line, 0: word
  val addr = UInt(32 bits)
  val word = UInt(32 bits)
  val line = Vec(UInt(32 bits), 4)
}

case class Cache() extends Component {
  val io = new Bundle() {
    val cacheReq = slave Stream CacheRequest()
    val cacheResp = master Stream CacheResponse()
    val memReq = master Flow MemRequest()
    val memResp = slave Flow MemResponse()
  }

  val valid = RegInit(False)
  val tags = Mem(UInt(20 bits), 1024) init {
    Seq.fill(1024)(U(0, 20 bits))
  }
  val datas = Mem(Vec(UInt(32 bits), 4), 1024) init {
    Seq.fill(1024)(Vec(U(0, 32 bits), 4))
  }

  val cacheRequestReg = RegNextWhen(io.cacheReq.payload, io.cacheReq.fire)
  io.cacheReq.ready := !valid
  valid.setWhen(io.cacheReq.fire)

  val requestAddr = io.cacheReq.payload.addr.as(AddrFormat())
  val requestAddrReg = cacheRequestReg.addr.as(AddrFormat())
  val tag = tags.readSync(requestAddr.line, io.cacheReq.fire).as(TagFormat())
  val data = datas.readSync(requestAddr.line, io.cacheReq.fire)

  val hit = tag.valid && (tag.tag === requestAddrReg.tag)

  val writeMem = Bool()
//  val readMem = Bool()
  val writeMemAddr = Cat(tag.tag, requestAddrReg.line, U"4'h0")
  val readMemAddr = Cat(requestAddrReg.tag, requestAddrReg.line, U"4'h0")
  io.memReq.payload.wr := writeMem
  io.memReq.payload.addr := Mux(writeMem, writeMemAddr, readMemAddr).asUInt
  io.memReq.payload.data := data.asBits.asUInt

  val cacheWrite = CacheWrite()
  when (cacheWrite.valid) {
    val addr = cacheWrite.addr.as(AddrFormat())
    val line = tags(addr.line).as(TagFormat())
    line.tag := cacheRequestReg.addr.as(AddrFormat()).tag
    line.valid.set()
    line.dirty.set()
    when (cacheWrite.sel) {
      datas(addr.line) := cacheWrite.line
    }.otherwise {
      datas(addr.line)(addr.word) := cacheWrite.word
    }
  }

  cacheWrite.line := io.memResp.payload.data.as(Vec(UInt(32 bits), 4))
  when (cacheRequestReg.wr) {
    cacheWrite.line(requestAddrReg.word) := cacheRequestReg.data
  }
  cacheWrite.word := cacheRequestReg.data
  cacheWrite.addr := cacheRequestReg.addr

  object StateEnum extends SpinalEnum {
    val compareTag, memWriteReq, memWriteResp, memReadReq, memReadResp, finish = newElement()
  }

  import StateEnum._
  val state = RegInit(compareTag)
  val jump2finish = False
  switch (state) {
    is (compareTag) {
      when (valid) {
        when (hit) {
          state := finish
          valid := False
          jump2finish.set()
        }.elsewhen(tag.valid && tag.dirty) { state := memWriteReq }
          .elsewhen(cacheRequestReg.wr === False) {state := memReadReq}
          .otherwise {
            state := finish
            jump2finish.set()
          }
      }
    }
    is (memWriteReq) { state := memWriteResp }
    is (memWriteResp) { when (io.memResp.fire) {state := memReadReq}}
    is (memReadReq) {state := memReadResp}
    is (memReadResp) {
      when (io.memResp.fire) {
        state := finish
      }
    }
    is (finish) {when (io.cacheResp.fire) {
      state := compareTag
      valid.clear()
    }}
  }

  io.memReq.valid := state === memWriteReq || state === memReadReq
  cacheWrite.valid := state === memReadResp || (cacheRequestReg.wr && jump2finish)
  cacheWrite.sel := state === memReadResp
  io.cacheResp.valid := state === finish
  io.cacheResp.payload.data := RegNext(Mux(state === memReadResp,
    io.memResp.payload.data.as(Vec(UInt(32 bits), 4))(requestAddrReg.word),
    U(0, 32 bits)))
//    data(requestAddrReg.word)))
  writeMem := state === memWriteReq || state === memWriteResp
//  readMem := state === memReadReq || state === memReadResp
}

object Cache extends App {
  SpinalConfig (
    mode = Verilog,
    targetDirectory = "generated"
  ).generate(Cache()).printPruned()
}
