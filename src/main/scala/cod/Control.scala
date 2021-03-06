package cod

import spinal.core._

case class DecodeIO() extends Bundle {
  val pcSrc = UInt(ctrlSize)
  val aluSrc1 = UInt(ctrlSize)
  val aluSrc2 = UInt(ctrlSize)
  val aluOp = UInt(ctrlSize)
  val rfWen = Bool()
  val memWen = Bool()
  val memRen = Bool()
  val wbSrc = UInt(ctrlSize)
  val brType = UInt(ctrlSize)
  val isCsr = Bool()
  val immType = UInt(ctrlSize)
}

class Control {

}
