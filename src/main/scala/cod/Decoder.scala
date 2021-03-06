package cod

import spinal.core._
import spinal.lib._
import Const.Instruction._
import Const._
import lib._

class Decoder extends Component {
  val io = new Bundle {
    val valid, branchEval = in Bool()
    val instr = in UInt(32 bits)
    val decode = out(DecodeIO())
  }

  val decode = io.instr.mux (
    BUBBLE -> (InstrType.typeR, AluOpType.add, InstrFlag.nop),
    ADDI -> (InstrType.typeI, AluOpType.add, InstrFlag.nop),
    SLTI -> (InstrType.typeI, AluOpType.comp, InstrFlag.nop),
    SLTIU -> (InstrType.typeI, AluOpType.compu, InstrFlag.nop),
    ANDI -> (InstrType.typeI, AluOpType.and, InstrFlag.nop),
    ORI -> (InstrType.typeI, AluOpType.or, InstrFlag.nop),
    XORI -> (InstrType.typeI, AluOpType.xor, InstrFlag.nop),

    SLLI -> (InstrType.typeS, AluOpType.lshift, InstrFlag.isStore),
    SRLI -> (InstrType.typeS, AluOpType.rshift, InstrFlag.isStore),
    SRAI -> (InstrType.typeS, AluOpType.rshifta, InstrFlag.isStore),

    LUI -> (InstrType.typeU, AluOpType.bypass2, InstrFlag.nop),
    AUIPC -> (InstrType.typeU, AluOpType.bypass2, InstrFlag.nop),

    ADD -> (InstrType.typeR, AluOpType.add, InstrFlag.nop),
    SLT -> (InstrType.typeR, AluOpType.comp, InstrFlag.nop),
    SLTU -> (InstrType.typeR, AluOpType.compu, InstrFlag.nop),
    AND -> (InstrType.typeR, AluOpType.and, InstrFlag.nop),
    OR -> (InstrType.typeR, AluOpType.or, InstrFlag.nop),
    XOR -> (InstrType.typeR, AluOpType.xor, InstrFlag.nop),
    SLL -> (InstrType.typeR, AluOpType.lshift, InstrFlag.nop),
    SRL -> (InstrType.typeR, AluOpType.rshift, InstrFlag.nop),
    SUB -> (InstrType.typeR, AluOpType.sub, InstrFlag.nop),
    SRA -> (InstrType.typeR, AluOpType.rshifta, InstrFlag.nop),

    JAL -> (InstrType.typeJ, AluOpType.nop, InstrFlag.isJump),
    JALR -> (InstrType.typeI, AluOpType.nop, InstrFlag.isJump),

    BEQ -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),
    BNE -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),
    BLT -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),
    BLTU -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),
    BGE -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),
    BGEU -> (InstrType.typeB, AluOpType.nop, InstrFlag.isBranch),

    LW -> (InstrType.typeI, AluOpType.add, InstrFlag.isLoad),
    SW -> (InstrType.typeS, AluOpType.add, InstrFlag.isStore),

    ECALL -> (InstrType.typeI, AluOpType.nop, InstrFlag.nop),
    EBREAK -> (InstrType.typeI, AluOpType.nop, InstrFlag.nop),

    CSRRW -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    CSRRS -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    CSRRC -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    CSRRWI -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    CSRRSI -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    CSRRCI -> (InstrType.typeI, AluOpType.bypass2, InstrFlag.isCsr),
    default -> (InstrType.typeN, AluOpType.add, InstrFlag.isBranch)
  )

  val (instrType, aluOpType, instrFlag) = decode
  when (io.valid) { assert(instrType =/= InstrType.typeN, "Unknown instr")}

  def hasFlag(flag: UInt): Bool = { (instrFlag & flag).orR }

  io.decode.pcSrc := MuxCase(PCSel.plus4, Seq(
    (io.branchEval && hasFlag(InstrFlag.isBranch)) -> PCSel.jump
  ))
  
  io.decode.aluOp := aluOpType

  io.decode.brType := MuxCase(BranchSel.nop, Seq(
    hasFlag(InstrFlag.isJump) -> BranchSel.jump,
    (io.instr === BEQ) -> BranchSel.beq,
    (io.instr === BNE) -> BranchSel.bne,
    (io.instr === BLT) -> BranchSel.blt,
    (io.instr === BLTU) -> BranchSel.bltu,
    (io.instr === BGE) -> BranchSel.bge,
    (io.instr === BGEU) -> BranchSel.bgeu
  ))

  io.decode.memRen := hasFlag(InstrFlag.isLoad)
  io.decode.memWen := hasFlag(InstrFlag.isStore)

  io.decode.rfWen := !(instrType === InstrType.typeB || instrType === InstrType.typeS)

  io.decode.aluSrc1 := AluSrc.rf
  io.decode.aluSrc2 := MuxCase(AluSrc.rf, Seq(
    (instrType === InstrType.typeI) -> AluSrc.imm,
    (instrType === InstrType.typeU) -> AluSrc.imm,
    (instrType === InstrType.typeJ) -> AluSrc.imm,
    hasFlag(InstrFlag.isJump) -> AluSrc.pc,
    hasFlag(InstrFlag.isCsr) -> AluSrc.csr
  ))

  io.decode.wbSrc := Mux(io.decode.memRen, WbSrc.mem, WbSrc.alu)

  io.decode.isCsr := hasFlag(InstrFlag.isCsr)

  io.decode.immType := MuxCase(ImmType.typeN, Seq(
    (io.instr === AUIPC) -> ImmType.addPc,
    (instrType === InstrType.typeI) -> ImmType.typeI,
    (instrType === InstrType.typeS) -> ImmType.typeS,
    (instrType === InstrType.typeB) -> ImmType.typeB,
    (instrType === InstrType.typeU) -> ImmType.typeU,
    (instrType === InstrType.typeJ) -> ImmType.typeJ
  ))
}
