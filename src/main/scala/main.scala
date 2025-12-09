import chisel3._
import chisel3.util._

// ==========================================
// 1. ALU 操作码枚举
// ==========================================
object ALUOps extends ChiselEnum {
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Value
  val ERROR = Value
}

// ==========================================
// 2. 控制器 (Control Unit)
// ==========================================
class ControlUnit extends Module {
  val io = IO(new Bundle {
    val opcode  = Input(UInt(7.W))
    val funct3  = Input(UInt(3.W))
    val funct7  = Input(UInt(7.W))

    // 控制信号输出
    val aluOp   = Output(ALUOps())
    val regWen  = Output(Bool())
    val memWen  = Output(Bool())
    val src1Sel = Output(UInt(2.W)) // 0: RS1, 1: PC, 2: 0 (for LUI)
    val src2Sel = Output(UInt(1.W)) // 0: RS2, 1: Imm
    val wbSel   = Output(UInt(1.W)) // 0: ALU, 1: Mem
    val isSigned = Output(Bool())

    // 转移指令专用信号
    val isBranch      = Output(Bool()) // 是否为分支指令 (B-Type)
    val isBeq         = Output(Bool()) // 细分：是否为 BEQ
    val isJal         = Output(Bool()) // 是否为 JAL
    val isJalr        = Output(Bool()) // 是否为 JALR
    val isLink        = Output(Bool()) // 是否需要保存返回地址 (PC+4)
  })

  // 默认值
  io.aluOp   := ALUOps.ADD
  io.regWen  := false.B
  io.memWen  := false.B
  io.src1Sel := 0.U
  io.src2Sel := 0.U
  io.wbSel   := 0.U
  io.isSigned := false.B

  io.isBranch     := false.B
  io.isBeq        := false.B
  io.isJal        := false.B
  io.isJalr       := false.B
  io.isLink       := false.B

  // 指令解码
  val isRType  = io.opcode === "b0110011".U
  val isIType  = io.opcode === "b0010011".U
  val isLoad   = io.opcode === "b0000011".U
  val isStore  = io.opcode === "b0100011".U
  val isLUI    = io.opcode === "b0110111".U
  val isAUIPC  = io.opcode === "b0010111".U
  val isBranch = io.opcode === "b1100011".U
  val isJal    = io.opcode === "b1101111".U
  val isJalr   = io.opcode === "b1100111".U

  // 细分信号
  io.isBranch := isBranch
  io.isJal    := isJal
  io.isJalr   := isJalr
  io.isLink   := isJal || isJalr // JAL 和 JALR 都需要写回 PC+4
  io.isBeq    := isBranch && (io.funct3 === "b000".U) // funct3=000 为 BEQ

  // ALU 控制逻辑
  when(isRType || isIType) {
    switch(io.funct3) {
      is("b000".U) { // ADD or SUB
        when(isRType && io.funct7 === "b0100000".U) { io.aluOp := ALUOps.SUB }
          .otherwise { io.aluOp := ALUOps.ADD }
      }
      is("b001".U) { io.aluOp := ALUOps.SLL }
      is("b010".U) { io.aluOp := ALUOps.SLT }
      is("b011".U) { io.aluOp := ALUOps.SLTU }
      is("b100".U) { io.aluOp := ALUOps.XOR }
      is("b101".U) { // SRL or SRA
        when(io.funct7 === "b0100000".U) { io.aluOp := ALUOps.SRA }
          .otherwise { io.aluOp := ALUOps.SRL }
      }
      is("b110".U) { io.aluOp := ALUOps.OR }
      is("b111".U) { io.aluOp := ALUOps.AND }
    }
  }
  io.isSigned := (io.aluOp === ALUOps.ADD) || (io.aluOp === ALUOps.SUB) ||
                 (io.aluOp === ALUOps.SLT) || (io.aluOp === ALUOps.SRA)

  // 数据通路控制信号
  when(isRType) {
    io.regWen := true.B; io.src1Sel := 0.U; io.src2Sel := 0.U; io.wbSel := 0.U
  } .elsewhen(isIType) {
    io.regWen := true.B; io.src1Sel := 0.U; io.src2Sel := 1.U; io.wbSel := 0.U
  } .elsewhen(isLoad) {
    io.regWen := true.B; io.src1Sel := 0.U; io.src2Sel := 1.U; io.wbSel := 1.U
    io.aluOp := ALUOps.ADD
  } .elsewhen(isStore) {
    io.memWen := true.B; io.src1Sel := 0.U; io.src2Sel := 1.U; io.aluOp := ALUOps.ADD
  } .elsewhen(isLUI) {
    io.regWen := true.B; io.src1Sel := 2.U; io.src2Sel := 1.U; io.aluOp := ALUOps.ADD
  } .elsewhen(isAUIPC) {
    io.regWen := true.B; io.src1Sel := 1.U; io.src2Sel := 1.U; io.aluOp := ALUOps.ADD
  } .elsewhen(isBranch) {
    // Branch: ALU 做减法比较，不写回
    io.regWen := false.B; io.src1Sel := 0.U; io.src2Sel := 0.U; io.aluOp := ALUOps.SUB
  } .elsewhen(isJal) {
    // JAL: 需要写回 PC+4, ALU 计算跳转目标(PC+Imm)用于调试或辅助
    io.regWen := true.B; io.src1Sel := 1.U; io.src2Sel := 1.U; io.aluOp := ALUOps.ADD
  } .elsewhen(isJalr) {
    // JALR: 需要写回 PC+4, ALU 计算跳转目标(RS1+Imm)
    io.regWen := true.B; io.src1Sel := 0.U; io.src2Sel := 1.U; io.aluOp := ALUOps.ADD
  }
}

// ==========================================
// 3. 立即数生成器 (ImmGen)
// ==========================================
class ImmGen extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val out  = Output(UInt(32.W))
  })
  val inst = io.inst
  val opcode = inst(6, 0)

  val immI = Cat(Fill(20, inst(31)), inst(31, 20)).asSInt
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7)).asSInt
  val immU = Cat(inst(31, 12), 0.U(12.W)).asSInt
  val immB = Cat(Fill(19, inst(31)), inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)).asSInt
  val immJ = Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)).asSInt

  io.out := MuxCase(0.S, Seq(
    (opcode === "b0100011".U) -> immS, // Store
    ((opcode === "b0110111".U) || (opcode === "b0010111".U)) -> immU, // LUI/AUIPC
    (opcode === "b1100011".U) -> immB, // Branch
    (opcode === "b1101111".U) -> immJ, // JAL
    (true.B)                  -> immI  // Default I-Type
  )).asUInt
}

// ==========================================
// 4. ALU 模块
// ==========================================
class ALU extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(32.W))
    val b      = Input(UInt(32.W))
    val aluOp  = Input(ALUOps())
    val out    = Output(UInt(32.W))
    val zf     = Output(Bool())
    val of     = Output(Bool())
  })
  val shamt = io.b(4, 0)
  val isSub = (io.aluOp === ALUOps.SUB) || (io.aluOp === ALUOps.SLT) || (io.aluOp === ALUOps.SLTU)
  val adderB = Mux(isSub, (~io.b).asUInt, io.b)
  val sum    = io.a + adderB + isSub.asUInt

  io.out := 0.U
  switch(io.aluOp) {
    is(ALUOps.ADD)  { io.out := sum }
    is(ALUOps.SUB)  { io.out := sum }
    is(ALUOps.SLL)  { io.out := io.a << shamt }
    is(ALUOps.SLT)  { io.out := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U) }
    is(ALUOps.SLTU) { io.out := Mux(io.a < io.b, 1.U, 0.U) }
    is(ALUOps.XOR)  { io.out := io.a ^ io.b }
    is(ALUOps.OR)   { io.out := io.a | io.b }
    is(ALUOps.AND)  { io.out := io.a & io.b }
    is(ALUOps.SRL)  { io.out := io.a >> shamt }
    is(ALUOps.SRA)  { io.out := (io.a.asSInt >> shamt).asUInt }
  }
  io.zf := (io.out === 0.U)
  val signA = io.a(31); val signB = adderB(31); val signR = sum(31)
  io.of := (io.aluOp === ALUOps.ADD || io.aluOp === ALUOps.SUB) && (signA === signB) && (signR =/= signA)
}

// ==========================================
// 5. 寄存器堆
// ==========================================
class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1    = Input(UInt(5.W)); val rs2 = Input(UInt(5.W))
    val waddr  = Input(UInt(5.W)); val wdata = Input(UInt(32.W))
    val wen    = Input(Bool())
    val rdata1 = Output(UInt(32.W)); val rdata2 = Output(UInt(32.W))
  })
  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  io.rdata1 := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.rdata2 := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))
  when(io.wen && io.waddr =/= 0.U) { regs(io.waddr) := io.wdata }
}

// ==========================================
// 6. 异步指令存储器 (Instruction Memory)
//    *** 关键修改：替换为实验六手册提供的机器码 ***
// ==========================================
class AsyncInstMem extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(8.W))
    val inst = Output(UInt(32.W))
  })

  // 这里的指令序列来自实验手册 Source 910 和 Source 205
  // 包含 beq, bne, jal, jalr 测试
  val initProg = Seq(
    "h00500113".U, // 00: addi x2, x0, 5
    "h00500193".U, // 04: addi x3, x0, 5 (手册中注释写x3=6, 但机器码00500193确实是addi x3,x0,5。若x3=5则beq成立)
    "h00310663".U, // 08: beq x2, x3, +12 (跳转到 beq_pass)
    "h00f00513".U, // 0C: addi x10, x0, 15 (Fail标记)
    "h0080006f".U, // 10: j beq_end
    "h00100513".U, // 14: beq_pass: addi x10, x0, 1 (Success标记)
    "h00500113".U, // 18: beq_end: addi x2, x0, 5
    "h00600193".U, // 1C: addi x3, x0, 6
    "h00311663".U, // 20: bne x2, x3, +12 (跳转到 bne_pass)
    "h00f00593".U, // 24: addi x11, x0, 15 (Fail标记)
    "h0080006f".U, // 28: j bne_end
    "h00100593".U, // 2C: bne_pass: addi x11, x0, 1 (Success标记)
    "h00c000ef".U, // 30: bne_end: jal x1, func
    "h00100693".U, // 34: addi x13, x0, 1 (返回后执行)
    "h00c0006f".U, // 38: j end
    "h00100613".U, // 3C: func: addi x12, x0, 1
    "h00008067".U, // 40: jalr x0, 0(x1) (返回)
    "h0000006f".U  // 44: end: j end (死循环)
  ) ++ Seq.fill(240)("h00000013".U) // 填充 NOP

  val mem = VecInit(initProg)
  io.inst := mem(io.addr)
}

// ==========================================
// 7. 异步数据存储器 (Data Memory)
// ==========================================
class AsyncDataMem extends Module {
  val io = IO(new Bundle {
    val addr  = Input(UInt(32.W)); val wdata = Input(UInt(32.W))
    val wen   = Input(Bool())
    val rdata = Output(UInt(32.W))
  })
  val mem = RegInit(VecInit(Seq.fill(128)(0.U(32.W))))
  val wordAddr = io.addr(8, 2)
  io.rdata := mem(wordAddr)
  when(io.wen) { mem(wordAddr) := io.wdata }
}

// ==========================================
// 8. 单周期 CPU 顶层
// ==========================================
class SingleCycleCPU extends Module {
  val io = IO(new Bundle {
    val zf = Output(Bool()); val of = Output(Bool())
    val aluResult = Output(UInt(32.W)); val currentInst = Output(UInt(32.W))
    val currentPC = Output(UInt(32.W)); val isSignedOp = Output(Bool())
    val dmemWen = Output(Bool()); val dmemAddr = Output(UInt(32.W))
    val dmemWData = Output(UInt(32.W)); val dmemRData = Output(UInt(32.W))
  })

  val pcReg = RegInit(0.U(32.W))
  val control = Module(new ControlUnit)
  val regFile = Module(new RegisterFile)
  val immGen  = Module(new ImmGen)
  val alu     = Module(new ALU)
  val imem    = Module(new AsyncInstMem)
  val dmem    = Module(new AsyncDataMem)

  // 取指
  io.currentPC := pcReg
  imem.io.addr := pcReg(9, 2)
  val inst = imem.io.inst
  io.currentInst := inst

  // 连接部件
  control.io.opcode := inst(6, 0); control.io.funct3 := inst(14, 12); control.io.funct7 := inst(31, 25)
  immGen.io.inst := inst
  regFile.io.rs1 := inst(19, 15); regFile.io.rs2 := inst(24, 20)
  regFile.io.waddr := inst(11, 7); regFile.io.wen := control.io.regWen

  // ALU 输入选择
  val src1 = MuxLookup(control.io.src1Sel, 0.U)(Seq(0.U -> regFile.io.rdata1, 1.U -> pcReg, 2.U -> 0.U))
  val src2 = Mux(control.io.src2Sel === 1.U, immGen.io.out, regFile.io.rdata2)
  alu.io.a := src1; alu.io.b := src2; alu.io.aluOp := control.io.aluOp

  // 访存
  dmem.io.addr := alu.io.out; dmem.io.wdata := regFile.io.rdata2; dmem.io.wen := control.io.memWen

  // 写回寄存器堆: 增加 isLink 判断，若为 JAL/JALR 则写回 PC+4 [cite: 728, 835]
  val pcPlus4 = pcReg + 4.U
  val wbData = Mux(control.io.isLink, pcPlus4, Mux(control.io.wbSel === 1.U, dmem.io.rdata, alu.io.out))
  regFile.io.wdata := wbData

  // ---------------------------------------------
  // PC 更新逻辑 (Experiment 6 核心)
  // ---------------------------------------------
  
  // 1. 计算跳转目标地址
  // JAL 和 Branch 的目标是 PC + Imm [cite: 721]
  val branchTarget = (pcReg.asSInt + immGen.io.out.asSInt).asUInt
  
  // JALR 的目标是 (RS1 + Imm) & ~1 [cite: 721]
  // ControlUnit 中 JALR 会让 ALU 计算 RS1 + Imm，所以直接取 alu.io.out 并清零最低位
  val jalrTarget = alu.io.out & "hfffffffe".U

  // 2. 判断是否满足跳转条件 (Taken)
  // beq: ZF=1 跳转; bne: ZF=0 跳转 
  val isTaken = control.io.isBranch && Mux(control.io.isBeq, alu.io.zf, !alu.io.zf)

  // 3. 多路选择生成 Next PC
  // 优先级: JALR > (JAL | BranchTaken) > PC+4
  val nextPC = Mux(control.io.isJalr,
    jalrTarget,
    Mux(control.io.isJal || isTaken, branchTarget, pcPlus4)
  )

  pcReg := nextPC

  // 调试信号输出
  io.aluResult := alu.io.out; io.zf := alu.io.zf; io.of := alu.io.of; io.isSignedOp := control.io.isSigned
  io.dmemWen := control.io.memWen; io.dmemAddr := alu.io.out; io.dmemWData := regFile.io.rdata2; io.dmemRData := dmem.io.rdata
}

// 生成 Verilog
object CPUGen extends App {
  emitVerilog(new SingleCycleCPU, Array("--target-dir", "generated"))
}