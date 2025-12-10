import chisel3._
import chisel3.util._

// ==========================================
// 配置参数类
// ==========================================
class CPUConfig {
  val XLEN = 32           // 数据位宽
  val ADDR_WIDTH = 32     // 地址位宽
  val REG_NUM = 32        // 寄存器数量
  val IMEM_SIZE = 1024    // 指令存储器大小（字）
  val DMEM_SIZE = 1024    // 数据存储器大小（字）
  val PC_START = 0x0      // PC 起始地址
}

// ==========================================
// 1. ALU 操作码枚举
// ==========================================
object ALUOps extends ChiselEnum {
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Value
}

// ==========================================
// 2. 控制信号 Bundle（优化版）
// ==========================================
class ControlSignals extends Bundle {
  val aluOp   = ALUOps()    // ALU操作类型
  val regWen  = Bool()      // 寄存器写使能
  val memWen  = Bool()      // 内存写使能
  val aluSrc1 = UInt(2.W)   // ALU源1选择: 0=RS1, 1=PC, 2=0
  val aluSrc2 = Bool()      // ALU源2选择: 0=RS2, 1=Imm
  val wbSrc   = UInt(2.W)   // 写回源选择: 0=ALU, 1=Mem, 2=PC+4
  val pcSrc   = UInt(2.W)   // PC源选择: 0=+4, 1=Branch, 2=JAL, 3=JALR
}

// ==========================================
// 3. 控制器 (Control Unit)
// ==========================================
class ControlUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))
    val ctrl   = Output(new ControlSignals)
  })

  // 默认控制信号
  val defaultCtrl = Wire(new ControlSignals)
  defaultCtrl.aluOp   := ALUOps.ADD
  defaultCtrl.regWen  := false.B
  defaultCtrl.memWen  := false.B
  defaultCtrl.aluSrc1 := 0.U
  defaultCtrl.aluSrc2 := false.B
  defaultCtrl.wbSrc   := 0.U
  defaultCtrl.pcSrc   := 0.U

  io.ctrl := defaultCtrl

  // 指令类型识别
  val opcode = io.opcode
  val isRType  = opcode === "b0110011".U
  val isIType  = opcode === "b0010011".U
  val isLoad   = opcode === "b0000011".U
  val isStore  = opcode === "b0100011".U
  val isLUI    = opcode === "b0110111".U
  val isAUIPC  = opcode === "b0010111".U
  val isBranch = opcode === "b1100011".U
  val isJAL    = opcode === "b1101111".U
  val isJALR   = opcode === "b1100111".U

  // ========== R-Type / I-Type 算术逻辑指令 ==========
  when(isRType || isIType) {
    io.ctrl.regWen  := true.B
    io.ctrl.aluSrc1 := 0.U // RS1
    io.ctrl.aluSrc2 := isIType
    io.ctrl.wbSrc   := 0.U // ALU结果写回

    switch(io.funct3) {
      is("b000".U) {
        io.ctrl.aluOp := Mux(isRType && io.funct7 === "b0100000".U, 
                             ALUOps.SUB, ALUOps.ADD)
      }
      is("b001".U) { io.ctrl.aluOp := ALUOps.SLL  }
      is("b010".U) { io.ctrl.aluOp := ALUOps.SLT  }
      is("b011".U) { io.ctrl.aluOp := ALUOps.SLTU }
      is("b100".U) { io.ctrl.aluOp := ALUOps.XOR  }
      is("b101".U) {
        io.ctrl.aluOp := Mux(io.funct7 === "b0100000".U, 
                             ALUOps.SRA, ALUOps.SRL)
      }
      is("b110".U) { io.ctrl.aluOp := ALUOps.OR  }
      is("b111".U) { io.ctrl.aluOp := ALUOps.AND }
    }
  }

  // ========== Load 指令 ==========
  .elsewhen(isLoad) {
    io.ctrl.regWen  := true.B
    io.ctrl.aluSrc1 := 0.U // RS1
    io.ctrl.aluSrc2 := true.B // Imm
    io.ctrl.aluOp   := ALUOps.ADD
    io.ctrl.wbSrc   := 1.U // Mem数据写回
  }

  // ========== Store 指令 ==========
  .elsewhen(isStore) {
    io.ctrl.memWen  := true.B
    io.ctrl.aluSrc1 := 0.U // RS1
    io.ctrl.aluSrc2 := true.B // Imm
    io.ctrl.aluOp   := ALUOps.ADD
  }

  // ========== LUI 指令 ==========
  .elsewhen(isLUI) {
    io.ctrl.regWen  := true.B
    io.ctrl.aluSrc1 := 2.U // Nop
    io.ctrl.aluSrc2 := true.B // Imm
    io.ctrl.aluOp   := ALUOps.ADD
  }

  // ========== AUIPC 指令 ==========
  .elsewhen(isAUIPC) {
    io.ctrl.regWen  := true.B
    io.ctrl.aluSrc1 := 1.U
    io.ctrl.aluSrc2 := true.B
    io.ctrl.aluOp   := ALUOps.ADD
  }

  // ========== Branch 指令 ==========
  .elsewhen(isBranch) {
    io.ctrl.aluSrc1 := 0.U // RS1
    io.ctrl.aluSrc2 := false.B // RS2
    io.ctrl.pcSrc   := 1.U // Branch

    switch(io.funct3) {
      is("b000".U, "b001".U) { io.ctrl.aluOp := ALUOps.SUB  }
      is("b100".U, "b101".U) { io.ctrl.aluOp := ALUOps.SLT  }
      is("b110".U, "b111".U) { io.ctrl.aluOp := ALUOps.SLTU }
    }
  }

  // ========== JAL 指令 ==========
  .elsewhen(isJAL) {
    io.ctrl.regWen := true.B // 写回PC+4
    io.ctrl.wbSrc  := 2.U // PC+4
    io.ctrl.pcSrc  := 2.U // JAL目标地址
  }

  // ========== JALR 指令 ==========
  .elsewhen(isJALR) {
    io.ctrl.regWen  := true.B
    io.ctrl.aluSrc1 := 0.U
    io.ctrl.aluSrc2 := true.B
    io.ctrl.aluOp   := ALUOps.ADD
    io.ctrl.wbSrc   := 2.U
    io.ctrl.pcSrc   := 3.U
  }
}

// ==========================================
// 4. 立即数生成器 (ImmGen)
// ==========================================
class ImmGen(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(config.XLEN.W))
    val imm  = Output(UInt(config.XLEN.W))
  })

  val inst = io.inst
  val opcode = inst(6, 0)

  val immI = Cat(Fill(20, inst(31)), inst(31, 20)).asSInt
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7)).asSInt
  val immB = Cat(Fill(19, inst(31)), inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)).asSInt
  val immU = Cat(inst(31, 12), 0.U(12.W)).asSInt
  val immJ = Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)).asSInt

  io.imm := MuxLookup(opcode, immI.asUInt)(Seq(
    "b0100011".U -> immS.asUInt,
    "b1100011".U -> immB.asUInt,
    "b0110111".U -> immU.asUInt,
    "b0010111".U -> immU.asUInt,
    "b1101111".U -> immJ.asUInt
  ))
}

// ==========================================
// 5. ALU 模块
// ==========================================
class ALU(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(config.XLEN.W))
    val b      = Input(UInt(config.XLEN.W))
    val aluOp  = Input(ALUOps())
    val result = Output(UInt(config.XLEN.W))
    val zero   = Output(Bool())
  })

  val shamt = io.b(4, 0)
  
  val isSub = (io.aluOp === ALUOps.SUB) || 
              (io.aluOp === ALUOps.SLT) || 
              (io.aluOp === ALUOps.SLTU)
  val operandB = Mux(isSub, ~io.b, io.b)
  val sum = io.a + operandB + isSub.asUInt

  io.result := MuxLookup(io.aluOp.asUInt, 0.U)(Seq(
    ALUOps.ADD.asUInt  -> sum,
    ALUOps.SUB.asUInt  -> sum,
    ALUOps.SLL.asUInt  -> (io.a << shamt),
    ALUOps.SLT.asUInt  -> Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U),
    ALUOps.SLTU.asUInt -> Mux(io.a < io.b, 1.U, 0.U),
    ALUOps.XOR.asUInt  -> (io.a ^ io.b),
    ALUOps.SRL.asUInt  -> (io.a >> shamt),
    ALUOps.SRA.asUInt  -> (io.a.asSInt >> shamt).asUInt,
    ALUOps.OR.asUInt   -> (io.a | io.b),
    ALUOps.AND.asUInt  -> (io.a & io.b)
  ))

  io.zero := (io.result === 0.U)
}

// ==========================================
// 6. 分支判断单元
// ==========================================
class BranchUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val funct3  = Input(UInt(3.W))
    val aluZero = Input(Bool())
    val aluOut  = Input(UInt(config.XLEN.W))
    val taken   = Output(Bool())
  })

  io.taken := MuxLookup(io.funct3, false.B)(Seq(
    "b000".U -> io.aluZero,
    "b001".U -> !io.aluZero,
    "b100".U -> (io.aluOut === 1.U),
    "b101".U -> (io.aluOut === 0.U),
    "b110".U -> (io.aluOut === 1.U),
    "b111".U -> (io.aluOut === 0.U)
  ))
}

// ==========================================
// 7. 寄存器堆
// ==========================================
class RegisterFile(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val rs1Addr = Input(UInt(5.W))
    val rs2Addr = Input(UInt(5.W))
    val rdAddr  = Input(UInt(5.W))
    val rdData  = Input(UInt(config.XLEN.W))
    val rdWen   = Input(Bool())
    val rs1Data = Output(UInt(config.XLEN.W))
    val rs2Data = Output(UInt(config.XLEN.W))
  })

  val regs = RegInit(VecInit(Seq.fill(config.REG_NUM)(0.U(config.XLEN.W))))

  io.rs1Data := Mux(io.rs1Addr === 0.U, 0.U, regs(io.rs1Addr))
  io.rs2Data := Mux(io.rs2Addr === 0.U, 0.U, regs(io.rs2Addr))

  when(io.rdWen && io.rdAddr =/= 0.U) {
    regs(io.rdAddr) := io.rdData
  }
}

// ==========================================
// 8. 指令存储器
// ==========================================
class InstructionMemory(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(config.ADDR_WIDTH.W))
    val inst = Output(UInt(config.XLEN.W))
  })

  val program = Seq(
    "h00500113".U,  // 0x00: addi x2, x0, 5
    "h00500193".U,  // 0x04: addi x3, x0, 5
    "h00310663".U,  // 0x08: beq x2, x3, 12
    "h00f00513".U,  // 0x0C: addi x10, x0, 15
    "h0080006f".U,  // 0x10: jal x0, 8
    "h00100513".U,  // 0x14: addi x10, x0, 1
    "h00500113".U,  // 0x18: addi x2, x0, 5
    "h00600193".U,  // 0x1C: addi x3, x0, 6
    "h00311663".U,  // 0x20: bne x2, x3, 12
    "h00f00593".U,  // 0x24: addi x11, x0, 15
    "h0080006f".U,  // 0x28: jal x0, 8
    "h00100593".U,  // 0x2C: addi x11, x0, 1
    "h00c000ef".U,  // 0x30: jal x1, 12
    "h00100693".U,  // 0x34: addi x13, x0, 1
    "h00c0006f".U,  // 0x38: jal x0, 12
    "h00100613".U,  // 0x3C: addi x12, x0, 1
    "h00008067".U,  // 0x40: jalr x0, 0(x1)
    "h0000006f".U   // 0x44: jal x0, 0
  )

  val mem = RegInit(VecInit(program ++ Seq.fill(config.IMEM_SIZE - program.length)("h00000013".U)))
  
  val wordAddr = io.addr(31, 2)
  io.inst := mem(wordAddr)
}

// ==========================================
// 9. 数据存储器
// ==========================================
class DataMemory(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val addr  = Input(UInt(config.ADDR_WIDTH.W))
    val wdata = Input(UInt(config.XLEN.W))
    val wen   = Input(Bool())
    val rdata = Output(UInt(config.XLEN.W))
  })

  val mem = RegInit(VecInit(Seq.fill(config.DMEM_SIZE)(0.U(config.XLEN.W))))
  
  val wordAddr = io.addr(31, 2)
  
  io.rdata := mem(wordAddr)
  
  when(io.wen) {
    mem(wordAddr) := io.wdata
  }
}

// ==========================================
// 10. 数据通路 (Datapath)
// ==========================================
class Datapath(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val imemInst = Input(UInt(config.XLEN.W))
    val dmemData = Input(UInt(config.XLEN.W))
    val ctrl     = Input(new ControlSignals)
    
    val pc       = Output(UInt(config.ADDR_WIDTH.W))
    val aluOut   = Output(UInt(config.XLEN.W))
    val memWData = Output(UInt(config.XLEN.W))
  })

  // ========== 模块实例化 ==========
  val regFile = Module(new RegisterFile)
  val immGen  = Module(new ImmGen)
  val alu     = Module(new ALU)
  val branch  = Module(new BranchUnit)

  // ========== PC 寄存器 ==========
  val pcReg = RegInit(config.PC_START.U(config.ADDR_WIDTH.W))
  io.pc := pcReg

  // ========== 取指 ==========
  val inst = io.imemInst

  // ========== 译码 ==========
  immGen.io.inst := inst
  
  regFile.io.rs1Addr := inst(19, 15)
  regFile.io.rs2Addr := inst(24, 20)
  regFile.io.rdAddr  := inst(11, 7)
  regFile.io.rdWen   := io.ctrl.regWen

  // ========== 执行 ==========
  val aluSrc1 = MuxLookup(io.ctrl.aluSrc1, regFile.io.rs1Data)(Seq(
    0.U -> regFile.io.rs1Data,
    1.U -> pcReg,
    2.U -> 0.U
  ))

  val aluSrc2 = Mux(io.ctrl.aluSrc2, immGen.io.imm, regFile.io.rs2Data)

  alu.io.a     := aluSrc1
  alu.io.b     := aluSrc2
  alu.io.aluOp := io.ctrl.aluOp

  io.aluOut   := alu.io.result
  io.memWData := regFile.io.rs2Data

  // ========== 分支判断 ==========
  branch.io.funct3  := inst(14, 12)
  branch.io.aluZero := alu.io.zero
  branch.io.aluOut  := alu.io.result

  // ========== 写回 ==========
  val pcPlus4 = pcReg + 4.U
  
  val wbData = MuxLookup(io.ctrl.wbSrc, alu.io.result)(Seq(
    0.U -> alu.io.result,
    1.U -> io.dmemData,
    2.U -> pcPlus4
  ))
  
  regFile.io.rdData := wbData

  // ========== PC 更新 ==========
  val branchTarget = (pcReg.asSInt + immGen.io.imm.asSInt).asUInt
  val jalrTarget   = alu.io.result & "hfffffffe".U

  val nextPC = MuxLookup(io.ctrl.pcSrc, pcPlus4)(Seq(
    0.U -> pcPlus4,
    1.U -> Mux(branch.io.taken, branchTarget, pcPlus4),
    2.U -> branchTarget,
    3.U -> jalrTarget
  ))

  pcReg := nextPC
}

// ==========================================
// 11. 单周期 CPU 顶层
// ==========================================
class SingleCycleCPU(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val debug = new Bundle {
      val pc       = Output(UInt(config.ADDR_WIDTH.W))
      val inst     = Output(UInt(config.XLEN.W))
      val aluOut   = Output(UInt(config.XLEN.W))
      val memAddr  = Output(UInt(config.ADDR_WIDTH.W))
      val memWData = Output(UInt(config.XLEN.W))
      val memRData = Output(UInt(config.XLEN.W))
      val memWen   = Output(Bool())
    }
  })

  val control  = Module(new ControlUnit)
  val datapath = Module(new Datapath)
  val imem     = Module(new InstructionMemory)
  val dmem     = Module(new DataMemory)

  // ========== 取指 ==========
  imem.io.addr := datapath.io.pc
  val inst = imem.io.inst

  // ========== 控制器 ==========
  control.io.opcode := inst(6, 0)
  control.io.funct3 := inst(14, 12)
  control.io.funct7 := inst(31, 25)

  // ========== 数据通路 ==========
  datapath.io.imemInst := inst
  datapath.io.ctrl     := control.io.ctrl
  datapath.io.dmemData := dmem.io.rdata

  // ========== 数据存储器 ==========
  dmem.io.addr  := datapath.io.aluOut
  dmem.io.wdata := datapath.io.memWData
  dmem.io.wen   := control.io.ctrl.memWen

  // ========== 调试输出 ==========
  io.debug.pc       := datapath.io.pc
  io.debug.inst     := inst
  io.debug.aluOut   := datapath.io.aluOut
  io.debug.memAddr  := datapath.io.aluOut
  io.debug.memWData := datapath.io.memWData
  io.debug.memRData := dmem.io.rdata
  io.debug.memWen   := control.io.ctrl.memWen
}

// ==========================================
// 12. 顶层生成
// ==========================================
object CPUGen extends App {
  implicit val config = new CPUConfig
  
  emitVerilog(new SingleCycleCPU, Array("--target-dir", "generated"))
}