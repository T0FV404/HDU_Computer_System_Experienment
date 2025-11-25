import chisel3._
import chisel3.util._

// ==========================================
// 1. ALU 模块
// 对应教材 / PDF 表 7-11 中 ALU OP 的设置
// 实现基本算术逻辑运算：加、减、移位、比较、位运算等
// ==========================================

// ALU 操作码枚举（使用 object + 常量）
// 这里用 4 位宽度来编码不同的 ALU 操作
object ALUOps {
  // 注意：此处是“控制信号编码”，不是 RISC-V 指令的 opcode/funct3/funct7
  val ADD  = "b0000".U(4.W) // 加法
  val SLL  = "b0001".U(4.W) // 逻辑左移
  val SLT  = "b0010".U(4.W) // 有符号小于比较（Set Less Than）
  val SLTU = "b0011".U(4.W) // 无符号小于比较
  val XOR  = "b0100".U(4.W) // 异或
  val SRL  = "b0101".U(4.W) // 逻辑右移
  val OR   = "b0110".U(4.W) // 逻辑或
  val AND  = "b0111".U(4.W) // 逻辑与
  val SUB  = "b1000".U(4.W) // 减法
  val SRA  = "b1101".U(4.W) // 算术右移（保留符号位）
}

// ALU 模块定义
// 输入：两个 32 位操作数 a、b，4 位 ALU 控制信号 aluOp
// 输出：32 位结果 out，零标志 zf，溢出标志 of
class ALU extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(32.W))  // 操作数 A（无符号视角，但可通过 asSInt 看作有符号）
    val b      = Input(UInt(32.W))  // 操作数 B
    val aluOp  = Input(UInt(4.W))   // ALU 操作码（来自控制逻辑）
    val out    = Output(UInt(32.W)) // 运算结果
    val zf     = Output(Bool())     // Zero Flag：结果是否为 0
    val of     = Output(Bool())     // Overflow Flag：有符号加/减是否溢出
  })

  // 从 b 里取出低 5 位作为移位量（RISC-V RV32 中移位只看 5 位）
  val shamt = io.b(4, 0) // shift amount

  // 用一个中间 wire 保存 ALU 结果
  val res   = Wire(UInt(32.W))

  // 默认结果设为 0，避免综合出锁存器
  res := 0.U

  // 根据 aluOp 选择具体运算
  switch(io.aluOp) {
    is(ALUOps.ADD)  { res := io.a + io.b }
    is(ALUOps.SUB)  { res := io.a - io.b }
    is(ALUOps.SLL)  { res := io.a << shamt }                          // 逻辑左移
    is(ALUOps.SLT)  { res := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U) } // 有符号比较
    is(ALUOps.SLTU) { res := Mux(io.a < io.b, 1.U, 0.U) }              // 无符号比较
    is(ALUOps.XOR)  { res := io.a ^ io.b }
    is(ALUOps.OR)   { res := io.a | io.b }
    is(ALUOps.AND)  { res := io.a & io.b }
    is(ALUOps.SRL)  { res := io.a >> shamt }                           // 逻辑右移，高位补 0
    is(ALUOps.SRA)  { res := (io.a.asSInt >> shamt).asUInt }           // 算术右移，高位补符号位
  }

  // 将内部 wire 连接到模块输出
  io.out := res

  // Zero Flag：结果是否为 0
  io.zf  := (res === 0.U)

  // 溢出标志（仅对有符号加减法有效）
  // 有符号数采用二进制补码：
  //   Add 溢出：同号相加得异号
  //   Sub 溢出：异号相减得结果符号与被减数不同
  val signA = io.a(31)   // 操作数 A 的符号位
  val signB = io.b(31)   // 操作数 B 的符号位
  val signR = res(31)    // 结果的符号位

  // 默认无溢出
  io.of := false.B

  when (io.aluOp === ALUOps.ADD) {
    // A、B 同号且结果与 A 符号不同 -> 溢出
    io.of := (signA === signB) && (signR =/= signA)
  } .elsewhen (io.aluOp === ALUOps.SUB) {
    // A、B 异号且结果与 A 符号不同 -> 溢出
    io.of := (signA =/= signB) && (signR =/= signA)
  }
}

// ==========================================
// 2. 寄存器堆 (Register File)
// 对应图 7-25 中的寄存器堆：32 个 32 位通用寄存器
// 特性：
//   - x0 永远为 0（RISC-V 规定）
//   - 两个读端口（rs1, rs2）异步读
//   - 一个写端口（rd）同步写（时钟上升沿）
// ==========================================

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1   = Input(UInt(5.W))   // 读端口 1 地址（寄存器编号 0~31）
    val rs2   = Input(UInt(5.W))   // 读端口 2 地址
    val waddr = Input(UInt(5.W))   // 写端口地址
    val wdata = Input(UInt(32.W))  // 写数据
    val wen   = Input(Bool())      // 写使能
    val rdata1 = Output(UInt(32.W))// 读数据 1
    val rdata2 = Output(UInt(32.W))// 读数据 2
  })

  // 32 个 32 位寄存器，复位时全部清零
  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  // 同步写：在时钟上升沿更新寄存器内容
  // 同时禁止对 x0 写（waddr =/= 0）
  when (io.wen && io.waddr =/= 0.U) {
    regs(io.waddr) := io.wdata
  }

    // 异步读：直接通过地址索引寄存器数组
  // x0 必须始终为 0，所以当读地址为 0 时强制返回 0
  io.rdata1 := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.rdata2 := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))
}
// ==========================================
// 3. 异步指令存储器 (Instruction Memory)
// 说明：
//   - 用 Vec 模拟一个简单 ROM
//   - 异步读：地址变化时输出立即更新（组合逻辑）
//   - addr 为字地址（Word Address），与 PC[9:2] 相连
//   - 这里预存了一段示例程序（多条算术/逻辑指令）
// ==========================================

class AsyncInstMem extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(8.W))   // 字地址：连接 PC[9:2]，表示第几条指令
    val inst = Output(UInt(32.W)) // 输出对应地址的 32 位指令
  })

  // 使用 VecInit 创建一个常量 ROM
  // 注意：这里使用的是大端形式的 32 位十六进制指令编码（标准 RISC-V 格式）
  val initProg = Seq(
    // 0x00: lui x1, 0x12345
    // LUI: rd = 1, imm[31:12] = 0x12345
    "x123450b7".U(32.W),

    // 以下部分你已经根据教材/PDF 重构为标准 RISC-V 指令编码：
    "h123450b7".U, // 00: lui x1, 0x12345
    "h67808093".U, // 04: addi x1, x1, 0x678
    "h87654137".U, // 08: lui x2, 0x87654
    "h32110113".U, // 0C: addi x2, x2, 0x321
    "h002081b3".U, // 10: add x3, x1, x2  (纠正自 PDF: 00208163)
    "h40218233".U, // 14: sub x4, x3, x2
    "h111112b7".U, // 18: lui x5, 0x11111
    "h11128293".U, // 1C: addi x5, x5, 0x111
    "h0042f333".U, // 20: and x6, x5, x4
    "h0042e3b3".U, // 24: or  x7, x5, x4
    "h0042c433".U, // 28: xor x8, x5, x4
    "h0042b4b3".U, // 2C: sltu x9, x5, x4
    "h00629533".U, // 30: sll x10, x5, x6
    "hfff00593".U, // 34: addi x11, x0, -1
    "h0ff5f613".U, // 38: andi x12, x11, 0xff
    "h0ff5c693".U, // 3C: xori x13, x11, 0xff
    "h00359713".U, // 40: slli x14, x11, 3
    "h00500793".U, // 44: addi x15, x0, 5
    "h0ff7a813".U, // 48: slti x16, x15, 0xff
    "h00000897".U  // 4C: auipc x17, 0
                   // 后续地址全部填充为 NOP
  ) ++ Seq.fill(236)("h00000013".U) // nop: addi x0, x0, 0

  val mem = VecInit( initProg )
  // 异步读：直接用 addr 索引 Vec 输出
  // 注意：这里只是教学/仿真简化实现，真实硬件可能用 Block RAM/ROM 实现
  io.inst := mem(io.addr)
}

// ==========================================
// 4. 单周期 CPU 顶层模块 (Single-Cycle CPU)
// 对应图 7-25：
//   - 单周期：一条指令在一个时钟周期内完成 取指-译码-执行-写回
//   - 不包含数据访存（没有 load/store），只做运算类、LUI/AUIPC 等
//   - PC 每周期 +4，暂不含跳转/分支
// ==========================================

class SingleCycleCPU extends Module {
  val io = IO(new Bundle {
    // 对应表 7-12 的对外“观察”信号
    // 实际上，如果接到 FPGA，可把这几个信号连到 LED/数码管上调试
    val zf = Output(Bool())        // 当前 ALU 结果的 Zero Flag
    val of = Output(Bool())        // 当前 ALU 结果的 Overflow Flag
    val aluResult = Output(UInt(32.W)) // 当前 ALU 输出结果
    val currentInst = Output(UInt(32.W))// 当前执行的指令
    val currentPC   = Output(UInt(32.W))// 当前 PC（方便调试）
  })

  // 1. 取指令部件 (Fetch Unit)
  // -------------------------
  // PC 寄存器：存放当前指令地址
  val pc = RegInit(0.U(32.W))      // 复位时从 0 开始执行

  // 下一个 PC = 当前 PC + 4（顺序执行）
  val pcNext = pc + 4.U

  // 每个周期更新 PC
  pc := pcNext

  // 实例化指令存储器
  val imem = Module(new AsyncInstMem)

  // PC 是字节地址，PC[1:0] 固定为 00；PC[9:2] 是第几条 32bit 指令
  imem.io.addr := pc(9, 2) // 只使用部分高位地址访问 ROM

  // 从存储器取出的指令
  val inst = imem.io.inst

  // 输出给外部，方便仿真/调试
  io.currentInst := inst
  io.currentPC := pc

  // 2. 译码器 (Decoder) Logic
  val opcode = inst(6, 0)   // 指令低 7 位：opcode

  // // U-Type 立即数：bits[31:12] << 12，低 12 位为 0
  // 控制信号：根据 opcode 判断指令类型
  val isRType = (opcode === "b0110011".U) // R 型运算指令
  val isIType = (opcode === "b0010011".U) // I 型运算指令
  val isLUI   = (opcode === "b0110111".U) // LUI
  val isAUIPC = (opcode === "b0010111".U) // AUIPC

  // 这里的逻辑基于 RISC-V 的 funct3 + funct7 编码拆解为内部 ALUOps
  val aluOp = Wire(UInt(4.W))
  aluOp := ALUOps.ADD // 默认是加法（即使不被使用也给一个安全默认值）

// 3. 按类型分别处理
when (isRType) {

  val rd     = inst(11, 7)  // 目的寄存器
  val funct3 = inst(14, 12) // 中间 3 位：funct3
  val rs1    = inst(19, 15) // 源寄存器 1
  val rs2    = inst(24, 20) // 源寄存器 2
  val funct7 = inst(31, 25) // 高 7 位：funct7

  // R-type: funct3 + funct7 决定 ALU 操作
  switch (funct3) {
    is ("b000".U) {                // ADD / SUB
      when (funct7 === "b0100000".U) {
        aluOp := ALUOps.SUB        // SUB
      } .otherwise {
        aluOp := ALUOps.ADD        // ADD
      }
    }
    is ("b001".U) { aluOp := ALUOps.SLL  } // SLL
    // is ("b010".U) { aluOp := ALUOps.SLT  } // SLT
    is ("b011".U) { aluOp := ALUOps.SLTU } // SLTU
    is ("b100".U) { aluOp := ALUOps.XOR  } // XOR
    is ("b110".U) { aluOp := ALUOps.OR   } // OR
    is ("b111".U) { aluOp := ALUOps.AND  } // AND
  }
} .elsewhen (isIType) {
  val rd     = inst(11, 7)  // 目的寄存器
  val funct3 = inst(14, 12) // 中间 3 位：funct3
  val rs1    = inst(19, 15) // 源寄存器 1
  val immI := inst(31, 20).asSInt // 获取立即数

  switch (funct3) {
      is ("b000".U) { aluOp := ALUOps.addi }
      is ("b001".U) { aluOp := ALUOps.slli }
      is ("b010".U) { aluOp := ALUOps.slti }
    }
} .elsewhen (isLUI || isAUIPC) { // U类型
  val immU := (inst(31, 12) << 12).asSInt
  aluOp := ALUOps.ADD
} .otherwise {
  // 其它类型（以后扩展 load/store/branch/jump）在这里继续加分支
  // val immU := (inst(31, 12) << 12).asSInt
  aluOp := ALUOps.ADD
}

  // 3. 寄存器堆 & 操作数准备
  // -------------------------
  val regFile = Module(new RegisterFile)

  // 将指令中的 rs1、rs2 连接到寄存器堆读端口
  regFile.io.rs1 := rs1
  regFile.io.rs2 := rs2

  // TODO
  regFile.io.wen   := true.B      // 始终允许写（对于 rd=0 写不会生效）
  regFile.io.waddr := rd          // 写回目标寄存器号

  // 4. ALU 输入选择 (操作数选择多路复用器)
  // -------------------------
  val src1 = Wire(UInt(32.W))     // ALU 输入 A
  val src2 = Wire(UInt(32.W))     // ALU 输入 B

  // Src1 选择：
  //   AUIPC：使用 PC 作为 src1（PC + imm）
  //   LUI  ：使用 0 作为 src1（0 + imm）
  //   其它：使用寄存器 rs1 读出的数据
  // TODO
  when (isAUIPC) {
    src1 := pc
  } .elsewhen (isLUI) {
    src1 := 0.U
  } .otherwise {
    src1 := regFile.io.rdata1
  }

  // Src2 选择：
  //   R-Type：使用 rs2 的寄存器值
  //   LUI / AUIPC：使用 U-type 立即数
  //   I-Type：使用 I-type 立即数
  when (isRType) {
    src2 := regFile.io.rdata2
  } .elsewhen (isLUI || isAUIPC) {
    src2 := immU.asUInt
  } .otherwise { // 认为是 I-Type 算术类
    src2 := immI.asUInt
  }

  // 5. ALU 执行
  // -------------------------
  val alu = Module(new ALU)
  alu.io.a := src1
  alu.io.b := src2
  alu.io.aluOp := aluOp

  // 6. 写回阶段
  // -------------------------
  // 把 ALU 结果写回寄存器堆（regFile.io.wen 已经恒为 true）
  regFile.io.wdata := alu.io.out

  // 输出信号连到顶层 IO，方便仿真或板级调试
  io.aluResult := alu.io.out
  io.zf := alu.io.zf
  io.of := alu.io.of
}

// ==========================================
// 5. 生成 Verilog 的入口对象
// 使用：sbt "runMain CPUGen" 或 mill 方式构建
// 会在 generated/ 目录下生成 SingleCycleCPU.v
// ==========================================

object CPUGen extends App {
  emitVerilog(new SingleCycleCPU, Array("--target-dir", "generated"))
}
