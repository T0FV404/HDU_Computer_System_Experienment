import chisel3._
import chisel3.util._

// ==========================================
// 配置参数类
// ==========================================
class CPUConfig {
  val XLEN = 32
  val ADDR_WIDTH = 32
  val REG_NUM = 32
  val IMEM_SIZE = 1024
  val DMEM_SIZE = 1024
  val PC_START = 0x0
}

// ==========================================
// HDU-XL-01 专用数码管驱动模块
// ==========================================
class Seg7LEDCtrl_HDU extends Module {
  val io = IO(new Bundle {
    val data   = Input(UInt(32.W))  // 要显示的 32 位数据
    val seg    = Output(UInt(8.W))  // 段选 (CA-CG, DP)
    val which  = Output(UInt(3.W))  // 位选 (3:8 译码器输入)
    val enable = Output(Bool())     // 译码器使能
  })

  // 扫描计数器：20MHz / 20000 = 1kHz 扫描频率
  val scanCounter = RegInit(0.U(15.W))
  val digitSelect = RegInit(0.U(3.W))

  scanCounter := scanCounter + 1.U
  when(scanCounter === 19999.U) {
    scanCounter := 0.U
    digitSelect := digitSelect + 1.U
  }

  // 译码器控制
  io.enable := true.B
  io.which  := digitSelect

  // 数据切片映射 (从左到右显示 High -> Low)
  // TB7(左) -> TB0(右) 对应 data[31:28] -> data[3:0]
  val shiftAmount = (7.U - digitSelect) << 2
  val currentNibble = (io.data >> shiftAmount)(3, 0)

  // 段选译码 (共阳极: 0亮1灭)
  io.seg := MuxLookup(currentNibble, "hFF".U)(Seq(
    0x0.U -> "h03".U, 0x1.U -> "h9F".U, 0x2.U -> "h25".U, 0x3.U -> "h0D".U,
    0x4.U -> "h99".U, 0x5.U -> "h49".U, 0x6.U -> "h41".U, 0x7.U -> "h1F".U,
    0x8.U -> "h01".U, 0x9.U -> "h09".U, 0xA.U -> "h11".U, 0xB.U -> "hC1".U,
    0xC.U -> "h63".U, 0xD.U -> "h85".U, 0xE.U -> "h61".U, 0xF.U -> "h71".U
  ))
}

// ==========================================
// 板级顶层 BoardTop
// ==========================================
class BoardTop(sim: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val sys_clk = Input(Clock())     // 20MHz 系统时钟 (H4)
    val rst_n   = Input(Bool())      // 复位按键 (低电平有效, R4)
    val sw      = Input(UInt(32.W))  // 32个拨码开关
    val swb     = Input(UInt(8.W))   // 8个按键 (仅使用 swb[7:0])
    
    val led     = Output(UInt(32.W)) // 32个LED
    val seg     = Output(UInt(8.W))  // 数码管段选
    val which   = Output(UInt(3.W))  // 数码管位选
    val enable  = Output(Bool())     // 数码管使能
  })

  // CPU 配置
  implicit val config = new CPUConfig

  // ----------------------------------------------------------
  // 1. 时钟分频逻辑 (产生慢速 CPU 时钟)
  // ----------------------------------------------------------
  // 使用 sys_clk 作为时钟源，不复位（避免时钟停止）
  val cpuClkReg = withClockAndReset(io.sys_clk, false.B) {
    // 仿真模式: 20MHz/4 = 5MHz, 上板模式: 20MHz/10000000 = 2Hz
    val countMax = if (sim) 4 else 10000000
    val counter = RegInit(0.U(32.W))
    val clkReg = RegInit(false.B)

    counter := counter + 1.U
    when(counter === (countMax - 1).U) {
      counter := 0.U
      clkReg := ~clkReg
    }
    clkReg
  }

  // ----------------------------------------------------------
  // 2. 实例化 CPU（使用慢时钟）
  // ----------------------------------------------------------
  val cpuReset = !io.rst_n  // 转换为高电平有效
  val cpu = withClockAndReset(cpuClkReg.asClock, cpuReset) {
    Module(new SingleCycleCPU)
  }

  // ----------------------------------------------------------
  // 3. 数据显示选择 (使用拨码开关 sw[2:0] 选择)
  // ----------------------------------------------------------
  val displayData = MuxLookup(io.sw(2, 0), cpu.io.debug.pc)(Seq(
    0.U -> cpu.io.debug.pc,       // 000: PC
    1.U -> cpu.io.debug.inst,     // 001: 指令
    2.U -> cpu.io.debug.aluOut,   // 010: ALU 结果
    3.U -> cpu.io.debug.memRData, // 011: 内存读数据
    4.U -> cpu.io.debug.memWData, // 100: 内存写数据
    5.U -> cpu.io.debug.memAddr,  // 101: 内存地址
    6.U -> Cat(Fill(31, 0.U), cpuClkReg),      // 110: 时钟心跳
    7.U -> Cat(Fill(31, 0.U), cpuReset.asUInt) // 111: 复位状态
  ))

  // ----------------------------------------------------------
  // 4. 数码管驱动（使用快时钟，不复位）
  // ----------------------------------------------------------
  val segDriver = withClockAndReset(io.sys_clk, false.B) {
    Module(new Seg7LEDCtrl_HDU)
  }
  
  segDriver.io.data := displayData
  io.seg    := segDriver.io.seg
  io.which  := segDriver.io.which
  io.enable := segDriver.io.enable

  // ----------------------------------------------------------
  // 5. LED 状态指示（功耗优化：仅点亮必要的 LED）
  // ----------------------------------------------------------
  // LED[0]: CPU 时钟心跳（闪烁说明 CPU 在工作）
  // LED[1]: 复位状态（复位时亮）
  // LED[2]: 内存写使能（写内存时亮）
  // LED[3]: CPU 时钟状态（调试用）
  // LED[31:4]: 关闭以降低功耗
  io.led := Cat(
    Fill(28, 0.U),              // LED[31:4] 关闭
    cpu.io.debug.memWen,        // LED[3]: 内存写指示
    cpuReset.asUInt,            // LED[2]: 复位状态
    io.swb(0),                  // LED[1]: 按键状态（调试）
    cpuClkReg                   // LED[0]: CPU 时钟心跳
  )
}

// ==========================================
// 板级顶层约束文件生成器
// ==========================================
object BoardTopConstraints {
  def generate(): String = {
    """
// ==========================================
// HDU-XL-01 板级约束文件
// ==========================================

// 开启比特流压缩，优化 .bit 文件大小
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]

// ==========================================
// 系统时钟 (20MHz)
// ==========================================
set_property -dict {PACKAGE_PIN H4 IOSTANDARD LVCMOS18} [get_ports sys_clk]
create_clock -period 50.000 -name sys_clk [get_ports sys_clk]

// ==========================================
// 复位按键 (低电平有效)
// ==========================================
set_property -dict {PACKAGE_PIN R4 IOSTANDARD LVCMOS18} [get_ports rst_n]

// ==========================================
// 32个拨码开关
// ==========================================
set_property IOSTANDARD LVCMOS18 [get_ports sw[*]]
set_property PULLDOWN true [get_ports sw[*]]

set_property PACKAGE_PIN T3 [get_ports {sw[31]}]
set_property PACKAGE_PIN U3 [get_ports {sw[30]}]
set_property PACKAGE_PIN T4 [get_ports {sw[29]}]
set_property PACKAGE_PIN V3 [get_ports {sw[28]}]
set_property PACKAGE_PIN V4 [get_ports {sw[27]}]
set_property PACKAGE_PIN W4 [get_ports {sw[26]}]
set_property PACKAGE_PIN Y4 [get_ports {sw[25]}]
set_property PACKAGE_PIN Y6 [get_ports {sw[24]}]
set_property PACKAGE_PIN W7 [get_ports {sw[23]}]
set_property PACKAGE_PIN Y8 [get_ports {sw[22]}]
set_property PACKAGE_PIN Y7 [get_ports {sw[21]}]
set_property PACKAGE_PIN T1 [get_ports {sw[20]}]
set_property PACKAGE_PIN U1 [get_ports {sw[19]}]
set_property PACKAGE_PIN U2 [get_ports {sw[18]}]
set_property PACKAGE_PIN W1 [get_ports {sw[17]}]
set_property PACKAGE_PIN W2 [get_ports {sw[16]}]
set_property PACKAGE_PIN Y1 [get_ports {sw[15]}]
set_property PACKAGE_PIN AA1 [get_ports {sw[14]}]
set_property PACKAGE_PIN V2 [get_ports {sw[13]}]
set_property PACKAGE_PIN Y2 [get_ports {sw[12]}]
set_property PACKAGE_PIN AB1 [get_ports {sw[11]}]
set_property PACKAGE_PIN AB2 [get_ports {sw[10]}]
set_property PACKAGE_PIN AB3 [get_ports {sw[9]}]
set_property PACKAGE_PIN AB5 [get_ports {sw[8]}]
set_property PACKAGE_PIN AA6 [get_ports {sw[7]}]
set_property PACKAGE_PIN R2 [get_ports {sw[6]}]
set_property PACKAGE_PIN R3 [get_ports {sw[5]}]
set_property PACKAGE_PIN T6 [get_ports {sw[4]}]
set_property PACKAGE_PIN R6 [get_ports {sw[3]}]
set_property PACKAGE_PIN U7 [get_ports {sw[2]}]
set_property PACKAGE_PIN AB7 [get_ports {sw[1]}]
set_property PACKAGE_PIN AB8 [get_ports {sw[0]}]

// ==========================================
// 8个按键
// ==========================================
set_property IOSTANDARD LVCMOS18 [get_ports swb[*]]

set_property PACKAGE_PIN R4 [get_ports {swb[0]}]
set_property PACKAGE_PIN AA4 [get_ports {swb[1]}]
set_property PACKAGE_PIN AB6 [get_ports {swb[2]}]
set_property PACKAGE_PIN T5 [get_ports {swb[3]}]
set_property PACKAGE_PIN V8 [get_ports {swb[4]}]
set_property PACKAGE_PIN AA8 [get_ports {swb[5]}]
set_property PACKAGE_PIN V9 [get_ports {swb[6]}]
set_property PACKAGE_PIN Y9 [get_ports {swb[7]}]

// ==========================================
// 32个LED显示灯
// ==========================================
set_property IOSTANDARD LVCMOS18 [get_ports led[*]]

set_property PACKAGE_PIN R1 [get_ports {led[31]}]
set_property PACKAGE_PIN P2 [get_ports {led[30]}]
set_property PACKAGE_PIN P1 [get_ports {led[29]}]
set_property PACKAGE_PIN N2 [get_ports {led[28]}]
set_property PACKAGE_PIN M1 [get_ports {led[27]}]
set_property PACKAGE_PIN M2 [get_ports {led[26]}]
set_property PACKAGE_PIN L1 [get_ports {led[25]}]
set_property PACKAGE_PIN J2 [get_ports {led[24]}]
set_property PACKAGE_PIN G1 [get_ports {led[23]}]
set_property PACKAGE_PIN E1 [get_ports {led[22]}]
set_property PACKAGE_PIN D2 [get_ports {led[21]}]
set_property PACKAGE_PIN A1 [get_ports {led[20]}]
set_property PACKAGE_PIN L3 [get_ports {led[19]}]
set_property PACKAGE_PIN G3 [get_ports {led[18]}]
set_property PACKAGE_PIN K4 [get_ports {led[17]}]
set_property PACKAGE_PIN G4 [get_ports {led[16]}]
set_property PACKAGE_PIN K1 [get_ports {led[15]}]
set_property PACKAGE_PIN J1 [get_ports {led[14]}]
set_property PACKAGE_PIN H2 [get_ports {led[13]}]
set_property PACKAGE_PIN G2 [get_ports {led[12]}]
set_property PACKAGE_PIN F1 [get_ports {led[11]}]
set_property PACKAGE_PIN E2 [get_ports {led[10]}]
set_property PACKAGE_PIN D1 [get_ports {led[9]}]
set_property PACKAGE_PIN B1 [get_ports {led[8]}]
set_property PACKAGE_PIN B2 [get_ports {led[7]}]
set_property PACKAGE_PIN N3 [get_ports {led[6]}]
set_property PACKAGE_PIN M3 [get_ports {led[5]}]
set_property PACKAGE_PIN K3 [get_ports {led[4]}]
set_property PACKAGE_PIN H3 [get_ports {led[3]}]
set_property PACKAGE_PIN N4 [get_ports {led[2]}]
set_property PACKAGE_PIN L4 [get_ports {led[1]}]
set_property PACKAGE_PIN J4 [get_ports {led[0]}]

// ==========================================
// 8位数码管
// ==========================================
set_property IOSTANDARD LVCMOS18 [get_ports seg[*]]
set_property IOSTANDARD LVCMOS18 [get_ports which[*]]
set_property IOSTANDARD LVCMOS18 [get_ports enable]

// 段选信号 (CA-CG, DP)
set_property PACKAGE_PIN H19 [get_ports {seg[7]}]
set_property PACKAGE_PIN G20 [get_ports {seg[6]}]
set_property PACKAGE_PIN J22 [get_ports {seg[5]}]
set_property PACKAGE_PIN K22 [get_ports {seg[4]}]
set_property PACKAGE_PIN K21 [get_ports {seg[3]}]
set_property PACKAGE_PIN H20 [get_ports {seg[2]}]
set_property PACKAGE_PIN H22 [get_ports {seg[1]}]
set_property PACKAGE_PIN J21 [get_ports {seg[0]}]

// 位选信号 (3:8译码器输入)
set_property PACKAGE_PIN N22 [get_ports {which[0]}]
set_property PACKAGE_PIN M21 [get_ports {which[1]}]
set_property PACKAGE_PIN M22 [get_ports {which[2]}]

// 译码器使能
set_property PACKAGE_PIN L21 [get_ports enable]

// ==========================================
// 时钟约束（允许非专用时钟路由）
// ==========================================
set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets sys_clk_IBUF]
"""
  }
}

// ==========================================
// 生成 Verilog
// ==========================================
object BoardGen extends App {
  // 生成上板版本（慢时钟）
  emitVerilog(new BoardTop(sim = false), Array("--target-dir", "generated_board"))
  
  // 生成约束文件
  import java.io._
  val writer = new PrintWriter(new File("generated_board/BoardTop.xdc"))
  writer.write(BoardTopConstraints.generate())
  writer.close()
  
  println("✅ 板级顶层模块和约束文件生成完成！")
  println("📁 输出目录: generated_board/")
  println("📄 Verilog: BoardTop.v")
  println("📄 约束文件: BoardTop.xdc")
}

object BoardSimGen extends App {
  // 生成仿真版本（快时钟）
  emitVerilog(new BoardTop(sim = true), Array("--target-dir", "generated"))
  println("✅ 仿真版本生成完成！")
  println("📁 输出目录: generated_sim/")
}