`timescale 1ns / 1ps

module tb_SingleCycleCPU();

    // 1. 信号声明
    reg clock;
    reg reset;
    wire io_zf;
    wire io_of;
    wire [31:0] io_aluResult;
    wire [31:0] io_currentInst;
    wire [31:0] io_currentPC;

    // 2. 实例化 CPU
    SingleCycleCPU uut (
        .clock(clock), 
        .reset(reset), 
        .io_zf(io_zf), 
        .io_of(io_of), 
        .io_aluResult(io_aluResult), 
        .io_currentInst(io_currentInst), 
        .io_currentPC(io_currentPC)
    );

    // 3. 生成时钟
    initial begin
        clock = 0;
        forever #5 clock = ~clock;
    end

    // =======================================================
    // 🌟 核心修改：为了打印出漂亮的表格
    // =======================================================
    
    integer cycle_count = 0;   // 记录第几个时钟周期
    string asm_code;           // 存储汇编指令字符串 (SystemVerilog特性)
    string check_res;          // 存储结论字符串

    // 4. 根据 PC 手动翻译汇编指令 (针对你作业中的 initProg)
    always @(*) begin
        case (io_currentPC)
            32'h00: asm_code = "lui x1, 0x12345";
            32'h04: asm_code = "addi x1, x1, 0x678";
            32'h08: asm_code = "lui x2, 0x87654";
            32'h0C: asm_code = "addi x2, x2, 0x321";
            32'h10: asm_code = "add x3, x1, x2";
            32'h14: asm_code = "sub x4, x3, x2";
            32'h18: asm_code = "lui x5, 0x11111";
            32'h1C: asm_code = "addi x5, x5, 0x111";
            32'h20: asm_code = "and x6, x5, x4";
            32'h24: asm_code = "or  x7, x5, x4";
            32'h28: asm_code = "xor x8, x5, x4";
            32'h2C: asm_code = "sltu x9, x5, x4";
            32'h30: asm_code = "sll x10, x5, x6";
            32'h34: asm_code = "addi x11, x0, -1";
            32'h38: asm_code = "andi x12, x11, 0xff";
            32'h3C: asm_code = "xori x13, x11, 0xff";
            32'h40: asm_code = "slli x14, x11, 3";
            default: asm_code = "Running / NOP ..."; 
        endcase
    end

    // 5. 打印控制逻辑
    initial begin
        // --- 初始化复位 ---
        reset = 1;
        #100;
        
        // --- 打印表头 ---
        $display("\n=========================================================================================================");
        // 使用 %-Ns 来左对齐字符串
        $display("%-6s | %-25s | %-12s | %-12s | %-3s | %-3s | %s", 
                 "Clock", "汇编指令 (参考)", "机器码(Inst)", "结果(Result)", "ZF", "OF", "结论");
        $display("-------+---------------------------+--------------+--------------+-----+-----+--------");

        reset = 0; // 释放复位
        #5000;     // 运行足够长的时间
        $stop;
    end

    // 6. 逐行打印数据
    always @(negedge clock) begin
        if (!reset) begin
            // 简单判断一下结论 (这里默认显示 Check，你可以自己加判断逻辑)
            check_res = "Check"; 

            $display("%-6d | %-25s | %h     | %h     |  %b  |  %b  | %s", 
                     cycle_count,    // Clock 计数
                     asm_code,       // 汇编字符串
                     io_currentInst, // 机器码
                     io_aluResult,   // ALU 结果
                     io_zf,          // Zero Flag
                     io_of,          // Overflow Flag
                     check_res       // 结论
            );
            cycle_count = cycle_count + 1;
        end
    end

endmodule