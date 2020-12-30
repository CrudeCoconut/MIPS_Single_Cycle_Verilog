// single-cycle MIPS processor
// instantiates a controller and a datapath module

module mips(input          clk, reset,
            output  [31:0] pc,
            input   [31:0] instr,
            output         memwrite,
            output  [31:0] aluout, writedata,
            input   [31:0] readdata);

  wire        memtoreg, branch,
               pcsrc, zero,
               alusrc, regdst, regwrite, jump;
  wire [2:0]  alucontrol;

  wire zeroExtend; //changes: added this signal to choose zeroExtended Immediate. only needed for logical i type

  //changes: add one more port for zeroExtend signal to both controller and datapath
  controller c(instr[31:26], instr[5:0], zero,
               memtoreg, memwrite, pcsrc,
               alusrc, regdst, regwrite, jump,
               alucontrol, zeroExtend);
  datapath dp(clk, reset, memtoreg, pcsrc,
              alusrc, regdst, regwrite, jump,
              alucontrol,
              zero, pc, instr,
              aluout, writedata, readdata, zeroExtend);
endmodule


// Todo: Implement controller module
module controller(input   [5:0] op, funct,
                  input         zero,
                  output        memtoreg, memwrite,
                  output        pcsrc, alusrc,
                  output        regdst, regwrite,
                  output        jump,
                  output  [2:0] alucontrol,
                  output zeroExtend);

// **PUT YOUR CODE HERE**
    wire branch;
    wire bne;//changes: added this to store bne signal
    wire [1:0] aluop;

    maindec maindecModule(op,memtoreg,memwrite,branch,bne,regdst,regwrite,jump,alusrc,aluop,zeroExtend);//main decoder that generates signals based on op signal
    aludec aludecModule(funct,aluop, alucontrol);//alu decoder that generates alu signals based on aluop from main decoder and func signal

    //assign pcsrc = zero & branch; //pcsrc signal is combinational and of the branch wire and the zero input signal
    
    //changes:added pcsrc combinational logic for bne
    assign pcsrc = (zero & branch) | (~zero & bne);//combinational logic for branch and bne instr

endmodule

module maindec(input [5:0] op,
               output memtoreg,memwrite,
               output branch,bne,
               output regdst,regwrite,
               output jump, alusrc,
               output [1:0] aluop,
               output zeroExtend);//changes: added zeroExtend output
    reg [10:0] controls; //changes: increased control width by 2 to accomadate bne and zeroExtend
    assign {regwrite,regdst,alusrc,branch,memwrite,memtoreg,aluop,jump,bne,zeroExtend} = controls;//changes: added bne signal 
    always@(*)
        case(op)
            6'b000000 : controls<= 11'b11000010000;//R type
            6'b100011 : controls<= 11'b10100100000;//lw
            6'b101011 : controls<= 11'b0x101x00000;//sw
            6'b000100 : controls<= 11'b0x010x01000;//beq
            6'b001000 : controls<= 11'b10100000000;//addi
            6'b000010 : controls<= 11'b0xxx0xxx100;//j
            
            //changes: added cases for ori and bne op codes
            6'b001101 : controls<= 11'b10100011001;//ori
            6'b000101 : controls<= 11'b0x000x01010;//bne
            default   : controls<= 11'bxxxxxxxxxxx;//illegal
        endcase
endmodule


module aludec(input [5:0] funct,
              input [1:0] aluop,
              output reg [2:0] alucontrol);

    always@(*)
        case(aluop)
            2'b00: alucontrol<=3'b010;//i add
            2'b01: alucontrol<=3'b110;//i sub
            
            //changes: added case for i instruction or
            2'b11: alucontrol<=3'b001;//i or
            default: //10
                case(funct) //for r type
                    6'b100000: alucontrol<=3'b010;//add
                    6'b100010: alucontrol<=3'b110;//sub
                    6'b100100: alucontrol<=3'b000;//and
                    6'b100101: alucontrol<=3'b001;//or
                    6'b101010: alucontrol<=3'b111;//slt
                    default:   alucontrol<=3'bxxx;//illegal
                endcase
        endcase

endmodule


// Todo: Implement datapath
module datapath(input          clk, reset,
                input          memtoreg, pcsrc,
                input          alusrc, regdst,
                input          regwrite, jump,
                input   [2:0]  alucontrol,
                output         zero,
                output  [31:0] pc,
                input   [31:0] instr,
                output  [31:0] aluout, writedata,
                input   [31:0] readdata,
                input zeroExtend);//changes: added zeroExtend input
// **PUT YOUR CODE HERE**                
    wire [31:0] PCPLUS4, ImmExt,shiftImmExt;
    wire [31:0] zeroImmExt,chosenImm;//changes: added corresponding signals to store zero extended immediate and the mux output
    wire [31:0] PCBranch,pcnext1,pcnext2;
    wire [31:0] PCJUMP27,PCJump;
    wire [4:0] WriteReg;
    wire [31:0] result;
    wire [31:0] SrcA, SrcB;

    adder pcPlus4Adder(pc,32'b100,PCPLUS4);//PCPLUS4= PC+4
    assign ImmExt = {{16{instr[15]}},instr[15:0]};//sign extends imm at instr[15:0]
    assign shiftImmExt = {ImmExt[29:0],2'b00};//shiftImmExt is ImmExt shifted 2 bits left
    adder pcBranchAdder(shiftImmExt,PCPLUS4,PCBranch);//adds shifted signext imm to pc+4, gets branch adress
    assign PCJump = {PCPLUS4[31:28],instr[25:0],2'b00};//gets jump adress from corr variables

    mux2 pcMux1(PCPLUS4,PCBranch,pcsrc,pcnext1);//mux between PC+4 and the branch adress, select is pcsrc signal
    mux2 pcMux2(pcnext1,PCJump,jump,pcnext2);//mux between previous mux output and jump adress, select is jump signal
    dflipflop pcSeq(clk, reset, pcnext2, pc);//d ff to update pc with the nextpc at next clock edge

    //changes
    assign zeroImmExt = {{16{1'b0}},instr[15:0]};//changes: zero extends imm at instr[15:0]
    mux2 immMux(ImmExt,zeroImmExt,zeroExtend,chosenImm);//changes: mux to select between zero and sign ext imm

    regfile regfileModule(clk,regwrite,instr[25:21],instr[20:16],WriteReg,result,SrcA,writedata);
    mux2 #(5) writeRegMux(instr[20:16],instr[15:11],regdst,WriteReg);//chooses between which register to write to based on r or i type
    
    //changes
    //mux2 srcBMux(writedata,ImmExt,alusrc,SrcB);//mux to choose between register and immediate 
    mux2 srcBMux(writedata,chosenImm,alusrc,SrcB);//changes: changed immediate input to be input from immMux
    
    ALU aluModule(SrcA,SrcB,alucontrol,aluout,zero);//alu module

    mux2 resultMux(aluout,readdata,memtoreg,result);//chooses between memmory or register to write from
    

endmodule


module regfile(input clk, we3,
                input [4:0] a1,a2,
                input [4:0] a3,
                input [31:0] wd3,
                output [31:0] rd1,rd2);
    reg [31:0] rf[31:0];//32 32 bit registers
    assign rd1 = (a1!=0)?rf[a1]:0;//if a1 is not register 0, returns word at a1 reg
    assign rd2 = (a2!=0)?rf[a2]:0;//if a2 is not register 0, returns word at a2 reg

    always @ (posedge clk)
        begin
            if(we3)//if enable is hi, writes data at wd3 to register a3
                rf[a3] <= wd3;
        end

endmodule



module adder(input [31:0] a, b,
            output [31:0] y);
    assign y = a + b;
endmodule

module mux2 #(parameter WIDTH = 32)//default 32 bit mux
            (input [WIDTH-1:0] d0, d1,
            input s,
            output [WIDTH-1 :0] y);
    assign y = s ? d1 : d0;
endmodule


module dflipflop(input clk, rst,
                input [31:0] d,
                output reg [31:0] q);
    always@(posedge clk, posedge rst)
        begin
            if(rst)
                q<=0;
            else
                q<=d;
        end
endmodule






