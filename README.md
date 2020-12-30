# Implementation of Mips Single-Cycle Processor using Verilog

## Summary
Control and Datapath implemented support R Type(add,sub,or,slt), lw, sw, beq, bne, addi, j and ori instructions.

## Contents
- ALU.v contains code for the alu module
- Memfile.dat, Memfile2.dat contain test instructions
- mips.v contains implementation of the Single-Cycle Processor. Includes both Datapath and Control.
- mipsmem.v has data and instruction memmory modules
- mipstop.v connects processor with instruction and data memmory
- tb.v is the testbench
