`timescale 1ns/1ns

module testbench();
    reg clk = 0;
    reg reset;
   
    // instantiate device to be tested
    top dut(clk, reset);
    // initialize test
    initial
        begin
            reset <= 1; # 5; reset <= 0;
        end

    initial 
	begin
	   forever begin
		clk<=~clk;
		#5;
	end
   end

   initial
   begin
   	#200;
	$stop;
   end
    
endmodule