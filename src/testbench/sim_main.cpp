// Top-level driver for "verilated" objects (Verilog compiled with verilator)

#include <verilated.h>

#include "VmkTbSoC.h"


vluint64_t main_time = 0;    // Current simulation time

double sc_time_stamp () {    // Called by $time in Verilog
    return main_time;
}

int main (int argc, char **argv, char **env) {
    Verilated::commandArgs (argc, argv);    // remember args

    VmkTbSoC* mkTbSoC = new VmkTbSoC;    // create instance of model

    mkTbSoC->RST_N = 0;    // assert reset
    mkTbSoC->CLK = 0;

    while (! Verilated::gotFinish ()) {

	if (main_time == 1) {
	    mkTbSoC->CLK = 1;
	}
	else if (main_time == 2) {
	    mkTbSoC->RST_N = 1;    // Deassert reset
	}

	// Toggle clock
	if ((main_time % 10) == 5) {
	    mkTbSoC->CLK = 0;
	}
	else if ((main_time % 10) == 0) {
	    mkTbSoC->CLK = 1;
	}


	mkTbSoC->eval ();
	main_time++;
    }

    mkTbSoC->final ();    // Done simulating

    delete mkTbSoC;
    mkTbSoC = NULL;

    exit (0);
}
