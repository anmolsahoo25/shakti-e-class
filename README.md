[![pipeline status](https://gitlab.com/shaktiproject/cores/e-class/badges/master/pipeline.svg)](https://gitlab.com/shaktiproject/cores/e-class/commits/master)
# SHAKTI E-CLASS #

This is the E-class core of the SHAKTI Processor family. The core has been completely developed using BSV (Bluespec System Verilog). This is the embedded class processor, built around a 3-stage in-order core. It is aimed at low-power and low compute applications and is capable of running basic RTOSs like FreeRTOS (Zephyr and eChronos are also being ported and will be released soon). Typical market segments include: smart-cards, IoT sensors, motor-controls and robotic platforms

If you wish to contribute please make a pull-request. You can also reach out to us at: shakti.iitm@gmail.com

## Current Feature List

* You can generate a core which supports the RV32/64-IMAC RISC-V ISA extensions of subset of this combination.
* Uses DSP based multi-cycle multiplier for FPGA ports to Xilinx based FPGAs.
* Supports Machine and User mode only.
* Optional enable,disable of USER-TRAP handling mechanism.
* The core alone consumes less than 3K LUTs on a base Artix7 FPGA .

## Quick Start

        $ git clone https://gitlab.com/shaktiproject/cores/e-class
        $ make patch
        
It is important to set the SHAKTI_HOME variable for generation of the core and relevant other surrounding tools and software. It is recommended
to set this variable in your bashrc permanently.

## Directory Structure

* **src**: This directory contains the BSV source for the e-class core.
* **verification**: This directory containts various scripts to run tests and regressions on the core.
* **README.md**: This file
* **soc_config.inc**: This file is used to change configurations of the code and generate variants. Details of legal changes is documented later in this file.
* **Makefile**: The makefile to compile, link and simulate the e-class design.

## Pre-requisites

A BSV compiler (version 2017 or above) is necessary to compile the code. More information on Bluespec can be found [here](www.bluespec.com).

## Configuring the Core ###

The soc_config.inc file is used to configure the specs of the core and the Soc that you would like to generate. Following are the current options. The valid values for most of these are "enable" or "disable" unless specified otherwise:

| Feature  | Valid Values | Description |
|----------|--------------|-------------|
| __ISA__  | RV64/RV32[I][M][A][C] | The ISA support you want the core to provide. Unsuported instructions will be treated as illegal |
| __MUL__  | fpga, sequential, parallel | FPGA, uses a DSP based wrapperfor Xilinx FGPAs, Sequential chooses a variable cycle multiplier with early-out mechanism. Parallel, simply implements a combinational multiplier. |
|__SYNTH__ |  FPGA,SIM    |  When compiling BSV for simulation set to SIM|
|__MAINMEM__  | BRAM,DDR| BRAM will implement main-mem module made of BRAMs. DDR setting will simply instantiate a bus-master which can be connected directly to the DDR ip.|
|__VERBOSITY__  | 0, 1 and 2 |Higher the number, more the display statements while simulation|
|__BOOTROM__|enable,disable|Instantiates a read-only BRAM memory of 64KB size.|
| __USERTRAPS__|True,False| When True, implements the user-space csrs meant to handle traps in user-mode.|
|__COREFABRIC__|AXI4, AXI4Lite, TileLink| Chooses which protocol based master must be instantiated for the core.|
|__MULSTAGES__| 1 to XLEN| This defines the latency of the DSP multiplier that needs to be generated and instantiated|
|__DIVSTAGES__| 1 to XLEN| This defines the number of cycles for the non-restoring division algorithm|
|__FPGA__| Xilinx Part Number| This should be set with the FPGA part number the synthesis is targetted towards.|
|__SYNTHTOP__| Module Name| This is the module name of the design that needs to be synthesized in the FPGA.|
|__COVERAGE__| none,all, line, toggle, user| This is used for verilator simulation only and enables the coverage.|
|__TRACE__| enable, disable | This is used by verilator to enable or disable VCD dump.|
|__THREADS__|Integer| Number of threads to be used by verilator.|
|__RTLDUMP__|True,False| When set to True enables creation of rtl.dump.|
|__SIGNATURE__|True,False| When set to True instantiates a signature-dump master on the bus-fabric.|
|__SIGNATURESTART__| Hex value without any prefix like "0x" or "'h"| This indicates the start address of the signature dump.|

# Compiling the Core/SoC

The Makefile in the root-folder is to be used to compile the core/SoC bsv code. For the makefile to work you need to:

* have soc_config.inc in the same folder (template is provided)
* have a file called "old_vars" in the same folder (template is provided). 
* set the following variables in Makefile:
    * __TOP_MODULE__: set it to the top bsv module name (eg. mkTbSoc)
    * __TOP_FILE__:   set it to the file (with .bsv extension) containing the TOP_MODULE (eg. TbSoc.bsv)
    * __TOP_DIR__:    set it to relative path from root-folder containing TOP_FILE (eg. src/testbench.bsv )

Following are the make targets that a user can use:

| Target | Description |
|--------|-------------|
|__compile_bluesim__| This will compile the code in the bsim environment and generate bsv intermediate files in the bsv_build folder.|
|__link_bluesim__|This will link the bsim compiled code and generate a binary in the bin folder.|
|__generate_verilog__|This will compile the code in the verilog environment and generate the verilog files in the verilog folder.|
|__link_ncverilog__| This will compile the generated verilog files and generate an executable in the bin folder using Cadence ncvlog and ncelab tools.|
|__link_msim__|This will compile the generated verilog files and generate an executable in the bin folder using Modelsim.|
|__link_iverilog__|This will compile the generated verilog files and generate an executable in the bin folder using iVerilog.|
|__link_verilator__| This will link the generated verilog files using verilator|
|__simulate__|This executes the "out" executable created by any of the above link_* commands.|
|__clean__| Will delete the bin and bsv_build folders.|
|__clean_verilog__|Will call clean and remove the verilog folder as well.|
|__restore__|Will call clean and clean_verilog  and also perform a clean in the benchmarks folder.|
|__generate_boot_files__|By default the core will start execution from 0x1000 which is mapped to the read-only BootROM. To match the execution with spike this region should hold the dts files which is available in verification/dts/boot.hex . This target will convert the hex file into a format which can be loaded into C-CLASS's bootrom.|
|__ip_build__| Builds the xilinx coregen ips like multiplier, mig, etc to be used by the core|
|__vivado_build__| Initiates a vivado synthesis using the __SYNTHTOP__ variable as the top-module. Results are available in the fpga folder. The project is called e-class|

# Simulation Requirements

While simulating the core using the "out" executable (generated by any of the link_* targets of the makefile) the following files are required to be present in the same folder as the executable:

* boot.LSB and boot.MSB: generated using the makefile target "generate_boot_files".
* code.mem.LSB and code.mem.MSB: For a software code compiled at 0x80000000 to be loaded into the SHAKTI's memory it has to be first converted to a hex file using the elf2hex command. A elf2hex command is as follows:

            $ elf2hex 8 32768 software.elf 2147483648 > code.hex

This code.hex should now be further split into code.mem.LSB and code.mem.MSB as follows:

            $ cut -c1-8 code.hex> code.mem.MSB 
            $ cut -c9-16 code.hex > code.mem.LSB 
   
To generate VCD go to the bin and execute the following:

* in bsim environment: ./out -V
* in verilog environment: ./out +bscvcd

# Verilog Sources
The verilog instances of the e-class are released un GPL 3 license.
Please check the Release Tags for verilog releases.
These releases are typically meant for simulation only. 

If you interested in acquiring a BSD version of the verilog or any other instance of the verilog kindly send a mail at: shakti[dot]iitm[at]gmail[dot]com
