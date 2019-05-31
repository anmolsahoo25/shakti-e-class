# Compiling and Simulating the E-Class

The E-class core is highly-configurable and thus requires certain parameters to be defined at compile time. This readme will guide you through the steps of generating an instance of the core in a basic Soc and simulating it. Steps to simulate linux on the core are also provided.

# Quickstart

Make sure you have the `$BLUESPECDIR` variable set to point your `<bluespec-install-dir>/lib`

```
cd e-class/base-sim
./manager.sh update_deps
make CONFIG=soc_config.inc generate_verilog
make CONFIG=soc_config.inc link_verilator
make generate_boot_files CONFIG=soc_config.inc
```

Now assuming your program/elf resides in the `bin` folder - 
```
cd bin
elf2hex 8 524288 prog.elf 2147483648 > code.hex
cut -c1-8 code.hex> code.mem.MSB 
cut -c9-16 code.hex > code.mem.LSB 
```

# Guide
- [Compiling and Simulating the E-Class](#compiling-and-simulating-the-e-class)
- [Quickstart](#quickstart)
- [Guide](#guide)
  * [Dependencies](#dependencies)
  * [Generating a SoC RTL for the E-Class core](#generating-a-soc-rtl-for-the-e-class-core)
      - [1. Cloning Dependent Modules](#1-cloning-dependent-modules)
      - [2. Generate RTL](#2-generate-rtl)
      - [3. Generate executable for simulation](#3-generate-executable-for-simulation)
      - [4. Generating boot-files](#4-generating-boot-files)
  * [Simulating Programs on E-class](#simulating-programs-on-e-class)
      - [1. Supporting printf](#1-supporting-printf)
      - [2. Simulation Outputs](#2-simulation-outputs)
  * [Connecting to GDB](#connecting-to-gdb)
      - [1. Simulate the RTL](#1-simulate-the-rtl)
      - [2. Connect to OpenOCD](#2-connect-to-openocd)
      - [3. Connect to GDB](#3-connect-to-gdb)
  * [FreeRTOS on Shakti](#freertos-on-shakti)
      - [1. Generate RTL](#1-generate-rtl)
      - [2. Generate FreeRTOS Image](#2-generate-freertos-image)
      - [3. To simulate on E-class](#3-to-simulate-on-e-class)

## Dependencies

The core along with the provided SoC can be simulated on a number of commercial and open-source verilog simulators. The scripts for each of the following platforms can be found in the Makefile.

* **Open Source Simulators**:
    * `Verilator`: You will need verilator-4.004 or above to compile the generated verilog RTL into an executable. You can download verilator from [HERE](https://www.veripool.org/projects/verilator/wiki/Installing). This the most supported platform for a number of SHAKTI E-class cores. Once the verilog of the design is generated/available use the command `make link_verilator` to generate the executable in the `bin` directory.
* **Commercial Simulators**: These simulators will look for sources in the Bluespec directory thus requiring `$BLUESPECDIR` to be set before running them. These platforms will not be rigorously supported and are meant to provide basic templates to each platform. Currently supported simulators are:
    * `VCS`, `NCVERILOG`, `Modelsim` and `IRUN`

## Generating a SoC RTL for the E-Class core

This folder contains a sample (synthesizable) SoC (`Soc.bsv`) and a testbench (`TbSoc.bsv`) with minimal peripherals like: uart, clint, jtag-debugger, bram-based bootrom and a bram-based memory (acting as main-mem). These two files are provided as a base template for the user to create their own complex SoCs. A few standard SoCs instantiating E-class can be found [HERE](https://gitlab.com/shaktiproject/cores/shakti-soc). 

In order to build the Soc various components like caches, fabrics, devices, etc are required which are maintained in separate repos. These dependencies first have to be pulled from their respective sources.

#### 1. Cloning Dependent Modules

To capture all the dependencies 
```
./manager.sh update_deps
```

The above command will clone all the required repositories to build the SoC.

#### 2. Generate RTL

The Makefile in this folder contains multiple targets to generate verilog and link the rtl using different simulators. The makefile requires a configuration of the core as input. The `soc_config.inc` file provides a sample configuration of the core. The details of each hook are available [HERE](/docs/configuring_code.md). Other various templates used in our CI/CD are available in the `templates` folder.

To see all available targets:
```
make help
```

To initiate bluespec compilation:
```
make CONFIG=soc_config.inc generate_verilog
```

One can also pass other configs as an argument to the `CONFIG` variable.
The above command will generate verilog files in the `verilog` folder.

#### 3. Generate executable for simulation

The makefile supports compiling the verilog for various simulators. However, verilator is the most supported target in SHAKTI as compared to other other simulators (iverilog, irun, msim and vcs). 

To generate a verilated executable:
```
make CONFIG=soc_config.inc link_verilator
```

The above command will create an executable named `out` in the `bin` folder. 


#### 4. Generating boot-files
By default the core will start execution from 0x1000 (change RESETPC in `soc_config.inc` to change this variable) which is mapped to the read-only BootROM of the provided SoC. The simulated executable thus expects 2 files `boot.MSB` and `boot.LSB` to be present in the same folder to continue simulation. To generate these files:

```
make generate_boot_files CONFIG=soc_config.inc
```

The above command will generate the required boot files in the bin folder. The boot-files basically hold the dts(device-tree-string) which is available in verification/dts/boot.hex. The first few instructions sets the redirection to 0x80000000 where the program needs to reside.

## Simulating Programs on E-class

The following steps assume that an `out` executable and the `boot.LSB` and `boot.MSB` files exist in the `bin` folder.
Let's the software program that you would like to simulate on the core is called `prog.elf` (compiled using standard riscv-gcc).

In order for the `out` executable to work the following files are requires in addition to the `boot.*` files: `code.mem.LSB` and `code.mem.MSB`. These files can be generated using the `elf2hex` command:

```
elf2hex 8 524288 prog.elf 2147483648 > code.hex
```

This code.hex should now be further split into code.mem.LSB and code.mem.MSB as follows:
```
cut -c1-8 code.hex> code.mem.MSB 
cut -c9-16 code.hex > code.mem.LSB 
```
            
For a 32-bit core use the following:
```
 elf2hex 4 524288 software.elf 2147483648 > code.mem.MSB
 elf2hex 4 524288 software.elf 2147483648 > code.mem.LSB
 ```
Please note, since the boot code in the bootrom implicitly jumps to `0x80000000` the programs should also be compiled at 0x80000000. Plus the bram main memory is 512KB large. This size can be changed by changing the value of `Addr_space` in `Soc.bsv`. Changing addr-space would required similar changes to the `elf2hex` command arguments as well.

#### 1. Supporting printf

The SoC for simulation contains a simple uart. The putchar function for the same is available [HERE](https://gitlab.com/shaktiproject/uncore/devices/blob/master/uart/uart_driver.c) . This has to be used in the printf functions. The output of the printf is captured in a separate file `app_log` during simulation.

#### 2. Simulation Outputs

1. if `RTLDUMP` variable is `enabled` in `soc_config.inc` then an rtl.dump file is created which contains the trace of the instruction execution sequence.
2. if `VERBOSITY` variable is set to more than 0, then executing the `out` binary will print all the display statements on the screen.
3. if printf statements are used in software program, then those get dumped in the `app_log` file.

## Connecting to GDB

A debugger implementation following the riscv-debug-draft-014 has been integrated with the core. 
Perform the following steps to connect to the core executable with a gdb terminal. This assumes you have installed openocd and is available as part of you $PATH variable.
Generat executable with debugger enabled and testbench with vpi to support jtag remote-bitbang:

```
make CONFIG=templates/debugger.inc gdb

```
#### 1. Simulate the RTL
In a new terminal do the following:
```
cd e-class/base-sim/bin/
./out > /dev/null
```

#### 2. Connect to OpenOCD
Open a new terminal and type the following:
```
cd e-class/base-sim/gdb_setup/
openocd -f shakti_ocd.cfg
```

#### 3. Connect to GDB
Open yet another terminal and type the following:
```
cd e-class/base-sim/gdb_setup
riscv64-unknown-elf-gdb -x gdb.script
```

In this window you can now perform gdb commands like : `set $pc, i r, etc`

## FreeRTOS on Shakti 
#### 1. Generate RTL
```
make CONFIG=templates/freertos_sim.inc 
```

#### 2. Generate FreeRTOS Image

Download the shakti-linux repository 
```
git clone https://gitlab.com/shaktiproject/software/FreeRTOS

```
To generate the kernel image 
```
cd FreeRTOS/FreeRTOS-RISCV/Demo/shakti/
make
```

#### 3. To simulate on E-class
Come back to the folder for e-class/base-sim/:
```
cd e-class/base-sim
cp FreeRTOS/FreeRTOS-RISCV/Demo/shakti/frtos-shakti.elf ./bin
cd bin
elf2hex 8 8388608 frtos-shakti.elf 2147483648 > code.mem
cut -c1-8 code.mem > code.mem.MSB 
cut -c9-16 code.mem > code.mem.LSB 
./out
```
