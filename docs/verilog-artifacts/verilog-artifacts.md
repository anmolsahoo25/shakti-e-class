# Verilog-Artifact

This document describes the verilog artifact that is released with each tag on the e-class repo. As of the now repo releases only a single verilog source of an instance of the e-class core under the [`GPL-V3`](https://www.gnu.org/licenses/gpl-3.0.en.html) License. 

- [Verilog-Artifact](#verilog-artifact)
  * [Intent of the Artifact](#intent-of-the-artifact)
  * [Downloading the Artifact](#downloading-the-artifact)
  * [Directory structure of Artifact](#directory-structure-of-artifact)
  * [RTL Details](#rtl-details)
    + [RTL hierarchy](#rtl-hierarchy)
  * [Synthesizing the Core](#synthesizing-the-core)
  * [Simulating Core](#simulating-core)
    + [BootRom content](#bootrom-content)
    + [Compiling programs](#compiling-programs)
  * [Simulation Outputs](#simulation-outputs)
  * [Ending the simulation](#ending-the-simulation)

## Intent of the Artifact

The verilog artifacts intend to enable users without access to a Bluespec compiler to evaluate the e-class core. This artifact includes an RTL of the full-featured e-class core which can be synthesized to both FPGAs and ASICs. The artifact also includes a standalone simulation model of the core which can be used to run small programs. More details of the artifact are provided below.

## Downloading the Artifact

There are [multiple-ways](https://docs.gitlab.com/ee/user/project/pipelines/job_artifacts.html#downloading-artifacts) to download the artifacts. The simplest is to simply click the following link: [ARTIFACT](https://gitlab.com/shaktiproject/cores/e-class/-/jobs/artifacts/master/download?job=release). 

The link should initiate the download of the latest artifact as an archive: `verilog-artifacts.zip`.


## Directory structure of Artifact
Once the tar-ball of the artifact has been downloaded untar it using the following command
```
unzip verilog-artifact.zip
```

The extracted `verilog-artifacts` folder has the following structure:

1. `verilog`: directory containing the verilog source code of the entire SoC along with the testbench
2. `sim`: directory containing an executable `eclass`, along with `boot.LSB` and `boot.MSB` files for simulation.
3. `benchmarks`: directory containing the sample benchmarks and programs which can be run on the core. 
4. `LICENSE`: contains the copyright information of the artifact.
5. `synth64.inc`: contains the configuration file used to the generate the this artifact from BSV.
6. `version.txt`: contains the tag and commit number of the repo at which point this release was made.

## RTL Details
The feature of the e-class (mkeclass_axi4) core are available in the `core_config.inc` file in the artifact folder. The user is encouraged to read the [configuring-core](https://gitlab.com/shaktiproject/cores/e-class/blob/master/docs/configuring_core.md) document for further information of the features. A quick summary of the features is presented below:

* RV32IMACNU.
* Multiplier with 4 cycle latency.
* Divider with 32 cycle latency.
* AXI4-Lite master interfaces of fetch and data.
* 2 Triggers.
* 4 counters.
* 4 pmp regions.
* JTAG based debugger.

The artifact also includes a sample Soc `(mkSoc.v)` which connects the core with a `uart`, `clint`, `bootrom` and few other slaves mentioned below. This Soc is provided only as a place-holder and for enabling simulations. This Soc is not intended to meet any particular metric of performance, power or area. The memory map of the Soc is provided below:

| Module | Address Range |
|--------|---------------|
|BRAM-Memory | 0x80000000 - 0x8FFFFFFF |
|BootROM     | 0x00001000 - 0x00010FFF |
|UART        | 0x00011300 - 0x00011340 |
|CLINT       | 0x02000000 - 0x020BFFFF |
|Debug-Halt Loop | 0x00000000 - 0x0000000F|
|Signature Dump  | 0x00002000 - 0x0000200c|


The artifact also includes a test-bench `(mkTbSoc.v)` which instantiates a dummy bram-based memory and a boot-rom.


### RTL hierarchy
The following diagram illustrates the top-4 levels of the hierarhcy including the test-bench

``` mermaid
graph TD
X[mkTbSoC] --> A(mkSoC)
X --> B(mkbram)
X --> C(mkbootrom)
A --> D(mkeclass_axi4)
A --> E(mkuart)
A --> F(mkclint)
A --> G(mksignature_dump)
D --> H(mkriscv)
```

Description of the above modules is provided in the following table:

| Module-Name | Description |
|------------------|-------------|
|mkriscv           | Contains the 5-stages of the core pipeline including the execution and only the interface to the memory subsystem |
|mkeclass_axi4     | Contains the above modules and the integrations across them. Also provides 3 AXI-4 interfaces to be connected to the Cross-bar fabric|
|mkuart             | UART module [(details)](https://gitlab.com/shaktiproject/uncore/devices/tree/master/uart) |
|mkclint            | Core Level Interrupt [(details)](https://gitlab.com/shaktiproject/uncore/devices/tree/master/clint)|
|mksignature_dump   | Signature dump module (for simulation only)  |
|mkSoc              | contains all the above modules and instantiates the AXI-4 crossbar fabric as well. The fabric has 2 additional slaves, which are brought out through the interface to connect to the boot-rom and bram-main-memory present in the Test-bench|
|mkbram             | BRAM based memory acting as main-memory [(details)](https://gitlab.com/shaktiproject/uncore/devices/tree/master/bram)|
|mkbootrom          | Bootrom slave [(details)](https://gitlab.com/shaktiproject/uncore/devices/tree/master/bootrom)                           |
|mkTbSoC            | Testbench that instantiates the Soc, and integrates it with the bootrom and a bram memory|


## Synthesizing the Core

When synthesizing for an FPGA/ASIC, the top module should be `mkeclass_axi4 (mkeclass_axi4.v)` as the top module. 


## Simulating Core

The `verilog-artifact/sim` directory includes a simulation model `(eclass)` of the testbench which was compiled using `verilator`. Users wanting to simulate the core with other simulators should refer to various `link_*` targets in the [Makefile](../../base-sim/Makefile) available in the repo.

Simulating core, requires `boot.MSB` and `boot.LSB` files which are also provided in the `verilog-artifact/sim` directory. The executable requires 2 more files `code.mem.LSB` and `code.mem.MSB` the generation of which is described below.

### BootRom content

On system-reset the core will always jump to `0x1000` which is mapped to the bootrom. The bootrom is initialized using the files `boot.MSB` and `boot.LSB`. The bootrom immediately causes a re-direction to address `0x80000000` where the main program is expected to lie. It is thus required that all programs are linked with text-section begining at `0x80000000`. The rest of the boot-rom holds a dummy device-tree-string information.

### Compiling programs
The directory `verilog-artifact/benchmarks` holds a few basic programs and their compile scripts which can be used as reference to compile the programs using the riscv-gnu-toolchain. Please refer to the `benchmarks/README` for further details. 

**NOTE**: Please note that the bram-based memory in the test-bench can only hold upt 2MB of code.

As shown in the sample examples of the benchmarks folder, for each program 2 files are generated: `code.mem.MSB` and `code.mem.LSB`. Coy these 2 files in the `verilog-artifact/sim` folder and then execute the following from the `verilog-artifact/sim` folder
```
./eclass 
```

## Simulation Outputs
The `syscalls.c` program in the `verilog-artifact/benchmarks/common` directory contains the sample `putchar` function which uses the uart module. The test-bench is configured to dump all the characters received by the uart in to a file : `app_log`. Thus, all the `printf` calls will be captured in a file named `app_log`. 


## Ending the simulation
The simulation will end with a store is performed at address `0x2000c`
