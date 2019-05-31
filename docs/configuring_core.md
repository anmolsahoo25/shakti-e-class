# Configuring the Core

The E-class is a highly-configurable core. Almost all the configurable hooks can be controlled buy the variables defined in the `soc-config.inc` file (yes yes .. it will be renamed as core-config).



  * [Configuring the ISA](#configuring-the-isa)
  * [Configuring the M-Extension](#configuring-the-m-extension)
  * [Configuring the Simulation Environment](#configuring-the-simulation-environment)
  * [Configuring Physical Memory Protection.](#configuring-physical-memory-protection)
  * [Configuring Debugger and Triggers](#configuring-debugger-and-triggers)
  * [Miscellaneous Configurations](#miscellaneous-configurations)


## Configuring the ISA

* __ISA__: This variable defines the RISC-V ISA to be supported by the E-Class core. The core supports the I, M, A, F, D and C extensions in both 32-bit and 64-bit formats. 
    * *Usage*: `ISA=RV32IMAC`, `ISA=RV64IMAFDC`, etc

* __USERTRAPS__: Valid options:
    * `enable`: enables the `N` Extension of RISC-V ISA spec.
    * `disable`: disables the `N` Extension of RISC-V ISA spec.
* __USER__: Valid options:
    * `enable`: enables the `U` Extension of the RISC-V ISA spec.
    * `disable`: disables the `U` Extension of the RISC-V ISA spec.

## Configuring the M-Extension

The following hooks only come in to effect when `M` is present in the `ISA` variable described above.

* __MUL__: This variable is used to configure the type of multiplier to be used while supporting the M-extension. Valid Options are
    * `fpga`: This will use a Xilinx's DSP based multiplier coregen IP. The number of stages in the multiplier is defined using the MULSTAGES variable in the `soc_config.inc`. The bit-width of the multiplier is 32-bits if the ISA variable field includes `RV32` and 64-bits if the ISA variable includes `RV64`. A bluespec wrapper to incorporate this IP in bluespec is available in the src/wrappers.bsv directory. This should be set when targetting FPGAs. For simulation purpose, a functional bsv code is used which mimics the FPGA IP in ports and behavior. Please note this will not simulate the DSP IPs from Xilinx.
    
    * `asic`: This will implement either a single-cycle multiplier or a sequential multiplier without an early out mechanism. The choice depends on the value of MULSTAGES. The bsv source of this is available in the src/m_ext folder. The bit-width of the multiplier is 32-bits if the ISA variable field includes `RV32` and 64-bits if the ISA variable includes `RV64`. This should be set when targetting ASIC synthesis.

* __MULSTAGES__: a numerical value indicating the number of cycles to perform multiplication. When set to `0` a single-cycle multiplier is instantiated. When `MUL` is set to fpga, this variable will indicate the number of cycles the Xilinx IP is configured with. When `MUL` is set to `asic` a non-zero value will instantiate an 8-cycle early out multiplier.  

    Please note, Max number should not cross XLEN value specified in the `ISA` variable.
    
* __DIVSTAGES__: The e-class implements an iterative restoring division algorithm. This variable indicates the number of cycles it takes the algorithm to complete the division. For max-frequency keep this value as equal to the XLEN value specified in the `ISA` variable. For highest throughput keep it low. i.e. 1.

## Configuring the Simulation Environment
* __VERBOSITY__: Valid values are 0 or 1. 
    Once the bluespec-generated-verilog is available, post-processing script is run to subsume all the `$display` statements in the verilog file, under the `VERBOSE` macro. This variable is used to indicate the RTL simulator (e.g. verilator, vcs, modelsim, etc.) whether or not to the `VERBOSE` macro is to be defined or not at compile-time. A value of 0 will ensure that no `$display` statements are compiled (except the ones inserted by bluespec based on rule-conflicts). A non-zero 0 will ensure they are compiled and one can use the Logger scheme to enabled different modules and levels at simulation time.

* __COVERAGE__: This is used by verilator to generate specific coverage metrics during simulation. Valid otions:
    * `none`: disable all coverage
    * `all`: enable all coverage metrics supported in verilator
    * `line`: enable line coverage
    * `toggle`: enable toggle coverage
* __TRACE__: Valid options:
    * `enable`: enables vcd dump for verilator simulation
    * `disable`: disables vcd dump for verilator simulations
    To enable vcd generation during simulation use the command `./out +trace`
* __THREAD__: Integer number indicating the number of threads to be used by verilator for simulation
* __VERILATESIM__: Valid options:
    * `fast`: This will insert the `-CFLAGS -O3` flag while compiling with verilator. The resultant
      executable is small and fast in simulation. The compile time for this is quite huge.
    * `small`: The verilated binary compiled with this option will be large and slow in simulation.
      However, the compile time is much much faster.
* __RTLDUMP__: Valid options:
    * `enable`: Will generate a rtl.dump file during simulation providing a trace of the instruction execution of the application on the core. Please note that enabilng this feature requires extra hardware to be generated and thus should be disabled when targeting synthesis.
    * `disable`: Will disable rtl.dump generation.
* __ASSERTIONS__: Valid options:
    * `enable`: Will enable dynamic assertions where-ever found.
    * `disable`: Will disable dynamic assertions where-ever found.
    In Bluespec, the dynamic/static assertions from the `Assert` library are compiled into `$display` statements in the generated verilog. Thus, having this feature enabled while targeting synthesis will have no effect on the synthesis. (All the display statements come under synopsys_translate_off macro which almost all synthesis tools will ignore)

## Configuring Physical Memory Protection.
* __PMP__: Valid options
    * `enable`: Will enable physical memory protection scheme as mentioned in the spec.
    * `disable`: Will disable physical memory protection scheme as mentioned in the spec.
* __PMPSIZE__: An integer `<=16` indicating the number of PMP registers to be implemented.

## Configuring Debugger and Triggers
* __DEBUGGER__: Valid options are `enable` or `disable`. When enabled a JTAG based debugger is instantiated in the SoC. This debugger is based on the riscv-debug-spec.
* __OPENOCD__: Valid options are `enable` or `disable`. When enabled the test-bench includes a vpi to connect to open-ocd via remote-bitbang.
* __TRIGGERS__: Accepts a valid number >=0. 
    Indicates the number of hardware triggers instantiated in the core. More information about the nature of the triggers can be found [here](../docs/triggers.md)
    
## Miscellaneous Configurations
* __RESETPC__: Values in integer format indicating the reset program counter value.
* __PADDR__: Bit-wdith of the phyiscal address.
* __COREFABRIC__: This variable indicates the fabric protocol supported by the Core. Current valid options are only `AXI4`. Future support will be provided for AXI4-Lite, TileLink-U, TileLink-H and TileLink-C.
* __SYNTH__: Indicates the nature of the generated verilog. Valid options:
    * `sim`: Setting this variable to `sim` will enable the `simulate` macro during bluesim compilation which enable a few simulation logic in the hardware. Eg,: if the MUL variable is set to `fpga` and `SYNTH` is set to `sim` then a naive bluespec model which mimics DSP multiplier in functionality will be used to simulation. The `sim` setting should only be used for simulations. 
    * `asic`: This should be set to generate a verilog for synthesis and will not include any simulation models.
* __ARITH_TRAP__: Valid values are `enable` or `disable`. When enabled, the core will treat some arithmetic operations as traps. More details of the behavior area available [here](../docs/arithmetic_trap.md)
