#This README Contains details about custom traps and their usage

As an extension to the 15 exceptions mentioned in the RISCV SPEC,we have added six arithmetic exceptions.

Out of this five are floating point exceptions specified by IEEE 754 floating point format.

Traps and their cause values are

            + ---------------------------------------+
            | Integer divide by zero            | 17 |
            +----------------------------------------+                                     

Above cause values can be changed by modifying their values in src/core/common_params.bsv.

To configure E-class to trap on these exceptions

1. set ARITH_TRAP in soc_config to enable before generating the verilog.

2. rg_custom_control(CSR address:0x800) is a user defined r/w csr register.Fourth bit of register should be set to one to enable arithmetic traps.This should be done as part of application.
























