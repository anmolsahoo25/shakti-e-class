# Device Memory Map: #

| Device            | Base Address | End Address |
|-------------------|--------------|-------------|
| BRAM based memory | 0x80000000   | 0x8007FFFF |
| Boot ROM          | 0x00001000   | 0x00010FFF |
| CLINT             | 0x02000000   | 0x020BFFFF |
| SimplUART         | 0x00011300   | 0x00011340 |
| SignDump          | 0x00020000   | 0x00020000 |

# Device Specifications: #

## CLINT ##
Standard Core Level Interrupt Module. Contains the Software interrupt bit, mtime value and mtime-compare registers.
Following is the access specifications

| Register   | Address  | Size in Bits   | Note                  |
|------------|----------|----------------|-----------------------|
| MSIP       |0x02000000| 1              |Read/Write using any load/store variant. Only lower bit contains relevant info |
| MTIME      |0x20004000| 64             |Read only using lb, lh, lw or ld |
| MTIMECMP   |0x2000BFF8| 64             |Read/Write using any load/stire variant|


## SimplUART ##
A simple uart implementation with just Rx/Tx coming out. Allows user to program the baud-rate.
Specs: 8-bit data, No parity, 1 stop bit, 16 samples.
Tested on FPGA.

Following is the access specifications (NE: Not Empyt, NF: Not Full):

| Register   | Address  | Size in Bits   | Note                  |
|------------|----------|----------------|-----------------------|
| BaudReg    |0x00011300| 16             |Read/Write using any lh/sh only.|
| RxReg      |0x00011304| 8              |Write only using sb to transmit data|
| TxReg      |0x00011308| 9              |Read only using lb to read received data|
| StatusReg  |0x0001130c| 4              |Read only using lb. Lower four bits organized as: {reciever-NE, receiver-NF, transmitter-NF, transmitter-NE} |


## SignDump ##
A simulation only module which is used to dump memory contents between specified regions.
This module is typically used to dump signatures of the tests found in riscv-tests and compliance suite.
The memory contents are written to a file named  "signature"

Following is the access specifications:

| Register     | Address  | Size in Bits   | Note                  |
|--------------|----------|----------------|-----------------------|
| Begin Address|0x00020000| 32             |Write using sw only. Defines the address of the first word to begin dumping. Address should be 4-byte aligned|
| End Address  |0x00020008| 32             |Write using sw only. Defines the address of the last word+4bytes to end dumping.  Address should be 4-byte aligned|
| End Sim      |0x0002000c| -              |Write to this using any store variant will cause the simulation to end after dumping.| 


