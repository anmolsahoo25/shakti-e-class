[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![pipeline status](https://gitlab.com/shaktiproject/cores/e-class/badges/master/pipeline.svg)](https://gitlab.com/shaktiproject/cores/e-class/commits/master)
# SHAKTI E-CLASS #

This is the E-class core of the SHAKTI Processor family. The core has been completely developed using BSV (Bluespec System Verilog). This is the embedded class processor, built around a 3-stage in-order core. It is aimed at low-power and low compute applications and is capable of running basic RTOSs like FreeRTOS and Zephyr (eChronos is also being ported and will be released soon). Typical market segments include: smart-cards, IoT sensors, motor-controls and robotic platforms


# Guide
- [SHAKTI E-CLASS](#shakti-e-class)
  * [Quick Start](#quick-start)
      - [Dependencies](#dependencies)
      - [Clone the repo](#clone-the-repo)
      - [Install DTC](#install-dtc)
  * [Directory Structure](#directory-structure)
  * [Overview of E-CLASS](#overview-of-e-class)
  * [Compiling and Simulating the the Core](#compiling-and-simulating-the-the-core)
  * [Verilog-Artifacts](./docs/verilog-artifacts/verilog-artifacts.md)
  * [CI-CD](#ci-cd)
  * [Contributing to the Project](#contributing-to-the-project)



## Quick Start

#### Dependencies

A BSV compiler (version 2017 or above) is necessary to compile the code. More information
on Bluespec can be found [here](www.bluespec.com).

#### Clone the repo 

``` 
git clone https://gitlab.com/shaktiproject/cores/e-class.git
cd e-class/base-sim
./manager.sh
```
#### Install DTC
We use the DTC 1.4.7 to generate the device tree string in the boot-files. To install DTC follow the below commands:
```
sudo wget https://git.kernel.org/pub/scm/utils/dtc/dtc.git/snapshot/dtc-1.4.7.tar.gz
sudo tar -xvzf dtc-1.4.7.tar.gz
cd dtc-1.4.7/
sudo make NO_PYTHON=1 PREFIX=/usr/
sudo make install NO_PYTHON=1 PREFIX=/usr/             
```

## Directory Structure
    ├── src                     # dir: Source code of the core, uncore and devices
    │   ├── core                # dir: e-class core
    │   │   ├── m_ext           # dir: modules for M extension support
    ├── base-sim                # Contains a base SoC and setup to simulate and verify the core. 
    ├── docs		            # Contains more info about the core and the environment.
   
## Overview of E-CLASS 

* 3-stage 64/32-bit pipelined core.
* Supports ISA=RV64[32]IMAC based on riscv-spec-2.2 and privilege-spec-1.10.
* Early out multiplier and a restoring divider.
* Boots FreeRTOS, Zephyr.

## Compiling and Simulating the the Core

The repository provides a sample SoC with minimal components like uart, clint, signature-dump, etc which can be used to simulate programs on the core. For more details please refer to the [readme](base-sim/README.md) in the base-sim folder

## CI-CD

The project uses a local gitlab-runner housed at RISE-LAB, CSE-Dept, IIT-Madras. Each commit to the master branch will trigger a build run on the runner which does the following:

1. generate the verilog for the core config=rv64imdc. More details can be found in gitlab-cy.yaml.
2. Run all the riscv-tests on the generated verilog using verilator.
3. Run 40 torture tests on the the generated verilog using verilator [TODO]
4. Run 20 AAPG tests on the generated verilog [Done]
5. Run 230 CSMITH tests [TODO]
6. Run benchmarks as well. [TODO]

The runner also performs a nightly build which checks for other configs the runs tests on them as well.


## Contributing to the Project 

The quickest way to contribute to the SHAKTI project is to create a pull-request for existing issues or any feature additions. A contributing guideline will be uploaded soon
