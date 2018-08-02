# SHAKTI E-CLASS #

This is the E-class core of the SHAKTI Processor family. This repo contains all the relevant
hardware and software related to the e-class core. The core and the peripherals are developed using Bluespec.

### Setting up environment variables ###

Follow the below steps before using the code-line:
``
  $ git clone https://bitbucket.org/casl/e-class.git
``
``
  $ cd e-class
``
``
  $ export XILINX_VIVADO=/tools/Vivado/Vivado/2016.1
``
``
  $ make ip_build
``
``
  $ make generate_verilog
``
``
  $ make link_vcs
``
