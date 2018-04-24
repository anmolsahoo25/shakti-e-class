### Makefile for the eclass project
### Generated by Bluespec Workstation on Thu Nov 12 19:54:06 IST 2015

include ./old_vars
include soc_config.inc

export SHAKTI_HOME=$$PWD
TOP_MODULE:=mkTbSoC
TOP_FILE:=TbSoC.bsv
TOP_DIR:=./src/testbench
WORKING_DIR := $(shell pwd)

ifneq (,$(findstring RV64,$(ISA)))
  define_macros += -D RV64=True
  XLEN=64
endif
ifneq (,$(findstring RV32,$(ISA)))
  define_macros += -D RV32=True
  XLEN=32
endif
ifneq (,$(findstring M,$(ISA)))
  define_macros += -D MULDIV=True
endif
ifneq (,$(findstring A,$(ISA)))
  define_macros += -D atomic=True
endif
ifeq ($(VERBOSE),enable)
  define_macros += -D verbose=True
endif
ifeq ($(PERF),enable)
  define_macros	+= -D perf=True
endif
ifeq ($(JTAG),enable)
  define_macros	+= -D JTAG=True
endif
ifeq ($(DEBUG),enable)
  define_macros += -D Debug=True
endif
ifeq ($(OPENOCD),enable)
  define_macros += -D Openocd=True
endif
ifeq ($(SYNTH),SIM)
  define_macros += -D simulate=True
endif
ifeq ($(BOOTROM), enable)
  define_macros += -D BOOTROM=True
endif
ifeq ($(COREFABRIC), AXI4Lite)
  define_macros += -D CORE_AXI4Lite=True
endif
define_macros += -D VERBOSITY=$(VERBOSITY) -D USERTRAPS=$(USERTRAPS) -D CORE_$(COREFABRIC)=True\
-D MULSTAGES=$(MULSTAGES) -D DIVSTAGES=$(DIVSTAGES)
CORE:=./src/core/
FABRIC:=./src/fabric/axi4:./src/fabric/axi4lite:./src/fabric/tilelink_lite
UNCORE:=./src/uncore
TESTBENCH:=./src/testbench/
PERIPHERALS:=./src/peripherals/bootrom:./src/peripherals/pwm
WRAPPERS:=./src/wrappers/
LIB:=./src/lib/
VERILATOR_FLAGS = --stats -O3 -CFLAGS -O3 -LDFLAGS -static --x-assign fast --x-initial fast --noassert --cc --bbox-sys -Wno-STMTDLY -Wno-UNOPTFLAT -Wno-WIDTH -Wno-lint -Wno-COMBDLY -Wno-INITIALDLY -Wno-INFINITELOOP
BSVINCDIR:=.:%/Prelude:%/Libraries:%/Libraries/BlueNoC:$(CORE):$(LIB):$(FABRIC):$(UNCORE):$(TESTBENCH):$(PERIPHERALS):$(WRAPPERS)
default: compile_bluesim link_bluesim generate_boot_files

check-env:
	@if test -z "$$BLUESPECDIR"; then echo "BLUESPECDIR variable not set"; exit 1; fi;
	@if test -z "$$SHAKTI_E_HOME"; then echo "SHAKTI_E_HOME variable not set"; exit 1; fi;

check-py:
	@if ! [ -a /usr/bin/python3 ] ; then echo "Python3 is required in /usr/bin to run AAPG" ; exit 1; fi;

###### Setting the variables for bluespec compile #$############################
BSVCOMPILEOPTS:= -check-assert  -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules -remove-starved-rules 
BSVLINKOPTS:=-parallel-sim-link 8 -keep-fires
VERILOGDIR:=./verilog/
BSVBUILDDIR:=./bsv_build/
BSVOUTDIR:=./bin
################################################################################

########## BSIM COMLILE, LINK AND SIMULATE TARGETS #################################
.PHONY: check-restore
check-restore:
	@if [ "$(define_macros)" != "$(old_define_macros)" ];	then	make clean update_xlen ;	fi;

.PHONY: update_xlen
update_xlen:
	@echo "XLEN=$(XLEN)" > verification/dts/Makefile.inc

.PHONY:  compile_bluesim
compile_bluesim: check-restore check-env
	@echo "Compiling $(TOP_MODULE) in Bluesim..."
	@mkdir -p $(BSVBUILDDIR) 
	@echo "old_define_macros = $(define_macros)" > old_vars
	bsc -u -sim -simdir $(BSVBUILDDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR) $(define_macros)\
  $(BSVCOMPILEOPTS) -p $(BSVINCDIR) -g $(TOP_MODULE) $(TOP_DIR)/$(TOP_FILE) 2>&1 | tee\
  bsv_compile.log || (echo "ERROR: BSC COMPILE ERROR"; exit 1)
	@echo "Compilation finished"

.PHONY: link_bluesim
link_bluesim:check-env
	@echo "Linking $(TOP_MODULE) in Bluesim..."
	@mkdir -p $(BSVOUTDIR)
	bsc -e $(TOP_MODULE) -sim -o $(BSVOUTDIR)/out -simdir $(BSVBUILDDIR) -p $(BSVINCDIR) -bdir\
  $(BSVBUILDDIR) $(BSVLINKOPTS) 2>&1 | tee bsv_link.log || (echo "ERROR: BSC LINK ERROR"; exit 1)
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	@exec ./$(BSVOUTDIR)/out
	@echo Simulation finished
########################################################################################

.PHONY: generate_verilog 
generate_verilog: check-restore check-env 
	@echo Compiling $(TOP_MODULE) in verilog ...
	@mkdir -p $(BSVBUILDDIR); 
	@mkdir -p $(VERILOGDIR); 
	@echo "old_define_macros = $(define_macros)" > old_vars
	@bsc -u -verilog -elab -vdir $(VERILOGDIR) -bdir $(BSVBUILDDIR) -info-dir $(BSVBUILDDIR)\
  $(define_macros) -D verilog=True $(BSVCOMPILEOPTS) -verilog-filter ${BLUESPECDIR}/bin/basicinout\
  -p $(BSVINCDIR) -g $(TOP_MODULE) $(TOP_DIR)/$(TOP_FILE)  || (echo "BSC COMPILE ERROR"; exit 1) 
	@cp ${BLUESPECDIR}/Verilog.Vivado/RegFile.v ./verilog/  
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM1Load.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog.Vivado/BRAM2BELoad.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO2.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFO20.v ./verilog/
	@cp ${BLUESPECDIR}/Verilog/FIFOL1.v ./verilog/
ifeq ($(SYNTH), SIM)
		@cp fpga/manage_ip/manage_ip.srcs/sources_1/ip/multiplier/multiplier_sim_netlist.v\
  	./verilog/multiplier.v || (echo "ERROR: PLEASE BUILD VIVADO IP FIRST"; exit 1)
		@cp fpga/manage_ip/manage_ip.srcs/sources_1/ip/divider/divider_sim_netlist.v\
  	./verilog/divider.v || (echo "ERROR: PLEASE BUILD VIVADO IP FIRST"; exit 1)
endif
	@echo Compilation finished

.PHONY: link_vcs
link_vcs: 
	@mkdir -p bin
	@rm -rf bin/*
	@vcs -full64 -l vcs_compile.log -sverilog +vpi +v2k -lca +define+TOP=$(TOP_MODULE) \
	+define+BSV_TIMESCALE=1ns/1ps +cli+4 +libext+.v +notimingcheck +vcs+dumpvars+test.vcd \
  ${XILINX_VIVADO}/data/verilog/src/glbl.v \
	-y $(VERILOGDIR)/ -y ${BLUESPECDIR}/Verilog/ \
	-y ${XILINX_VIVADO}/data/verilog/src/unisims +libext+.v \
	-y ${XILINX_VIVADO}/data/verilog/src/unimacro +libext+.v \
	-y ${XILINX_VIVADO}/data/verilog/src/retarget +libext+.v \
	${BLUESPECDIR}/Verilog/main.v -o out
	@mv csrc out* bin

.PHONY: link_ncverilog
link_ncverilog: 
	@echo "Linking $(TOP_MODULE) using ncverilog..."
	@rm -rf work include bin/work
	@mkdir -p bin 
	@mkdir work
	@echo "define work ./work" > cds.lib
	@echo "define WORK work" > hdl.var
	@ncvlog -sv -cdslib ./cds.lib -hdlvar ./hdl.var +define+TOP=$(TOP_MODULE) ${BLUESPECDIR}/Verilog/main.v -y ./$(VERILOGDIR)/ -y ${BLUESPECDIR}/Verilog/ -y ./src/bfm
	@ncelab  -cdslib ./cds.lib -hdlvar ./hdl.var work.main -timescale 1ns/1ps
	@echo 'ncsim -cdslib ./cds.lib -hdlvar ./hdl.var work.main #> /dev/null' > $(BSVOUTDIR)/out
	@mv work cds.lib hdl.var $(BSVOUTDIR)/
	@chmod +x $(BSVOUTDIR)/out
	@echo Linking finished

.PHONY: link_msim
link_msim: 
	@echo "Linking $(TOP_MODULE) using modelsim..."
	@rm -rf work* bin/*
	@mkdir -p bin 
	vlib work
	vlog -work work +libext+.v+.vqm -y ./src/bfm -y $(VERILOGDIR) -y ${BLUESPECDIR}/Verilog +define+TOP=$(TOP_MODULE) ${BLUESPECDIR}/Verilog/main.v ./$(VERILOGDIR)/$(TOP_MODULE).v  > compile_log
	mv compile_log ./$(BSVOUTDIR)
	mv work ./$(BSVOUTDIR)
	echo 'vsim -quiet -novopt -lib work -do "run -all; quit" -c main' > $(BSVOUTDIR)/out
	@chmod +x $(BSVOUTDIR)/out
	@echo Linking finished


.PHONY: link_verilator
link_verilator: 
	@echo "Linking $(TOP_MODULE) using verilator"
	@mkdir -p bin
	@verilator $(VERILATOR_FLAGS) -I$(VERILOGDIR) -y $(VERILOGDIR) -DBSV_TIMESCAL=1na/1ps -DTOP=$(TOP_MODULE) ${BLUESPECDIR}/Verilog/main.v -o out
	@mv out bin/

.PHONY: link_iverilog
link_iverilog: 
	@echo "Linking $(TOP_MODULE) using iverilog..."
	@mkdir -p bin 
	@iverilog -v -o bin/out -Wall -y ./src/bfm -y $(VERILOGDIR) -y ${BLUESPECDIR}/Verilog/ -DTOP=$(TOP_MODULE) ${BLUESPECDIR}/Verilog/main.v .$(VERILOGDIR)/$(TOP_MODULE).v
	@echo Linking finished

.PHONY: ip_build
ip_build: 
	@vivado -mode tcl -notrace -source src/tcl/create_ip_project.tcl -tclargs $(FPGA) || (echo "Could \
not create IP project"; exit 1)
	@vivado -mode tcl -notrace -source src/tcl/create_multiplier.tcl -tclargs $(XLEN) $(MULSTAGES) ||\
(echo "Could not create Multiplier IP"; exit 1)
	@vivado -mode tcl -notrace -source src/tcl/create_divider.tcl -tclargs $(XLEN) $(DIVSTAGES) ||\
(echo "Could not create Divider IP"; exit 1)

.PHONY: vivado_build
vivado_build: 
	@vivado -mode tcl -source src/tcl/create_project.tcl -tclargs $(TOP_MODULE) $(FPGA) || (echo "Could \
not create core project"; exit 1)
	@vivado -mode tcl -source src/tcl/run.tcl || (echo "ERROR: While running synthesis")

.PHONY: regress 
regress: compile_bluesim link_bluesim generate_boot_files 
	SHAKTI_HOME=$$PWD perl -I$(SHAKTI_HOME)/verification/scripts $(SHAKTI_HOME)/verification/scripts/makeRegress.pl $(opts)

.PHONY: test
test: compile_bluesim link_bluesim generate_boot_files 
	SHAKTI_HOME=$$PWD perl -I$(SHAKTI_HOME)/verification/scripts $(SHAKTI_HOME)/verification/scripts/makeTest.pl $(opts)

.PHONY: torture
torture: compile_bluesim link_bluesim generate_boot_files 
	SHAKTI_HOME=$$PWD perl -I$(SHAKTI_HOME)/verification/scripts $(SHAKTI_HOME)/verification/scripts/makeTorture.pl $(opts)


.PHONY: generate_boot_files
generate_boot_files:
	@mkdir -p bin
	@cd verification/dts/; make;
	@cut -c1-8 verification/dts/boot.hex > bin/boot.MSB
	@cut -c9-16 verification/dts/boot.hex > bin/boot.LSB

.PHONY: clean
clean:
	rm -rf $(BSVBUILDDIR) *.log $(BSVOUTDIR)
	rm -f *.jou rm *.log
	rm -rf verification/workdir/*

clean_verilog: clean 
	rm -rf verilog/
	rm -rf fpga/


restore: clean_verilog
