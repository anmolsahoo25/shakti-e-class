# ------------------------------------------------------------------------------------------------- 
# Copyright (c) 2018, IIT Madras All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
# 
# - Redistributions of source code must retain the below copyright notice, this list of conditions
#   and the following disclaimer.  
# - Redistributions in binary form must reproduce the above copyright notice, this list of 
#   conditions and the following disclaimer in the documentation and/or other materials provided 
#   with the distribution.  
# - Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
#   promote products derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
# WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# -------------------------------------------------------------------------------------------------
# Author: Lavanya J
# Email id: lavanya.jagan@gmail.com
# -------------------------------------------------------------------------------------------------
#
#!/usr/bin/perl

#-----------------------------------------------------------
# makeTest.pl
# Generates test related files
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use POSIX ":sys_wait_h";
use testRunConfig;
use scriptUtils;

checkSetup();

# Parse options
GetOptions(
          qw(test=s)  => \my $test_name,
          qw(suite=s) => \my $test_suite,
          qw(sim=s)   => \my $test_sim,
          qw(type=s)  => \my $test_type,
          qw(nodebug) => \my $no_debug,
          qw(notrace) => \my $no_trace,
          qw(help)    => \my $help,
          qw(clean)   => \my $clean
);

if (!$no_debug) {
  testRunConfig::setEnvConfig();
  
}
else {
  testRunConfig::setConfigValue("CONFIG_LOG",0);
}

#my $pwd = $ENV{'PWD'};
#my $scriptLog = `basename $0 .pl`; chomp($scriptLog);
my $riscvIncludeDir = "$shaktiHome/verification/tests/directed/riscv-tests";
my $testPath = "$shaktiHome/verification/tests";

my @isa = `grep ISA= $shaktiHome/soc_config.inc`;
my $XLEN=64;
chomp(@isa);
if (scalar(@isa) == 1) {
  if ($isa[0] =~ /RV32/) {
    $XLEN=32;
  }
  else {
    $XLEN=64;
  }
}
else {
  doPrint("ERROR: ISA undefined in $shaktiHome/soc_config.inc\n");
  exit(1);
}

#my $workdir = "$testPath/workdir";
doDebugPrint("Generating Test Dump Directory ------------\n");

my $testName;
my $testSuite;
my $currentTestPath;
my $test;
my $simulator;
my $testType;
my $trace;

# Test name
if (!$test_name) {
  doPrint("ERROR: Undefined test name\n");
  exit(1);
}
else {
  $testName = $test_name;
}

# Test suite
if (!$test_suite) {
  doPrint("ERROR: Undefined test suite\n");
  exit(1);
}
else {
  $testSuite = "$testPath/$test_suite";
}

# Simulator
if (!$test_sim) {
  $simulator = "bluespec";
}
elsif ($test_sim =~ /^ncverilog$/ || $test_sim =~ /^vcs$/ || $test_sim =~ /^bluespec$/){
  $simulator = $test_sim;
}
else {
  doPrint("ERROR: Invalid simulator, --sim=[bluespec|ncverilog|vcs]\n");
  exit(1);
}

# Type
if (!$test_type) {
  $testType = "p";
}
elsif ($test_type =~ /^p$/ || $test_type =~ /^v$/) {
  $testType = $test_type;
}
else {
  doPrint("ERROR: Invalid test type, --test_type=[p|v]\n");
  exit(1);
}

# trace

if (!$no_trace) {
  $trace = 1;
}
else {
  $trace = 0;
}

# Prints command line usage of script
if ($help) {
  printHelp();
  exit(0);
}

# Clean script generated outputs
if ($clean) {
  doClean();
  exit(0);
}

my @test = ();
if ($testSuite =~ /csmith-run/) {
  systemCmd("find $testSuite -name $testName.c");
  @test = `find $testSuite -name $testName.c`; chomp(@test);
}
else {
  systemCmd("find $testSuite -name $testName.S");
  @test = `find $testSuite -name $testName.S`; chomp(@test);
}
if (@test > 1) {
  doPrint("ERROR: Duplicate test names\n");
  exit(1);
}
else {
  $currentTestPath = `dirname $test[0]`; chomp($currentTestPath);
}
$test = $test[0];
doDebugPrint("Running test: $test_suite/$testName.S\n");

my $testDir = "$workdir/$test_suite/$testType/$testName";
if (-e "$testDir") {
  systemCmd("rm -rf $testDir");
}
systemCmd("mkdir -p $testDir");
chdir($testDir);
# Creating log for each test in the directory itself
closeLog();
openLog("$testDir/$testName.log");
#chdir("$workdir/$test_suite/$testName");
# Compiling the test
if ($testType =~ /^v$/) {
  systemCmd("riscv$XLEN-unknown-elf-gcc -march=rv$XLEN\imac -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -DENTROPY=0x9629af2 -std=gnu99 -O2 -I$riscvIncludeDir/env/v -I$riscvIncludeDir/isa/macros/scalar -T$riscvIncludeDir/env/v/link.ld $riscvIncludeDir/env/v/entry.S $riscvIncludeDir/env/v/*.c $test -o $testName.elf");
}
elsif ($testType =~ /^p$/) {
  if ($testSuite =~ /csmith-run/) {
    my $csmithInc = "$shaktiHome/verification/tools/csmith_run";
    systemCmd("riscv$XLEN-unknown-elf-gcc -march=rv$XLEN\imac  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c -I /tools/csmith/runtime $csmithInc/crt.S -o crt.o");
    systemCmd("riscv$XLEN-unknown-elf-gcc -march=rv$XLEN\imac  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I /tools/csmith/runtime $csmithInc/syscalls_shakti.c -o syscalls.o");
    systemCmd("riscv$XLEN-unknown-elf-gcc -w -Os -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I /tools/csmith/runtime $shaktiHome/verification/tests/$test_suite/$testName.c  -march=rv$XLEN.imac -o $testName.o");
    systemCmd("riscv$XLEN-unknown-elf-gcc -T $csmithInc/link.ld -I /tools/csmith/runtime $testName.o syscalls.o crt.o -static -nostdlib -nostartfiles -lgcc -lm -o $testName.elf");
  }
  elsif ($testSuite =~ /peripherals/) {
    my $periInc = "$shaktiHome/verification/tests/directed/peripherals";
    systemCmd("riscv$XLEN-unknown-elf-gcc -march=rv$XLEN\imac  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c $periInc/common/crt.S -o crt.o");
    systemCmd("riscv$XLEN-unknown-elf-gcc -march=rvi$XLEN\imac  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf  -c $periInc/common/syscalls.c -o syscalls.o");
    systemCmd("riscv$XLEN-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf -I $periInc/i2c/ -I $periInc/qspi/ -I $periInc/dma/ -I $periInc/plic/ -I $periInc/common/ -c $periInc/smoketests/smoke.c -o smoke.o -march=rv$XLEN\imac -lm -lgcc");
    systemCmd("riscv$XLEN-unknown-elf-gcc -T $periInc/common/link.ld smoke.o syscalls.o crt.o -o smoke.elf -static -nostartfiles -lm -lgcc");
  }
  else {
    systemCmd("riscv$XLEN-unknown-elf-gcc -march=rv$XLEN\g  -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -I$riscvIncludeDir/env/p -I$riscvIncludeDir/isa/macros/scalar -T$riscvIncludeDir/env/p/link.ld $test -o $testName.elf");
  }
}

# Generating the disassembly
systemFileCmd("riscv$XLEN-unknown-elf-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data $testName.elf", "$testName.disass");

# Generating hex

if ($XLEN==64) {
  systemFileCmd("elf2hex  8  524288 $testName.elf 2147483648","code.mem");
  systemFileCmd("cut -c1-8 code.mem", "code.mem.MSB");
  systemFileCmd("cut -c9-16 code.mem", "code.mem.LSB");
}
else {
  systemFileCmd("elf2hex  4  524288 $testName.elf 2147483648","code.mem");
  systemFileCmd("cut -c1-8 code.mem", "code.mem.MSB");
  systemFileCmd("cp code.mem code.mem.LSB")
}

if ($testSuite !~ /peripherals/) {
  if ($trace) {
    systemFileCmd("spike -l --isa=RV$XLEN\IMAC $testName.elf 2>&1","$testName\_spike.trace");
  }
  systemCmd("spike -c --isa=RV$XLEN\IMAC $testName.elf");
}
if ($simulator =~ /^bluespec$/) {
  systemCmd("ln -s $shaktiHome/bin/out.so    out.so");
}
systemCmd("ln -s $shaktiHome/bin/out       out");
#if (!(-e "$shaktiHome/bin/boot.MSB")) {
#  systemFileCmd("cut -c1-8 $shaktiHome/verification/dts/boot.hex","$shaktiHome/bin/boot.MSB");
#  systemFileCmd("cut -c9-16 $shaktiHome/verification/dts/boot.hex","$shaktiHome/bin/boot.LSB");
#}
systemCmd("ln -s $shaktiHome/bin/boot.MSB    boot.MSB");
systemCmd("ln -s $shaktiHome/bin/boot.LSB    boot.LSB");
#systemCmd("ln -s $shaktiHome/bin/boot.3l   boot.3l");
#systemCmd("ln -s $shaktiHome/bin/boot.2l   boot.2l");
#systemCmd("ln -s $shaktiHome/bin/boot.1l   boot.1l");
#systemCmd("ln -s $shaktiHome/bin/boot.0l   boot.0l");
#systemCmd("ln -s $shaktiHome/bin/boot.3h   boot.3h");
#systemCmd("ln -s $shaktiHome/bin/boot.2h   boot.2h");
#systemCmd("ln -s $shaktiHome/bin/boot.1h   boot.1h");
#systemCmd("ln -s $shaktiHome/bin/boot.0h   boot.0h");
if ($simulator =~ /^ncverilog$/) {
  systemCmd("ln -s $shaktiHome/bin/work work");
  systemCmd("ln -s $shaktiHome/verilog/cds.lib cds.lib");
  systemCmd("ln -s $shaktiHome/verilog/hdl.var hdl.var");
  systemCmd("ln -s $shaktiHome/verilog/include include");
}
if ($simulator =~ /^vcs$/) {
  systemCmd("ln -s /scratch/lavanya/c-class/bin/csrc csrc");
  systemCmd("ln -s /scratch/lavanya/c-class/bin/out.daidir out.daidir");
}

if ($simulator =~ /^bluespec$/) {
  my $timeout="30m";
  if ($testSuite =~ /riscv-tests/) {
    $timeout="5m";
  }
  elsif ($testSuite =~ /riscv-torture/) {
    $timeout="30m";
  }
  systemFileCmd("timeout $timeout ./out -w","log.txt");
}
elsif ($testSuite =~ /peripherals.*smoke/ && $simulator =~ /^vcs$/) {
    systemCmd("echo 53 > i2c.mem");
    systemFileCmd("timeout 20m ./out -w","log.txt");
}
else {
  systemFileCmd("./out","log.txt");
}
my $result;

if ($testSuite =~ /peripherals.*smoke/) {
  my @diff = `diff -w app_log $shaktiHome/verification/tests/$test_suite/app_log`;
  #print @diff;
  if (@diff) {
    `touch FAILED`;
    $result = "$testName.S | $test_suite | FAILED";
  }
  else {
    `touch PASSED`;
    $result = "$testName.S  | $test_suite | PASSED";
  }
}
else {
  if (!(-e "rtl.dump")) {
    `touch FAILED`;
     $result = "$testName.S | $test_suite | FAILED";
  }
  elsif (!(-e "spike.dump")) {
    `touch FAILED`;
     $result = "$testName.S | $test_suite | FAILED";
  }
  else {
    my @diff = `diff -w rtl.dump spike.dump`;
    #print @diff;
    if (@diff) {
      `touch FAILED`;
      $result = "$testName.S | $test_suite | FAILED";
    }
    else {
      `touch PASSED`;
      $result = "$testName.S  | $test_suite | PASSED";
    }
  }
  systemFileCmd("sdiff -iW rtl.dump spike.dump", "diff");
}
doDebugPrint("---------------------------------------------\n");
doPrint("testDir: $testDir\n");
doPrint("$result\n");
doDebugPrint("---------------------------------------------\n");
closeLog();
appendLog("$workdir/$scriptLog.log");
