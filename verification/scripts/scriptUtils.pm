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

package scriptUtils;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT = qw(systemCmd systemFileCmd systemKillCmd 
                 doClean doPrint doDebugPrint printHelp
                 checkSetup openLog closeLog appendLog
                 checkBins
                 $scriptLog $shaktiHome $workdir
                 );

our $scriptLog = `basename $0 .pl`; chomp($scriptLog);
our $shaktiHome;
our $workdir;

sub checkSetup {
  if (defined $ENV{'SHAKTI_E_HOME'}) {
    $shaktiHome = $ENV{'SHAKTI_E_HOME'};
    doDebugPrint("SHAKTI_E_HOME: $shaktiHome\n");
  }
  else {
    doPrint("ERROR: SHAKTI_E_HOME not defined\n");
    exit(1);
  }

  # create temporary directory where all outputs are generated
  $workdir = "$shaktiHome/verification/workdir";
  unless (-e $workdir or mkdir $workdir) {
    doPrint("ERROR: Unable to create workdir!\n");
    exit(1);
  }
  appendLog("$workdir/$scriptLog.log");

}

sub checkBins {
  my $out = "$shaktiHome/bin/out";
  my $boot = "$shaktiHome/bin/boot.MSB";

  unless (-e $out) {
    doPrint("ERROR: bin/out not present! [option: make complile_bluesim; make link_bluesim\n");
    exit(1);
  }
  
  unless (-e $boot) {
    doPrint("ERROR: Boot files not present! [option: make generate_boot_files]\n");
    exit(1);
  }

}

sub openLog {
  my @file = @_;
  open LOG, ">$file[0]" or die "[$scriptLog.pl] ERROR opening file $!\n";
}

sub appendLog {
  my @file = @_;
  open LOG, ">>$file[0]" or die "[$scriptLog.pl] ERROR opening file $!\n";
}

sub closeLog {
  close LOG;
}
#-----------------------------------------------------------
# systemCmd
# Runs and displays the command line, exits on error
#-----------------------------------------------------------
sub systemCmd {
  my (@cmd) = @_;
  chomp(@cmd);
  doDebugPrint("'$cmd[0]'\n");
  my $ret = system("@cmd 2>> $workdir/$scriptLog.log >> $workdir/$scriptLog.log");
  #my $ret = system("@cmd |& tee -a $pwd/$script.log");
  if ($ret) {
    if ($cmd[0] =~ /^riscv.*-unknown-elf-gcc/) { `touch COMPILE_FAIL`};
    if ($cmd[0] =~ /^spike/) { `touch MODEL_FAIL`};
    if ($cmd[0] =~ /sdiff -W rtl.dump spike.dump/) { `touch RTL_FAIL`};
    if ($cmd[0] =~ /timeout.*out/) { `touch RTL_TIMEOUT`};
    doPrint("ERROR: While running '@cmd'\n\n");  
    exit(1);
  }
}

#-----------------------------------------------------------
# systemCmd
# Runs and displays the command line, exits on error
#-----------------------------------------------------------
sub systemFileCmd {
  my (@cmd) = @_;
  my @sysOut;
  my $ret;

  chomp(@cmd);
  
  if ($cmd[1]) {
    doDebugPrint("'$cmd[0] > $cmd[1]'\n");
    @sysOut = `$cmd[0]`;
    $ret = $?;
  }
  else {
    doDebugPrint("'$cmd[0]'\n");
    $ret = system("@cmd 2>> $workdir/$scriptLog.log >> $workdir/$scriptLog.log");
  }
  if ($ret) {
    if ($cmd[0] =~ /^riscv.*-unknown-elf-gcc/) { `touch COMPILE_FAIL`};
    if ($cmd[0] =~ /^spike/) { `touch MODEL_FAIL`};
    if ($cmd[0] =~ /sdiff -W rtl.dump spike.dump/) { `touch RTL_FAIL`};
    if ($cmd[0] =~ /timeout.*out/) { `touch RTL_TIMEOUT`};
    doPrint("ERROR: Running '@cmd'\n\n");  
    exit(1);
  }
  else {
    if ($cmd[1]) {
      open FILE, ">$cmd[1]";
      print FILE @sysOut;
      close FILE;
    }
  }
  #my $ret = system("@cmd 2>> $workdir/$scriptLog.log >> $workdir/$scriptLog.log");
  #my $ret = system("@cmd |& tee -a $pwd/$script.log");
  #if ($ret) {
  #  die("[$scriptLog.pl] ERROR Running: '@cmd'\n\n");  
  #}
}

#-----------------------------------------------------------
# systemKillCmd
# Runs the system command kills it after timeout value
#-----------------------------------------------------------
sub systemKillCmd {
#  my (@cmd) = @_;
#  chomp(@cmd);
#  doDebugPrint("'$cmd[0] > $cmd[1]'\n");
#
#  my $exited_cleanly;                 #to this variable I will save the info about exiting
#
#  my $pid = fork;
#  if (!$pid) {
#    system("$cmd[0]");        #your long program 
#    #systemFileCmd(@cmd);
#  } 
#  else {
#    sleep 20;                           #wait 10 seconds (can be longer)
#    my $result = waitpid(-1, WNOHANG);  #here will be the result
#    if ($result==0) {                   #system is still running
#      $exited_cleanly = 0;            #I already know I had to kill it
#      kill('TERM', $pid);             #kill it with TERM ("cleaner") first
#      sleep(1);                       #wait a bit if it ends
#      my $result_term = waitpid(-1, WNOHANG);
#                                         #did it end?
#
#      if ($result_term == 0) {        #if it still didnt...
#        kill('KILL', $pid);         #kill it with full force!
#      }  
#      print "Killing : $pid\n";
#    } 
#    else {
#      $exited_cleanly = 1;            #it exited cleanly
#    }  
#  }
#
#  ##you can now say something to the user, for example
  #if ($exited_cleanly) { 
  #  print "Done";
  #}
  #else {
  #  print "ERROR: Timeout @cmd \n";
  #  exit(0);
  #}

}
#-----------------------------------------------------------
# doClean
# Deletes generated output
#------------------------------------------------------------
sub doClean {
  if ($scriptLog =~ /^makeRegress$/) {
    doPrint("Cleaning...\n");
    systemCmd("rm -rf $shaktiHome/verification/workdir/*");
    systemCmd("rm -rf $shaktiHome/verification/scripts/nohup.out");
    systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/nohup.out");
    systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/__pycache__");
    systemCmd("rm -rf $shaktiHome/verification/tests/random/*/generated_tests/*");
    systemFileCmd("perl -ne 'print unless /riscv-torture/ || print' $shaktiHome/verification/scripts/tests.list","$shaktiHome/verification/scripts/tests.list");
    systemFileCmd("perl -ne 'print unless /aapg/ || print' $shaktiHome/verification/scripts/tests.list","$shaktiHome/verification/scripts/tests.list");
  }
  elsif ($scriptLog =~ /^makeTorture$/) {
    doPrint("Cleaning...\n");
    systemCmd("rm -rf $shaktiHome/verification/tests/random/riscv-torture/generated_tests/*");
    systemFileCmd("perl -ne 'print unless /riscv-torture/ || print' $shaktiHome/verification/scripts/tests.list","$shaktiHome/verification/scripts/tests.list");
  }
  elsif ($scriptLog =~ /^makeAapg/) {
    doPrint("Cleaning...\n");
    systemCmd("rm -rf $shaktiHome/verification/tests/random/aapg/generated_tests/*");
    systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/nohup.out");
    systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/__pycache__");
    systemFileCmd("perl -ne 'print unless /aapg/ || print' $shaktiHome/verification/scripts/tests.list","$shaktiHome/verification/scripts/tests.list");
  }
}

#-----------------------------------------------------------
# doPrint
# Prints message
#------------------------------------------------------------
sub doPrint {
  my @msg = @_;
  print "[$scriptLog.pl] @msg";
  print LOG "[$scriptLog.pl] @msg";
}

#-----------------------------------------------------------
# doDebugPrint
# Prints message to help debug
#------------------------------------------------------------
sub doDebugPrint {
  my @msg = @_;
  if (testRunConfig::getConfig("CONFIG_LOG")) {
    print "[$scriptLog.pl] @msg";
    print LOG "[$scriptLog.pl] @msg";
  }
}

#-----------------------------------------------------------
# printHelp
# Displays script usage
#------------------------------------------------------------
sub printHelp {
  if ($scriptLog =~ /^makeRegress$/) {
    my $usage =<<USAGE;

Description: Runs regression
Usage: perl makeRegress.pl [OPTIONS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make regress opts="[OPTIONS]"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Options:
  --submit                      Simulates all the tests in the test list
  --compile                     Compiles RTL 
  --generate                    Generates random tests
  --final                       Displays regression result by waiting till all simulation is done
  --list=TEST_LIST              Tests in the TEST_LIST file are used for regression
                                [default] --list=tests.list
  --suite=TEST_SUITE            Specified along with --generate for a specific random generation
                                [default] --suite=all
                                --suite=[all|riscv-torture|aapg]
  --filter=FILTER_PATTERN       Filters the test list based on FILTER_PATTERN
  --result=RESULT_PATTERN       Filters the regression report based on RESULT_PATTERN
                                Eg. --res=[pass|fail|timeout|compile_fail]
  --test_count=TEST_COUNT       Generates TEST_COUNT value of random tests for each config 
                                [default] --test_count=1
  --parallel                    Specified along with --submit for running tests in parallel
  --report                      Displays regression report with percentages 
  --help                        Displays this help
  --clean                       Cleans all regression work directory
USAGE

    print $usage;
  }
  if ($scriptLog =~ /^makeTest$/) {
    my $usage =<<USAGE;

Description: Compiles test
Usage: perl makeTest.pl --test=TEST_NAME --suite=TEST_SUITE [OPTIONS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make test opts="--test=TEST_NAME --suite=TEST_SUITE [OPTIONS]"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Options:
  --test=TEST_NAME              Test name 
  --suite=TEST_SUITE            Test Suite, directory path that uniquely identifies the test
                                starting from \$SHAKTI_C_HOME/verification/tests
                                Examples:
                                        directed/riscv-tests/isa/rv64ui 
                                        aapg
                                        riscv-torture
  --sim=SIMULATOR               Specifies the simulator
                                [default] --sim=bluespec
                                --sim=[bluespec|ncverilog|vcs]
  --type=TEST_TYPE              Specifies the test type
                                [default] --type=p
                                --type=[p|v]
  --nodebug                     Forces no debug messages to be printed
  --help                        Displays this help
  --clean                       Cleans all regression work directory
USAGE

    print $usage;
  }
  if ($scriptLog =~ /^makeTorture$/) {
    my $usage =<<USAGE;

Description: Generates riscv-torture test
Usage: perl makeTorture.pl [OPTIONS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make torture opts="[OPTIONS]"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Options:
  --config=TEST_CONFIG          Config used for generation
                                [default] --config=bringup
  --list_configs                Lists available configs
  --test_count=TEST_COUNT       Generates TEST_COUNT number of tests
                                [default] --test_count=1
  --submit                      Generates and simulates a single riscv-torture test
  --nodebug                     Forces no debug messages to be printed
  --parallel                    Test generation runs in parallel
  --help                        Displays this help
  --clean                       Cleans all regression work directory
USAGE

    print $usage;
  }
  if ($scriptLog =~ /^makeAapg$/) {
    my $usage =<<USAGE;

Description: Generates AAPG tests
Usage: perl makeAapg.pl [OPTIONS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make aapg opts="[OPTIONS]"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Options:
  --config=TEST_CONFIG          Config used for generation
                                [default] --config=bringup
  --list_configs                Lists available configs
  --test_count=TEST_COUNT       Generates TEST_COUNT number of tests
                                [default] --test_count=1
  --submit                      Generates and simulates a single AAPG test
  --nodebug                     [TODO] Forces no debug messages to be printed
  --parallel                    [TODO] Test generation runs in parallel
  --help                        Displays this help
  --clean                       Cleans all regression work directory
USAGE

    print $usage;
  }
}


1;
