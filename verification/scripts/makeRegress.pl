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
# makeRegress.pl
# Generates test related files
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use testRunConfig; 
use scriptUtils;
use List::Util;

checkSetup();
setEnvConfig();

my $simulator = getConfig("CONFIG_SIM");


# Parse options
GetOptions(
          qw(submit)        => \my $submit_tests,
          qw(compile)       => \my $src_compile,
          qw(generate)      => \my $generate_tests,
          qw(final)         => \my $final_report,
          qw(list=s)        => \my $test_list,
          qw(suite=s)       => \my $test_suite,
          qw(filter=s)      => \my $test_filter,
          qw(result=s)      => \my $result_filter,
          qw(test_count=s)  => \my $test_count,
          qw(parallel=s)      => \my $parallel,
          qw(report)        => \my $report,
          qw(help)          => \my $help,
          qw(clean)         => \my $clean
);

my $submit;
my $testSuite;
my $testCount;
my $generate;
my $testList;
my $filter;
my $compile;
my $finalReport;

# test submission if not given, we simply report
if ($submit_tests) {
  $submit = 1;
}
else {
  $submit = 0;
}

if ($final_report) {
  $finalReport = 1;
}
else {
  $finalReport = 0;
}

if ($src_compile) {
  $compile = 1;
}
else {
  $compile = 0;
}

if (!$test_list) {
  $testList = "$shaktiHome/verification/scripts/tests.list";
}
else {
  $testList = "$shaktiHome/verification/scripts/$test_list";
}

if ($generate_tests) {
  $generate = 1;
}
else {
  $generate = 0;
}

if (!$test_filter) {
  $filter = "";
}
else {
  $filter = $test_filter;
}

# Test suite
if ($test_suite) {
  $testSuite = $test_suite;
}
else {
  $testSuite = "all"; 
}

# Test count
if ($test_count) {
  $testCount = $test_count;
}
else {
  my $count = $ENV{'CONFIG_GEN_COUNT'};
  if ($count) {
    $testCount = $count;
  }
  else {
    $testCount = 1;
  }
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
else {
  checkBins();
}

doPrint("Regression run ------------\n");
my $MaxCount = 0;
my $count = 0;
if ($generate) {
  chdir("$shaktiHome/verification/scripts");
  systemCmd("rm -rf $shaktiHome/verification/tests/random/riscv-torture/generated_tests/*");
  systemCmd("rm -rf $shaktiHome/verification/tests/random/aapg/generated_tests/*");
  if ($testSuite =~ /^all$/ || $testSuite=~ /^aapg$/) {
    my @configs = `ls $shaktiHome/verification/tests/random/aapg/configs/*.py`;
    $MaxCount = $MaxCount + scalar(@configs);
    systemCmd("perl makeAapg.pl --config=all --test_count=$testCount&");
  }
  if ($testSuite =~ /^all$/ || $testSuite=~ /^riscv-torture$/) {
    my @configs = `ls $shaktiHome/verification/tests/random/riscv-torture/configs/*.config`;
    $MaxCount = $MaxCount + scalar(@configs);
    systemCmd("perl makeTorture.pl --config=all --test_count=$testCount");
  }
  $MaxCount = $MaxCount * $testCount;
  $count = `find $shaktiHome/verification/tests/random/ -name "*.S" | wc -l`;
  doPrint("Test count to be generated = $MaxCount | Current count = $count\n");
  if (($testSuite =~ /^all$/) || ($testSuite=~ /^riscv-torture$/) || ($testSuite =~ /^aapg$/)) {
    my $timeout = 0;
    while ($count != $MaxCount) {
      sleep(10);
      $count = `find $shaktiHome/verification/tests/random/ -name "*.S" | wc -l`;
      chomp($count);
      doDebugPrint("Current test count = $count\n");
      $timeout++;
      if ($timeout == 300) {
        last;
      }
    }
  }
  open TESTLIST, "$shaktiHome/verification/scripts/riscv-tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
  my @listFile = <TESTLIST>;
  close TESTLIST;
  my @genTests = `find  $shaktiHome/verification/tests/random/riscv-torture -name "*.S"`;
  chomp(@genTests);
  foreach my $test (@genTests) {
    my $file = `basename $test .S`; chomp($file);
    my $suite = `dirname $test`; chomp($suite);
    $suite = substr($suite, index($suite, "random/"));
    push @listFile, "$file\t\t\t\t$suite\t\t\t\tp\n";
  }
  open TESTLIST, ">$shaktiHome/verification/scripts/tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
  print TESTLIST @listFile;
  close TESTLIST;
}
####### src compilation
if ($compile) {
  if ($simulator =~ /^bluespec$/) {
    chdir($shaktiHome);
    doDebugPrint("cd $shaktiHome\n");
    systemCmd("make restore");
    systemCmd("make compile_bluesim");
    systemCmd("make link_bluesim");
    systemCmd("make generate_boot_files");
  }
  elsif ($simulator =~ /^ncverilog$/) {
    chdir($shaktiHome);
    doDebugPrint("cd $shaktiHome\n");
    systemCmd("make restore");
    systemCmd("make generate_verilog ");
    systemCmd("make compile_ncverilog");
    systemCmd("make link_ncverilog");
    systemCmd("make generate_boot_files");
  }
}

#######
# Process tests.list -----------
open TESTLIST, "$testList" or die "[$scriptLog.pl] ERROR opening file $!\n";
my @listFile = <TESTLIST>;
close TESTLIST;

my @testList = ();

foreach my $line (@listFile) { # remove unwanted white space
  if ($line !~ /^\s*\#/) {
    if ($line !~ /^\s*$/) {
      my @line = split(" ", $line);
      chomp(@line);
      $line = join(" ", @line);
      push @testList, $line;
    }
  }
}

if ($testSuite !~ /^all$/) {
  if ($testSuite =~ /^riscv-torture$/) {
    @testList = ();
    my @genTests = `find  $shaktiHome/verification/tests/random/riscv-torture -name "*.S"`;
    chomp(@genTests);
    foreach my $test (@genTests) {
      my $file = `basename $test .S`; chomp($file);
      my $suite = `dirname $test`; chomp($suite);
      $suite = substr($suite, index($suite, "random/"));
      push @testList, "$file\t\t\t\t$suite\t\t\t\tp\n";
    }
  }
  elsif ($testSuite =~ /^aapg$/) {
    @testList = ();
    my @genTests = `find  $shaktiHome/verification/tests/random/aapg -name "*.S"`;
    chomp(@genTests);
    foreach my $test (@genTests) {
      my $file = `basename $test .S`; chomp($file);
      my $suite = `dirname $test`; chomp($suite);
      $suite = substr($suite, index($suite, "random/"));
      push @testList, "$file\t\t\t\t$suite\t\t\t\tp\n";
    }
  }
}
else {
    my @genTests = `find  $shaktiHome/verification/tests/random/riscv-torture -name "*.S"`;
    chomp(@genTests);
    foreach my $test (@genTests) {
      my $file = `basename $test .S`; chomp($file);
      my $suite = `dirname $test`; chomp($suite);
      $suite = substr($suite, index($suite, "random/"));
      push @testList, "$file\t\t\t\t$suite\t\t\t\tp\n";
    }
    @genTests = `find  $shaktiHome/verification/tests/random/aapg -name "*.S"`;
    chomp(@genTests);
    foreach my $test (@genTests) {
      my $file = `basename $test .S`; chomp($file);
      my $suite = `dirname $test`; chomp($suite);
      $suite = substr($suite, index($suite, "random/"));
      push @testList, "$file\t\t\t\t$suite\t\t\t\tp\n";
    }
}

if ($filter) {
  @testList = grep /$filter/, @testList;
}
# run the tests 
if ($submit) {
  my $totalTests = scalar @testList;
  my $licenseCount = $parallel;
  my @tempTests = @testList;
  my $count = 0;
  my $refreshCount = 0;

  while (@tempTests) {
    my $line = shift @tempTests;
    my @line = split(" ", $line);
    my $test = $line[0];
    my $tSuite = $line[1];
    my $pv = $line[2];
    unless (-e $workdir or mkdir -p $workdir/$tSuite) {
      die "ERROR: Unable to create workdir, check SHAKTI_C_HOME is set!\n";
    }
    if ($parallel) {
      system("nohup perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv --sim=$simulator &");
    }
    else {
      system("perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv --sim=$simulator");
      sleep(2);
    }
    #print "$test\t\t\t$tSuite\t\t\t$pv\t\t\tRunning\n";
    $refreshCount++;
    if ($refreshCount == $licenseCount) {
      if ($parallel) {
        sleep(60);
      }
      else {
        sleep(6);
      }
      $refreshCount=0;
    }
  }
}
elsif ($finalReport) { # waits till all the test results are there/timesout
  my %testResults = ();
  my @regress_report = ();
  my $timeout = 0; 
  my $passCount = 0;

  foreach my $line (@testList) {
    $testResults{$line} = "NOT_RUN";
  }
  while (keys %testResults) {
    foreach my $line (keys %testResults) {
      my @line = split(" ", $line);
      my $test = $line[0];
      my $tSuite = $line[1];
      my $pv = $line[2];
      my $pass = "$workdir/$tSuite/$pv/$test/PASSED";
      my $fail = "$workdir/$tSuite/$pv/$test/FAILED";
      my $compile_fail = "$workdir/$tSuite/$pv/$test/COMPILE_FAIL";
      my $model_fail = "$workdir/$tSuite/$pv/$test/MODEL_FAIL";
      my $rtl_fail = "$workdir/$tSuite/$pv/$test/RTL_FAIL";
      my $rtl_timeout = "$workdir/$tSuite/$pv/$test/RTL_TIMEOUT";
      my $result;
      my $delete=0;

      if (-e $compile_fail) {
        $result = sprintf("%40s %40s %5s    COMPILE_FAIL\n", $test, $tSuite, $pv);
        $delete = 1;
      }
      elsif (-e $model_fail) {
        $result = sprintf("%40s %40s %5s    MODEL_FAIL\n", $test, $tSuite, $pv);
        $delete = 1;
      }
      elsif (-e $rtl_fail) {
        $result = sprintf("%40s %40s %5s    RTL_FAIL\n", $test, $tSuite, $pv);
        $delete = 1;
      }
      elsif (-e $rtl_timeout) {
        $result = sprintf("%40s %40s %5s    RTL_TIMEOUT\n", $test, $tSuite, $pv);
        $delete = 1;
      }
      elsif (-e $fail) {
        $result = sprintf("%40s %40s %5s    FAILED\n", $test, $tSuite, $pv);
        $delete = 1;
      }
      elsif (-e $pass) {
        $result = sprintf("%40s %40s %5s    PASSED\n", $test, $tSuite, $pv);
        $delete = 1;
        $passCount++;
      }
      else {
        $result = sprintf("%40s %40s %5s    NOT_RUN\n", $test, $tSuite, $pv);
        $delete = 0;
      }
      if ($delete) {
        push @regress_report, $result;
        delete $testResults{$line};
      }
    } # end of foreach
    sleep(5);
    $timeout++;
    if ($timeout == 300) {
      last;
    }
  } # end of while
  if (keys %testResults) {
    print @regress_report;
    # TODO:print NOT_RUN tests
  }
  else {
    print @regress_report;
    if ($passCount == @testList) {
      exit(0);
      `touch $workdir/REGRESS_PASS`;
    }
    else {
      exit(1);
      `touch $workdir/REGRESS_FAIL`;
    }
  }
}
else {
  my @regress_report = ();
  my %result = ();
  foreach my $line (@testList) {
    my @line = split(" ", $line);
    my $test = $line[0];
    my $tSuite = $line[1];
    my $pv = $line[2];
    my $pass = "$workdir/$tSuite/$pv/$test/PASSED";
    my $fail = "$workdir/$tSuite/$pv/$test/FAILED";
    my $compile_fail = "$workdir/$tSuite/$pv/$test/COMPILE_FAIL";
    my $model_fail = "$workdir/$tSuite/$pv/$test/MODEL_FAIL";
    my $rtl_fail = "$workdir/$tSuite/$pv/$test/RTL_FAIL";
    my $rtl_timeout = "$workdir/$tSuite/$pv/$test/RTL_TIMEOUT";
    my $result;

    my $test_info=sprintf("%40s %40s %5s", $test, $tSuite, $pv);
    if (-e $compile_fail) {
      $result = sprintf("%40s %40s %5s    COMPILE_FAIL\n", $test, $tSuite, $pv);
      $result{$test_info}="COMPILE_FAIL";
    }
    elsif (-e $model_fail) {
      $result = sprintf("%40s %40s %5s    MODEL_FAIL\n", $test, $tSuite, $pv);
      $result{$test_info}="MODEL_FAIL";
    }
    elsif (-e $rtl_fail) {
      $result = sprintf("%40s %40s %5s    RTL_FAIL\n", $test, $tSuite, $pv);
      $result{$test_info}="RTL_FAIL";
    }
    elsif (-e $rtl_timeout) {
      $result = sprintf("%40s %40s %5s    RTL_TIMEOUT\n", $test, $tSuite, $pv);
      $result{$test_info}="RTL_TIMEOUT";
    }
    elsif (-e $fail) {
      $result = sprintf("%40s %40s %5s    FAILED\n", $test, $tSuite, $pv);
      $result{$test_info}="FAILED";
    }
    elsif (-e $pass) {
      $result = sprintf("%40s %40s %5s    PASSED\n", $test, $tSuite, $pv);
      $result{$test_info}="PASSED";
    }
    else {
      $result = sprintf("%40s %40s %5s    NOT_RUN\n", $test, $tSuite, $pv);
      $result{$test_info}="NOT_RUN";
    }
    push @regress_report, $result;
  }
  if ($result_filter) {
    @regress_report = grep /$result_filter/i, @regress_report;
  }
  print @regress_report;
  if ($report) {
    my @resultValues = values %result;
    open REPORT, ">$shaktiHome/verification/workdir/regress_report.log" or die "[$scriptLog.pl] ERROR opening file $!\n";
    print REPORT "#------------------------------------------------------------------------------------------------\n";
    print REPORT "# Total tests      :", scalar(@regress_report),"\n";
    print REPORT "# Pass percentage  :", sprintf("%.2f\%", (scalar(grep /PASSED/, @regress_report)/scalar(@regress_report))*100),"\n";
    print REPORT "# FAILED           :", scalar(grep /FAILED/, @regress_report),"\n";
    print REPORT "# RTL_FAIL         :", scalar(grep /RTL_FAIL/, @regress_report),"\n";
    print REPORT "# RTL_TIMEOUT      :", scalar(grep /RTL_TIMEOUT/, @regress_report),"\n";
    print REPORT "# COMPILE_FAIL     :", scalar(grep /COMPILE_FAIL/, @regress_report),"\n";
    print REPORT "# MODEL_FAIL       :", scalar(grep /MODEL_FAIL/, @regress_report),"\n";
    print REPORT "# NOT_RUN          :", scalar(grep /NOT_RUN/, @regress_report),"\n";
    print REPORT "# PASSED           :", scalar(grep /PASSED/, @regress_report),"\n";
    print REPORT "#------------------------------------------------------------------------------------------------\n";
    #print REPORT @regress_report;
    print REPORT "$_     $result{$_}\n" foreach(sort{$result{$b} cmp $result{$a}} keys %result);
    close REPORT;
  }
}
