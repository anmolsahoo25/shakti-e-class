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

doPrint("Regression run ------------\n");

# Parse options
GetOptions(
          qw(submit)        => \my $submit_tests,
          qw(compile)       => \my $src_compile,
          qw(generate)      => \my $generate_tests,
          qw(final)         => \my $final_report,
          qw(list=s)        => \my $test_list,
          qw(suite=s)       => \my $test_suite,
          qw(filter=s)      => \my $test_filter,
          qw(test_count=s)  => \my $test_count,
          qw(help)          => \my $help,
          qw(clean)         => \my $clean
);

my $submit;
my $report;
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

my $MaxCount = 0;
my $count = 0;
if ($generate) {
  systemCmd("rm -rf $shaktiHome/verification/tests/random/riscv-torture/generated_tests/*");
  systemCmd("rm -rf $shaktiHome/verification/tests/random/aapg/generated_tests/*");
  #if ($testSuite =~ /^all$/ || $testSuite=~ /^aapg$/) {
  if ($testSuite=~ /^aapg$/) {
    systemCmd("perl -pi -e 's/numberOfTests=.*/numberOfTests=$testCount/' $shaktiHome/verification/tests/random/aapg/configs/*.py");
    my @configs = `ls $shaktiHome/verification/tests/random/aapg/configs/*.py`;
    $MaxCount = $MaxCount + scalar(@configs);
    chomp(@configs);
    foreach my $config (@configs) {
      systemCmd("cp $config $shaktiHome/verification/tools/AAPG/config.py");
      chdir("$shaktiHome/verification/tools/AAPG");
      $config = `basename $config .py`; chomp($config);
      systemCmd("nohup ./regress.py gen_only $config nodebug&");
    }
  }
  chdir("$shaktiHome/verification/scripts");
  if ($testSuite =~ /^all$/ || $testSuite=~ /^riscv-torture$/) {
    my @configs = `ls $shaktiHome/verification/tests/random/riscv-torture/configs/*.config`;
    $MaxCount = $MaxCount + scalar(@configs);
    chomp(@configs);
    foreach my $config (@configs) {
      $config = `basename $config .config`; chomp($config);
      systemCmd("perl makeTorture.pl --test_config=$config --test_count=$testCount");
    }
  }
  $MaxCount = $MaxCount * $testCount;
  if ($testSuite =~ /^all$/ || $testSuite=~ /^riscv-torture$/) {
    my $timeout = 0;
    while ($count != $MaxCount) {
      sleep(5);
      $count = `find $shaktiHome/verification/tests/random/ -name "*.S" | wc -l`;
      chomp($count);
      $timeout++;
      if ($timeout == 300) {
        last;
      }
    }
  }
  doPrint("test count = $count\n");
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
  if ($simulator == 0) {
    chdir($shaktiHome);
    doDebugPrint("cd $shaktiHome\n");
    systemCmd("make restore");
    systemCmd("make compile_bluesim");
    systemCmd("make link_bluesim");
    systemCmd("make generate_boot_files");
  }
  elsif ($simulator == 1) {
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
  foreach my $line (@testList) {
    my @line = split(" ", $line);
    my $test = $line[0];
    my $tSuite = $line[1];
    my $pv = $line[2];
    unless (-e $workdir or mkdir -p $workdir/$tSuite) {
      die "ERROR: Unable to create workdir, check SHAKTI_HOME is set!\n";
    }
    system("nohup perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv --sim=$simulator &");
    print "$test\t\t\t$tSuite\t\t\t$pv\t\t\tRunning\n";
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
      my $result;
      my $delete=0;

      if (-e $compile_fail) {
        $result = sprintf("%30s %20s %5s    COMPILE_FAIL\n", $tSuite, $test, $pv);
        $delete = 1;
      }
      elsif (-e $model_fail) {
        $result = sprintf("%30s %20s %5s    MODEL_FAIL\n", $tSuite, $test, $pv);
        $delete = 1;
      }
      elsif (-e $rtl_fail) {
        $result = sprintf("%30s %20s %5s    RTL_FAIL\n", $tSuite, $test, $pv);
        $delete = 1;
      }
      elsif (-e $fail) {
        $result = sprintf("%30s %20s %5s    FAILED\n", $tSuite, $test, $pv);
        $delete = 1;
      }
      elsif (-e $pass) {
        $result = sprintf("%30s %20s %5s    PASSED\n", $tSuite, $test, $pv);
        $delete = 1;
        $passCount++;
      }
      else {
        $result = sprintf("%30s %20s %5s    NOT_RUN\n", $tSuite, $test, $pv);
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
    my $result;

    if (-e $compile_fail) {
      $result = sprintf("%30s %20s %5s    COMPILE_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $model_fail) {
      $result = sprintf("%30s %20s %5s    MODEL_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $rtl_fail) {
      $result = sprintf("%30s %20s %5s    RTL_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $fail) {
      $result = sprintf("%30s %20s %5s    FAILED\n", $tSuite, $test, $pv);
    }
    elsif (-e $pass) {
      $result = sprintf("%30s %20s %5s    PASSED\n", $tSuite, $test, $pv);
    }
    else {
      $result = sprintf("%30s %20s %5s    NOT_RUN\n", $tSuite, $test, $pv);
    }
    push @regress_report, $result;
  }
  if ($filter) {
    print grep /$filter/, @regress_report;
  }
  else {
    print @regress_report;
  }
}
