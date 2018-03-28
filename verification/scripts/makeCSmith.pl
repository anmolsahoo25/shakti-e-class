#!/usr/bin/perl

#-----------------------------------------------------------
# makeRegress.pl
# Generates test related files
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use testRunConfig;
use scriptUtils;

checkSetup();
setEnvConfig();

my $simulator = getConfig("CONFIG_SIM");
my $testPath =  "$shaktiHome/verification/tests/random/csmith-run/generated_tests";

doPrint("Test generation: csmith\n");

# Parse options
GetOptions(
          qw(test_count=s)    => \my $test_count,
          qw(submit)          => \my $submit,
          qw(nodebug) => \my $no_debug,
          qw(help)     => \my $help,
          qw(clean)    => \my $clean
);

if (!$no_debug) {
  testRunConfig::setEnvConfig();
  
}
else {
  testRunConfig::setConfigValue("CONFIG_LOG",1);
}

my $testCount;

# Test count
if ($test_count) {
  $testCount = $test_count;
}
else {
  my $count = $ENV{'CONFIG_TORTURE_COUNT'};
  if ($count) {
    $testCount = $count;
  }
  else {
    $testCount = 1;
  }

}

if ($submit) {
  $testCount = 1;
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

systemCmd("mkdir -p $testPath");
chdir("$testPath");
doDebugPrint("cd $testPath\n");
for (my $i=0; $i < $testCount; $i++) {
  my @date = `date +%d%m%Y%s`; chomp(@date);
  my $testName = join("", "csmith_", $date[0], "_test$i");
  if ($submit) {
    systemCmd("csmith --no-packed-struct -o $testPath/$testName.c");
    system("CONFIG_LOG=1 perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$testName --suite=random/csmith-run/generated_tests/ --type=p --sim=$simulator");
  }
  else {
    systemCmd("csmith --no-packed-struct --float -o $testPath/$testName.c");
  }
}
