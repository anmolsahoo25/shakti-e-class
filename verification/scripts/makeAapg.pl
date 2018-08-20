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
# makeAapg.pl
# Generates random tests using AAPG
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use testRunConfig;
use scriptUtils;

checkSetup();
setEnvConfig();

my $simulator = getConfig("CONFIG_SIM");
my $configPath = "$shaktiHome/verification/tests/random/aapg/configs";
my $testPath =  "$shaktiHome/verification/tests/random/aapg/generated_tests";


# Parse options
GetOptions(
          qw(config=s)        => \my $test_config,
          qw(list_configs)    => \my $list_configs,
          qw(test_count=s)    => \my $test_count,
          qw(new)             => \my $new,
          qw(submit)          => \my $submit,
          qw(nodebug)         => \my $no_debug,
          qw(parallel)        => \my $parallel,
          qw(help)            => \my $help,
          qw(clean)           => \my $clean
);

if (!$no_debug) {
  testRunConfig::setEnvConfig();
  
}
else {
  testRunConfig::setConfigValue("CONFIG_LOG",1);
}

my $testConfig;
my $testCount;

# Test suite
if ($test_config) {
  $testConfig = $test_config;
}
else {
  $testConfig = "bringup"; 
}

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

if ($list_configs) {
  my @configs = `ls $configPath/*.py |  xargs -n 2 basename -s .py`;
  doPrint("Config list at $configPath:\n");
  foreach my $cfg (@configs) {
    print "\t--config=$cfg";
  }
  exit(0);
}
doPrint("Test generation: AAPG\n");
## Process tests.list -----------
open TESTLIST, "$shaktiHome/verification/scripts/tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
my @listFile = <TESTLIST>;
close TESTLIST;

my @testList = ();
if ($new) {
  for (my $i=0; $i < $testCount; $i++) {
    my $name = "test_$i";
    systemCmd("aapg gen --config-file $configPath/compute.ini --output-dir $testPath --asm-name $name");
    system("CONFIG_LOG=1 perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --new-aapg --test=$name\_00000 --suite=random/aapg/generated_tests --type=p --sim=$simulator");
  }
}
else {
  if ($testConfig =~ /^all$/) {
    systemCmd("perl -pi -e 's/numberOfTests=.*/numberOfTests=$testCount/' $shaktiHome/verification/tests/random/aapg/configs/*.py");
    my @configs = `ls $shaktiHome/verification/tests/random/aapg/configs/*.py`;
    chomp(@configs);
    foreach my $config (@configs) {
      systemCmd("cp $config $shaktiHome/verification/tools/AAPG/config.py");
      chdir("$shaktiHome/verification/tools/AAPG");
      $config = `basename $config .py`; chomp($config);
      systemCmd("./regress.py gen_only $config nodebug&");
    }
  }
  else {
    if ($submit) {
        systemCmd("cp $configPath/bringup.py $shaktiHome/verification/tools/AAPG/config.py");
        chdir("$shaktiHome/verification/tools/AAPG");
        my @out = `./regress.py gen_only bringup`;
        if($out[-1] =~ /\] (\S+)\.S/) {
          system("CONFIG_LOG=1 perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$1 --suite=random/aapg/generated_tests/bringup --type=p --sim=$simulator");
        }
    }
    else {
      my $configFile = "$configPath/$testConfig\.py";
      if (-e $configFile) {
        doDebugPrint("$configFile is being used for AAPG generation\n");
        systemCmd("perl -pi -e 's/numberOfTests=.*/numberOfTests=$testCount/' $configFile");
        systemCmd("cp $configFile $shaktiHome/verification/tools/AAPG/config.py");
        chdir("$shaktiHome/verification/tools/AAPG");
        my $config = `basename $configFile .py`; chomp($config);
        systemCmd("./regress.py gen_only $config&");
      }
      else {
        doPrint("ERROR: $configFile does not exist\n");
        exit(1);
      }
    }
  }
}
