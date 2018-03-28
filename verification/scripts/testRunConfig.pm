#!/usr/bin/perl

package testRunConfig;

use strict;
use Exporter qw(import);
our @EXPORT = qw( %testRunConfig setConfigValue setConfigAll setEnvConfig
                  getSimulator getConfig checkValid clearConfigAll printConfig
                );
# ----------------------------------------------------
# Test run configurations
# <script_name>_debug : [1 -> prints debug info, 0 -> no debug
# traceConfig         : generates trace files for debug, rtl, model trace, disassembly
# fsdbConfig          : generates fsdb file
# simulatorConfig     : config for which design simulator to use
#                       [0 -> bluespec, 1 -> ncverilog, 2 -> vcs]
# ----------------------------------------------------
our %testRunConfig = (
                  "CONFIG_LOG"    => 0,
                  "CONFIG_TRACE"  => 0,    
                  "CONFIG_FSDB"   => 0,     # should be done at link stage: TODO 
                  "CONFIG_COV"    => 0, # should be done at link stage: TOD0
                  "CONFIG_SIM"    => 0 # should be done at link stage
               );

# ------------------------------------------------
# method to set config with the arg value
# usage eg: setConfigValue("traceConfig", 0);
# -----------------------------------------------
sub setConfigValue {
  my @config = @_;
  $testRunConfig{$config[0]} = $config[1];
}

# used for makeRegress.pl
# default is no trace, debug, log
sub setEnvConfig {
  foreach my $key (keys %testRunConfig) {
    if (defined $ENV{$key}) {
      if (checkValid($key, $ENV{$key})) {
        $testRunConfig{$key} = $ENV{$key};
      }
      else {
        print "ERROR: Invalid CONFIG value\n";
        exit(1);
      }
    }
    else {
      if ($key =~ /^CONFIG_SIM/) {
        setConfigValue($key, 0);
      }
      else {
        setConfigValue($key, 1);
      }
    }
  }
}

sub checkValid {
  my @arg = @_;
  my $config = $arg[0];
  my $value = $arg[1];

  if ($value == 0 || $value == 1) {
    return 1;
  }
  else {
    if ($config =~ /^CONFIG_SIM$/) {
      if ($value == 2) {
        return 1;
      }
      else {
        return 0;
      }
    }
    else {
      return 0;
    }
  }
}

# ------------------------------------------------
# method to get config value
# usage eg: getConfig("traceConfig");
# -----------------------------------------------
sub getConfig {
  my @config = @_;
  return $testRunConfig{$config[0]};
}

# ------------------------------------------------
# method to get config value
# usage eg: setConfigAll();
# -----------------------------------------------
sub setConfigAll {
  foreach my $key (keys %testRunConfig) {
    # simulatorConfig will only be set using setConfigValue method 
    if ($key !~ /^CONFIG_SIM$/) {
      $testRunConfig{$key} = 1;
    }
  }
}

# ------------------------------------------------
# method to get clear config value
# usage eg: clearConfigAll();
# Note* : Use this method as default method for regression runs
#         bluespec is the default simulator
# -----------------------------------------------
sub clearConfigAll {
  foreach my $key (keys %testRunConfig) {
    $testRunConfig{$key} = 0;
  }
}

# ------------------------------------------------
# method to print config values
# usage eg: printConfig();
# -----------------------------------------------
sub printConfig {
  print "---------CONFIG------------------------------\n";
  foreach my $key (keys %testRunConfig) {
    print "$key\t\t: $testRunConfig{$key}\n";
  }
  print "---------------------------------------------\n";
}
1;
