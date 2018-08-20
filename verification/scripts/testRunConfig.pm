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
                  "CONFIG_SIM"    => "bluespec" # should be done at link stage
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
        setConfigValue($key, "bluespec");
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
      if ($value =~ /^bluespec$/ || $value =~ /^ncverilog$/ || $value =~ /^vcs$/) {
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
