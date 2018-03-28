#!/usr/bin/perl

if (defined $ENV{'SHAKTI_HOME'}) {
  my $home = $ENV{'SHAKTI_HOME'};
  
  chdir($home);
  my @out = `git log -1 --pretty=format:"%h"`;
  chomp(@out);
  
  my $log = "$home/verification/tests/directed/benchmarks/sim/regression.log";
  if (-e $log) {
    `chmod 755 $log`;
    `scp -P443 $log riseweb\@10.21.226.184:/scratch/c-class/$out[0]\n`;
  }
}
else {
  print "ERROR: SHAKTI_HOME not defined\n";
  exit(1);
}
