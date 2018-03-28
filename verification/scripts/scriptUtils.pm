#!/usr/bin/perl

package scriptUtils;

use strict;
use warnings;

use Exporter qw(import);
our @EXPORT = qw(systemCmd systemFileCmd systemKillCmd 
                 doClean doPrint doDebugPrint printHelp
                 checkSetup openLog closeLog appendLog
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
  $workdir = "$shaktiHome/verification/workdir";
  
  # create temporary directory where all outputs are generated
  unless (-e $workdir or mkdir $workdir) {
    doPrint("ERROR: Unable to create workdir!\n");
    exit(1);
  }
  appendLog("$workdir/$scriptLog.log");
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
    if ($cmd[0] =~ /^\.\/out/) { `touch RTL_FAIL`};
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
    if ($cmd[0] =~ /^\.\/out/) { `touch RTL_FAIL`};
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
  doPrint("Cleaning...\n");
  systemCmd("rm -rf $shaktiHome/verification/workdir/*");
  systemCmd("rm -rf $shaktiHome/verification/scripts/nohup.out");
  systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/nohup.out");
  systemCmd("rm -rf $shaktiHome/verification/tools/AAPG/__pycache__");
  systemCmd("rm -rf $shaktiHome/verification/tests/random/*/generated_tests/*");
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
  my $usage =<<USAGE;

Description: Generates test dump directory
Options:
  --test=TEST_NAME

USAGE

  print $usage;
}


1;
