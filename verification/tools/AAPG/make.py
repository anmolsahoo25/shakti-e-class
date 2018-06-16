#!/usr/bin/env python3

#Author: Nandu Raj P (nndurj@gmail.com)
import sys
import subprocess
import os
from commonStuff import *
import parseObjdump as pars
import traceback
import aapg
import time

seedFile=open('randomSeed.txt','r')
seeds=seedFile.readlines()

count=1
try:
	makeType=sys.argv[1]
except:
	makeType='all'

try:
  subprocess.check_output('which riscv{0}-unknown-elf-gcc'.format(bitwidth),shell=True,stderr=None)
except Exception as e:
  print(e)
  print("\n\nPlease install modified riscv-tools\n\n")

def compileASM():
  print('Compiling Assembly Program')
  subprocess.call('riscv{0}-unknown-elf-gcc -march=rv{0}imafd -mcmodel=medany -fomit-frame-pointer -fno-strict-aliasing -fno-builtin -c -o output.o output.s'.format(bitwidth),shell=True)

def linkObject():
  print('linking')
  subprocess.call('riscv{0}-unknown-elf-ld output.o -T ../link.ld -o output.riscv '.format(bitwidth),shell=True)

def genObjdump():
    print('objdump')
    subprocess.call('riscv{0}-unknown-elf-objdump -D output.riscv > disaassembled.txt'.format(bitwidth),shell=True)
    subprocess.call('riscv{0}-unknown-elf-objdump -s output.riscv > objdump.txt'.format(bitwidth),shell=True)
    subprocess.call('elf2hex {0} 65536 output.riscv 2147483648 > code.mem'.format(bitwidth/8),shell=True);
    subprocess.call('ln -s {0}/bin/* ./'.format(os.getenv("SHAKTI_C_HOME")),shell=True)
    subprocess.call('cut -c1-8 code.mem > code.mem.MSB',shell=True)
    subprocess.call('cut -c9-16 code.mem > code.mem.LSB',shell=True)

def runSpike():
    #subprocess.call('spike -s --isa=RV{0}IMAFDC output.riscv'.format(bitwidth),shell=True)
    subprocess.call('spike --isa=rv{0}imafd -c output.riscv'.format(bitwidth),shell=True)
#    subprocess.call('mv dump_disassembled.txt spike_disassembled.txt',shell=True)

def runRTLSim():
    subprocess.call('./out > rtl_log.txt',shell=True)

def makeDir(dirName):
  if dirName in os.listdir('.'):
    subprocess.call('rm -r {0}/* 2>/dev/null'.format(dirName),shell=True)
  else:
    subprocess.call('mkdir {}'.format(dirName),shell=True)

try:
  if makeType=='clean':
    subprocess.call('rm -rf result 2>/dev/null',shell=True)
    subprocess.call('rm -rf build 2>/dev/null', shell=True)
    subprocess.call('rm -rf *.pyc',shell=True)
    print('Cleaned')
  else:
    makeDir('result')
    makeDir('build')
    os.chdir('build')
  if(makeType=='all'):
    for seed in seeds:
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      runSpike();
      print ('Running RTL Simulation')
      runRTLSim();
      stdoutdata=subprocess.getoutput("diff -qiw spike.dump rtl.dump")
      if not stdoutdata:
        print("--- TEST PASSED ----- ")
      else:
        print(" ----- TEST FAILED -----")
        break
      #if(percentSPFloat!=0):
      #	stdoutdata=subprocess.getoutput("diff -qiw rtl_fregister_dump.txt spike_fregister_dump.txt")
      #if not stdoutdata:
      #  print("Floating Results Match")
      #else:
      #  print(" *!*!*!*!*! Floating Results DO NOT Match *!*!*!*!*!")
      #  break
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='gen_and_spike':
    for seed in seeds:
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      runSpike();
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='gen_only':
    for seed in seeds:
      print ('\nGenerating Test Case - '+str(count))
      aapg.random.seed(seed.strip())
      aapg.aapgMain('output.s')
      compileASM()
      linkObject()
      genObjdump()
      subprocess.call('mkdir ../result/test{0}'.format(count),shell=True)
      subprocess.call('mv * ../result/test{0}'.format(count),shell=True)
      if(count==numberOfTests):
        break
      count=count+1
  elif makeType=='link':
    linkObject()
  elif makeType=='compile':
    compileASM()
except Exception as err:
  traceback.print_tb(err.__traceback__)
  exit(1)
