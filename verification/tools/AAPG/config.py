# All the python files use the settings mention in this file
import os
import sys

numberOfTests=10

spikeBootAddress = 0x80000000			      # This is where spike boots from.
stackPointerRegister = 2
memoryBaseAddressRegister = 2       #Base address for memory operations is stored in this register.

sperateInstrDataMemory = False      # False will generate one "hex".
lineWidthOfMainMemory  = 4          # number of bytes per line in the main memory hex. can be 4,8,16
depthOfMainMemory      = 16384       # number of lines in the Main memory.

totalInstructions = 12000            #Total number of instructions to generate
bitwidth=64
initialMemorySize = 1                #Size in KB. Should be less than or equal to 4 since immediate value for memory ops allows only that range.
maxNestedLoops= 3                    #Maximum number of nested loops
maxLoopIterations= 20                 #Max number of iterations for a loop
forwardBranchRange= 10               #Maximum number of instructions that can be jumped over during forward jumps
loopRange= 20                        #Maximum number of instructions within a loop is roughly loopRange
branchBackwardProbability= 0.2        #Prob of a branch being backward. Increase this to make more loops.

# Percentage split of instructions
percentBaseInstr = 100
perIntegerComputation =30           # Integer computation
perControlTransfer = 30              # Control transfer
perLoadStore = 40                    # Load and Store
perSystemInstr = 0                   # System


# Single precision floating point

percentSPFloat = 0                  # 0 = disabled
percentSPLoadStore = 30
percentSPComputational = 30
percentSPConversionMov = 20
percentSPCompare = 10
PercentSPClassify= 10
roundingmode=""
# Double precision floating point
percentDPFloat = 0                  # 0 = disabled
percentDPLoadStore = 30
percentDPComputational = 40
percentDPConversionMov = 30
percentDPCompare = 0
PercentDPClassify= 0

# Percent privileged instructions
percentPrivilegedInstructions = 0
percentPrivilegedBaseInstr = 0
percentChangePrivilegeInstr = 0
percentTrapRedirectionInstr = 0
percentInterruptManagementInstr = 0
percentMemoryManagementInstr = 0
percentCustomInstr = 0

# Atomic instructions
percentAtomicInstructions = 0

#Data hazards Probability ( out of 1 )
numberOfPreviousRegistersToConsider=3
readAfterWrite = 0.8	#Probability of a source register being the destination of one of the previous considered instructions
writeAfterRead=0.2		#Probability of a destination register being the source of one of the previous considered instructions
writeAfterWrite=0.2		#Probability of a destination register being the destination of one of the previous considered instructions




