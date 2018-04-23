import os
import sys

file1=open('config.spike','r')
outfile=open('boot.hex','w')

for lineno,line in enumerate(file1):
		lsb,msb = line[:int(len(line)/2)].rstrip("\n"),line[int(len(line)/2):].rstrip("\n")
		lsb="".join(reversed([lsb[i:i+2] for i in range(0, len(lsb), 2)]))
		msb="".join(reversed([msb[i:i+2] for i in range(0, len(msb), 2)]))
		outfile.write(lsb+'\n'+msb+"\n")
		
