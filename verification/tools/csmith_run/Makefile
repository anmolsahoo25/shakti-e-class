SHELL := /bin/bash # Use bash syntax
include ./Makefile.inc

CSMITH_INCDIR = /tools/csmith/runtime
RUNS=10
all:
	@ln -f -s $(SHAKTI_C_HOME)/bin/* .
	@n=1 ; while [[ $$n -le $(RUNS) ]] ; do \
	echo "----------------Test $$n ----------------";\
	make spike || { echo SKIP; make remove; continue; } ;\
	cut -c1-8 code.mem > code.mem.MSB ;\
	cut -c9-16 code.mem > code.mem.LSB ;\
	echo "Running test on SHAKTI C-$(XLEN)";\
	timeout 1m ./out > log || { echo SIM TIMED OUT; exit; };\
	cat app_log; echo " ";\
	diff -iqw app_log output.riscv || { echo RTL AND SPIKE DO NOT MATCH; exit; } ;\
	mkdir -p testdir_$$n ;\
	mv rtl* test.* output.* log code.mem* spike.dump testdir_$$n/ ; \
	((n=n+1)) ; \
	done;



spike: test.host test.riscv
	@echo "Running test on HOST";
	@timeout 2 ./test.host > output.host 
	@cat output.host
	@echo "Running test on SPIKE"
	@spike -c test.riscv > output.riscv 
	@cat output.riscv
	@diff -iqw output.host output.riscv

test.host: test.c
	@gcc -m$(XLEN) -w -Os -I $(CSMITH_INCDIR) -o test.host test.c

test.riscv: test.c crt.o syscalls.spike syscalls.shakti
	@riscv$(XLEN)-unknown-elf-gcc -w -Os -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) test.c -o test.o -march=rv$(XLEN)imafd 
	@riscv$(XLEN)-unknown-elf-gcc -T link.ld -I $(CSMITH_INCDIR) test.o syscalls.spike crt.o -o test.riscv -static -nostdlib -nostartfiles -lgcc -lm 
	@riscv$(XLEN)-unknown-elf-gcc -T link.ld -I $(CSMITH_INCDIR) test.o syscalls.shakti crt.o -o test.shakti -static -nostdlib -nostartfiles -lgcc -lm
	@riscv$(XLEN)-unknown-elf-objdump -D test.riscv > test.riscv.dump
	@riscv$(XLEN)-unknown-elf-objdump -D test.shakti > test.shakti.dump
	@elf2hex 8 32768 test.shakti 2147483648 > code.mem

test.c:
	@csmith --no-packed-struct -o test.c
	@gawk '/Seed:/ {print$$2,$$3;}' test.c

crt.o:
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c -I $(CSMITH_INCDIR) crt.S -o crt.o

syscalls.spike:
	@cp syscalls.c.spike syscalls.c
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) syscalls.c -o syscalls.spike
	@rm -f syscalls.c

syscalls.shakti:
	@cp syscalls.c.shakti syscalls.c
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf  -c -I $(CSMITH_INCDIR) syscalls.c -o syscalls.shakti
	@rm -f syscalls.c

clean: 
	@rm -rf platform.info test.* output.host output.riscv app_log code.mem* crt.o syscalls.spike syscalls.shakti testdir* out out.so rtl_* log config_string64.hex* *.txt rtl.dump spike.dump
	@find -type l -delete
	@echo "cleaned"
remove:
	@rm -f platform.info test.c test.host test.riscv output.host output.riscv test.o
