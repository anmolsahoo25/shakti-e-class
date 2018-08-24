
include ./Makefile.inc
ifeq ($(XLEN), 32)
  width=4
else
  width=8
endif

default: create_hex append$(width)

.PHONY: append4
append4:
	@printf '00000297\n02028593\nf1402573\n0182a283\n00028067\n00000000\n80000000\n00000000\n' | cat - boot.hex > temp && mv temp boot.hex
	@for number in $$(seq 1 7926) ; do \
    echo "00000000" >> boot.hex; \
  done;

.PHONY: append8
append8:
	@printf '0202859300000297\n0182b283f1402573\n0000000000028067\n0000000080000000\n' | cat - boot.hex > temp && mv temp boot.hex
	@for number in $$(seq 1 8059) ; do \
    echo "0000000000000000" >> boot.hex; \
  done;

.PHONY: create_hex
create_hex:
	@spike --isa=rv$(XLEN)imac --dump-dts > shakti.dts
	@dtc -O dtb -o shakti.dtb -b 0 shakti.dts
	@xxd -e -c $(width) shakti.dtb config.spike
ifeq ($(XLEN),64)
	@cat config.spike| awk '{print $$3 $$2}' > temp
endif
ifeq ($(XLEN),32)
	@cat config.spike| awk '{print $$2}' > temp
endif
	@mv temp boot.hex


.PHONY: clean
clean:
	@rm -f shakti.dtb boot.hex config.spike shakti.dts spike.dump
