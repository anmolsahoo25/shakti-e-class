
include ./Makefile.inc
ifeq ($(XLEN), 32)
  width=8
else
  width=16
endif

default: create_hex append$(width)

.PHONY: append8
append8:
	@printf '00000297\n02028593\nf1402573\n0182a283\n00028067\n00000000\n80000000\n00000000\n' | cat - boot.hex > temp && mv temp boot.hex

.PHONY: append16
append16:
	@printf '0202859300000297\n0182b283f1402573\n0000000000028067\n0000000080000000\n' | cat - boot.hex > temp && mv temp boot.hex

.PHONY: create_hex
create_hex:
	@spike --isa=rv$(XLEN)imac --dump-dts > shakti.dts
	@dtc -O dtb -o shakti.dtb -b 0 shakti.dts
	@xxd -ps -c $(width) shakti.dtb config.spike
	@python create_config.py

.PHONY: clean
clean:
	@rm -f shakti.dtb boot.hex config.spike shakti.dts spike.dump
