
include ./Makefile.inc
ifeq ($(XLEN), 32)
  width=8
else
  width=16
endif
.PHONY: create_hex
create_hex:
	@dtc -O dtb -o shakti.dtb -b 0 shakti.dts
	@xxd -ps -c $(width) shakti.dtb config.spike
	@python create_config.py
	@printf '0202859300000297\n0182b283f1402573\n0000000000028067\n0000000080000000\n' | cat - boot.hex > temp && mv temp boot.hex

.PHONY: clean
clean:
	@rm -f shakti.dtb boot.hex config.spike
