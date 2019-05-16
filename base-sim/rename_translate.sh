#!/bin/bash
if [[ $1 != *"mkTb"* ]] && [[ $1 != *"sign_dump"* ]]; then
	sed -i 's/\/\/ synopsys translate_off/`ifdef VERBOSE/g' $1
	sed -i 's/\/\/ synopsys translate_on/`endif/g' $1
fi
