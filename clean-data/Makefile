# Author: Mariana S
# Maintainer(s): OE, MS, AF
# License: (c) Data Cívica 2020
#
# ------------------------------------
#
# datos-volverteaver/clean-data/Makefile
#

.PHONY: all clean

all: output/rnpdno-cenapi-years.rds \
	 output/rnpdno-estados.rds \
	 output/rnpdno-morelos.rds \
	 output/sesnsp.rds

clean:
	rm output/*

OUT_RNPDNO := output/rnpdno-cenapi-years.rds \
	 output/rnpdno-estados.rds \
	 output/rnpdno-morelos.rds

$(OUT_RNPDNO): src/clean-rnpdno.R
	Rscript --vanilla $<

output/sesnsp.rds: src/clean-sesnsp.R
	Rscript --vanilla $<

# done.
