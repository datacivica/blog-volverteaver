# Author: Mariana S
# Maintainer(s): OE, MS, AF
# License: (c) Data Cívica 2020
#
# ------------------------------------
#
# datoss-volverteaver/import/Makefile
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

all: output/cenapi.rds \
	 output/nombre-entidad.rds \
	 output/poblacion.rds \
	 output/rnpdno-mor.rds \
  	 output/rnpdno-mor-year.rds \
	 output/rnpdno-nsv.rds \
	 output/sesnsp.rds

clean:
	rm output/*

INPUTS := $(shell find $(HERE)/import/input -name '*.csv')

OUTPUTS := output/cenapi.rds \
	 output/nombre-entidad.rds \
	 output/poblacion.rds \
	 output/rnpdno-mor.rds \
  	 output/rnpdno-mor-year.rds \
	 output/rnpdno-nsv.rds \
	 output/sesnsp.rds

$(OUTPUTS): src/import.R $(INPUTS)
	Rscript --vanilla $<

# done.