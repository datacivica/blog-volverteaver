# Author: OE 
# Maintainer(s): OE, MS, AF
# License: (c) Data Cívica 2020
# ------------------------------------
# datos-volverteaver/descriptives/Makefile
#

.PHONY: all clean

all: output/bar-estatal.svg \
	output/bar-estatal.png \
	output/fiebre-estatus.svg \
	output/fiebre-estatus.png \
	output/area-estatus-sexo.svg \
	output/area-estatus-sexo.png \
	output/point-municipios.svg \
	output/point-municipios.png \
	output/fiebre-sexo-mor.svg \
	output/fiebre-sexo-mor.png \
	output/scatter-delitos.svg \
	output/scatter-delitos.png

clean:
	rm output/*

OUTPUTS := output/bar-estatal.svg \
	output/bar-estatal.png \
	output/fiebre-estatus.svg \
	output/fiebre-estatus.png \
	output/area-estatus-sexo.svg \
	output/area-estatus-sexo.png \
	output/point-municipios.svg \
	output/point-municipios.png \
	output/fiebre-sexo-mor.svg \
	output/fiebre-sexo-mor.png \
	output/scatter-delitos.svg \
	output/scatter-delitos.png

$(OUTPUTS): src/descrip.R
	Rscript --vanilla $<
	rm Rplots.pdf

# done.
