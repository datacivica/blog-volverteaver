# Author: Mariana Solano
# Maintainer(s): OS
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# ===============================================
# datos-volverteaver/import/src/import-poblacion.R
#

pacman::p_load(readxl, foreign, R.utils, tidyverse, janitor, here)

files <- list(