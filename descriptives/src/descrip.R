#
# Author: Mariana Solano
# Maintainer(s): OE, MS, GJ, AL, AF
# License: (c) Data Cívica 2020
#
# =======================================
# datos-volverteaver/descriptives/src/descrip.R
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, ggrepel, scales, patchwork, extrafont, here)
extrafont::loadfonts(quiet=T)

files <- list(cenapi_rnpdno = here("clean-data/output/rnpdno-cenapi-years.rds"),
              rnpdno_edos = here("clean-data/output/rnpdno-estados.rds"),
              rnpdno_mor = here("clean-data/output/rnpdno-morelos.rds"),
              sesnsp = here("clean-data/output/sesnsp.rds"),
              g1 = here("descript/output/.png"),
              g2 = here("descript/output/.png"),
              g3 = here("descript/output/.png"),
              g4 = here("descript/output/.png"),
              g5 = here("descript/output/.png")
)

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 18, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 12, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 12, family = "Barlow Condensed"),
        axis.title = element_text(size = 14, family = "Barlow Condensed"),
        legend.text = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold", family = "Barlow Condensed"),
        text = element_text(family = "Barlow Condensed"),
        legend.position = "top")

anualizar <- function(variable, mes_anual){
  round(variable*(12/mes_anual))
}
mes_anual = 8

cenapi <- readRDS(files$cenapi)
rnpdno_edos <- readRDS(files$rnpdno_edos)
rnpdno_mor <- readRDS(files$rnpdno_mor)
sesnsp <- readRDS(files$sesnsp)

