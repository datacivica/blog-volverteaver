# Author: Mariana Solano
# Maintainer(s): OE, AF, AL, GJ
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# ===============================================
# datos-volverteaver/clean-data/clean-sesnsp.R


if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, stringr, here)

# === File paths === #
files <- list(inp_sesnsp = here("import/output/sesnsp.rds"), 
              pob = here("import/output/poblacion.rds"),
              out_sesnsp = here("clean-data/output/sesnsp.rds")
)

# ==== Population ==== #
pob <- readRDS(files$pob) %>% 
  filter(cve_ent == "17") %>% 
  group_by(year, cve_mun) %>% 
  summarise(pob = sum(pob))


# === SESNSP === #
print("working in SESNSP")
sesnsp <- readRDS(files$inp_sesnsp) %>% 
  filter(subtipo_de_delito %in% c("Feminicidio", "Homicidio doloso", "Tráfico de menores",
                                  "Otros delitos que atentan contra la libertad personal",
                                  "Trata de personas")) %>% 
  filter(cve_ent == "17") %>% 
  rowwise() %>%
  mutate(tot = sum(across(enero:diciembre), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, cve_mun, municipio, subtipo_de_delito) %>% 
  summarise(tot = sum(tot)) %>% 
  ungroup() %>% 
  left_join(pob, by = c("cve_mun", "year"))
  
saveRDS(sesnsp, files$out_sesnsp)



