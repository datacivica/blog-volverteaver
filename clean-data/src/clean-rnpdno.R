# Author: Mariana Solano
# Maintainer(s): OE, AF, AL, GJ
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# ===============================================
# datos-volverteaver/clean-data/clean-rnpdno.R


if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, lubridate, here)

# === File paths === #
files <- list(pob = here("import/output/poblacion.rds"),
              inp_cenapi = here("import/output/cenapi.rds"), 
              inp_rnpdno_nac = here("import/output/rnpdno-nac.rds"),
              inp_rnpdno_mor = here("import/output/rnpdno-mor.rds"),
              inp_rnpdno_y = here("import/output/rnpdno-mor-year.rds"),
              out_years = here("clean-data/output/rnpdno-cenapi-years.rds"),
              out_rnpdno_nac = here("clean-data/output/rnpdno-estados.rds"),
              out_rnpdno_mor = here("clean-data/output/rnpdno-morelos.rds")
              )

keep_years <- 2000:2020

# === CENAPI y RNPDNO Morelos por año === #
cenapi_rnpdno <- readRDS(files$inp_cenapi) %>%
  mutate(fecha_evento = word(fecha_evento, 1),
         fuente = "CENAPI",
         sexo = case_when(sexo=="MASCULINO" ~ "Hombres",
                          T ~ "Mujeres"),
         cve_ent = case_when(nom_ent == "NO ESPECIFICADO"  ~ "99",
                             nom_ent == "ZACATECAS" ~ "32",
                             nom_ent == "MICHOACAN" ~ "16",
                             nom_ent == "TAMAULIPAS"~ "28",
                             cve_ent == "NA" ~ "99",
                             T ~ cve_ent),
         year = year(as.Date(fecha_evento)),
         mes = month(as.Date(fecha_evento)),
         dia = day(as.Date(fecha_evento)),
         estatus = case_when(estatus == "AUN SIN LOCALIZAR" ~ "Aún sin localizar",
                             estatus == "VIVO" ~ "Localizado con vida",
                             estatus == "MUERTO" ~ "Localizado sin vida")) %>% 
  filter(year %in% keep_years & cve_ent == "17") %>% 
  group_by(year, sexo, estatus, fuente) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  bind_rows(readRDS(files$inp_rnpdno_y) %>% 
              mutate(year = as.integer(year),
                     year = case_when(is.na(year) ~ as.integer(9999),
                                      T ~ year)) %>% 
              rename(Hombres = hombre, 
                     Mujeres = mujer,
                     Indeterminado = indeterminado) %>% 
              pivot_longer(cols = -c(year, cve_ent, estatus),
                           names_to = "sexo",
                           values_to = "tot") %>%
              filter(year %in% keep_years) %>% 
              mutate(fuente = "RNPDNO") %>% 
              select(-cve_ent))

all <- cenapi_rnpdno %>% expand(crossing(year, sexo, estatus, fuente))

cenapi_rnpdno <- right_join(cenapi_rnpdno, all) %>% 
  replace_na(list(tot = 0)) %>% 
  arrange(year, sexo, estatus, fuente)

rm(all)
saveRDS(cenapi_rnpdno, files$out_years)

# === RNPDNO Nacional === #
rnpdno_nac <- readRDS(files$inp_rnpdno_nac) %>% 
  mutate(entidad = str_to_title(nom_ent)) %>% 
  rename(Hombres = hombre, 
         Mujeres = mujer,
         Indeterminado = indeterminado) %>% 
  select(-nom_ent) %>% 
  pivot_longer(cols = -c(entidad),
               names_to = "sexo",
               values_to = "tot") 

saveRDS(rnpdno_nac, files$out_rnpdno_nac)

# === RNPDNO Morelos por Municipio === #
rnpdno_mor <- readRDS(files$inp_rnpdno_mor) %>% 
  mutate(municipio = str_to_title(municipio),
         municipio = case_when(municipio %in% "Sin Municipio De Referencia" ~ "Se Desconoce",
                               T ~ municipio)) %>% 
 rename(Hombres = hombre, 
        Mujeres = mujer,
        Indeterminado = indeterminado) %>% 
  pivot_longer(cols = -c(municipio, cve_ent, estatus),
               names_to = "sexo",
               values_to = "tot") %>%
  select(-cve_ent) %>% 
  group_by(municipio, estatus, sexo) %>% 
  summarise(tot = sum(tot)) %>% 
  left_join(readRDS(files$pob) %>% 
              filter(cve_ent == "17" & year == 2020) %>% 
              group_by(municipio, sexo) %>% 
              summarise(pob = sum(pob)))

saveRDS(rnpdno_mor, files$out_rnpdno_mor)

# done.
