#
# Author: Mariana Solano
# Maintainer(s): OE, MS, AL, AF, GJ
# License: (c) Data Cívica 2020
#
# ---------------------------------------------------------
# datos-volverteaver/import/src/import.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, data.table, readxl, here)

# === File paths === #
file_paths = list(input = here("import/input/"),
                  cenapi = here("import/output/cenapi.rds"),
                  sesnsp = here("import/output/sesnsp.rds"),
                  pob = here("import/output/poblacion.rds"),
                  rnpdno_nac = here("import/output/rnpdno-nac.rds"),
                  rnpdno_y = here("import/output/rnpdno-mor-year.rds"),
                  rnpdno_mor = here("import/output/rnpdno-mor.rds")
                  )

# === CENAPI === #
print("working on CENAPI")
cenapi <- fread(paste0(file_paths$input, "cenapi/cenapi.csv")) %>% 
  clean_names() %>% 
  select(fecha_evento, estado, clave_estado, sexo, edad, vivo_o_muerto) %>%
  rename(nom_ent = estado,
         cve_ent = clave_estado,
         estatus = vivo_o_muerto) %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"))

saveRDS(cenapi, file_paths$cenapi)

# === RNPDNO === #
print("working on rnpdno")

# Para Morelos por municipio y estatus
rnpdno_mor_path <- paste0(file_paths$input, "rnpdno/morelos/")
rnpdno_mor_files <- dir(rnpdno_mor_path)

rnpdno_mor <- data.frame()

pb <- txtProgressBar(min=1, max=length(rnpdno_mor_files), style=3)
for (i in 1:length(rnpdno_mor_files)) {
  tempo <- fread(paste0(rnpdno_mor_path, rnpdno_mor_files[i])) %>% 
    clean_names() %>% 
    mutate(cve_ent = parse_number(rnpdno_mor_files[i]),
           cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"),
           estatus = str_extract(rnpdno_mor_files[i], "[^_]+"),
           estatus = case_when(estatus=="d" ~ "Aún sin localizar",
                               estatus=="lcv" ~ "Localizado con vida",
                               estatus=="lsv" ~ "Localizado sin vida"),
           category = as.character(category)) %>% 
    rename(municipio = category)
  rnpdno_mor <- bind_rows(rnpdno_mor, tempo)
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

saveRDS(rnpdno_mor, file_paths$rnpdno_mor)

# Para Morelos por año y estatus
rnpdno_y_path <- paste0(file_paths$input, "rnpdno/morelos-year/")
rnpdno_y_files <- dir(rnpdno_mor_path)

rnpdno_y <- data.frame()

pb <- txtProgressBar(min=1, max=length(rnpdno_y_files), style=3)
for (i in 1:length(rnpdno_y_files)) {
  tempo <- fread(paste0(rnpdno_y_path, rnpdno_y_files[i])) %>% 
    clean_names() %>% 
    mutate(cve_ent = parse_number(rnpdno_y_files[i]),
           cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"),
           estatus = str_extract(rnpdno_y_files[i], "[^_]+"),
           estatus = case_when(estatus=="d" ~ "Aún sin localizar",
                               estatus=="lcv" ~ "Localizado con vida",
                               estatus=="lsv" ~ "Localizado sin vida"),
           category = as.character(category)) %>% 
    rename(year = category)
  rnpdno_y <- bind_rows(rnpdno_y, tempo)
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

saveRDS(rnpdno_y, file_paths$rnpdno_y)

# Estatal total
nacional <- fread(paste0(file_paths$input, "rnpdno/estatal/total-estatal.csv")) %>% 
  clean_names() %>% 
  rename(nom_ent = category)

saveRDS(nacional, file_paths$rnpdno_nac)

# === SESNSP === #
print("working on sesnsp")
sesnsp <- read_csv(paste0(file_paths$input, "sesnsp/IDM_NM_ago2020.csv"), locale=locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  rename(year = ano,
         cve_ent = clave_ent,
         cve_mun = cve_municipio) %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"))
print("writing sesnsp")
saveRDS(sesnsp, file_paths$sesnsp)

# === POBLACIÓN === #
print("working on pob-mun")
pob <- fread(paste0(file_paths$input, "municipios_sexo_15-30.csv")) %>% 
  rename(year = ano,
         cve_ent = clave_ent,
         inegi = clave,
         municipio = mun,
         entidad = nom_ent) %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"),
         inegi = str_pad(inegi, width = 5, side = "left", pad = "0"))
print("writing on pob")
saveRDS(pob, file_paths$pob)

# done.
