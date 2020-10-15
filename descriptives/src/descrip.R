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
              g1 = here("descriptives/output/bar-estatal.svg"),
              g1_1 = here("descriptives/output/bar-estatal.png"),
              g2 = here("descriptives/output/fiebre-estatus.svg"),
              g2_2 = here("descriptives/output/fiebre-estatus.png"),
              g3 = here("descriptives/output/area-estatus-sexo.svg"),
              g3_3 = here("descriptives/output/area-estatus-sexo.png"),
              g4 = here("descriptives/output/point-municipios.svg"),
              g4_4 = here("descriptives/output/point-municipios.png"),
              g5 = here("descriptives/output/fiebre-sexo-mor.svg"),
              g5_5 = here("descriptives/output/fiebre-sexo-mor.png"),
              g6 = here("descriptives/output/scatter-delitos.svg"),
              g6_6 = here("descriptives/output/scatter-delitos.png")
              )

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0, face = "italic"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold"),
        text = element_text(family = "Barlow Condensed"),
        legend.position = "top")

anualizar <- function(variable, mes_anual){
  round(variable*(12/mes_anual))
}
mes_anual = 9

cenapi <- readRDS(files$cenapi)
rnpdno_edos <- readRDS(files$rnpdno_edos)
rnpdno_mor <- readRDS(files$rnpdno_mor)
sesnsp <- readRDS(files$sesnsp)

##==== RNPDNO nacional ====##
rnpdno_edos %>%
  filter(entidad != "Se Desconoce") %>% 
  pivot_wider(names_from = sexo, values_from = tot) %>% 
  rowwise() %>%
  mutate(total_d = sum(across(Hombres:Indeterminado), na.rm = T),
         porcent_Mujeres = round(Mujeres/total_d, 3),
         porcent_Hombres = round(Hombres/total_d, 3),
         porcent_Indeterminado = round(Indeterminado/total_d, 3)) %>% 
  ungroup() %>% 
  mutate(entidad = fct_reorder(entidad, porcent_Hombres)) %>% 
  select(entidad, starts_with("porcent")) %>% 
  pivot_longer(cols = starts_with("porcent"), names_to = "sexo", 
               names_prefix = "porcent_",values_to = "porcent") %>% 
  mutate(sexo = factor(sexo, levels = c("Indeterminado", "Mujeres", "Hombres"))) %>% 
  ggplot(aes(x = porcent, y = entidad , fill = sexo)) +
    geom_col() +
    scale_fill_manual(values = c("#f4d35e", "#0d3b66", "#f95738")) + 
    scale_x_continuous(breaks = seq(from=0, to=1, by=0.2), labels = scales::percent) +
    labs(title = "Porcentaje de personas desaparecidas y no localizadas", 
         subtitle = "Por entidad y sexo", 
         caption = "Fuente: Elaboración propia con datos del RNPDNO", 
         fill = "", y = "", x = "Porcentaje de personas desaparecidas y no localizadas") +
    tema +
    guides(fill = guide_legend(reverse = TRUE))

ggsave(files$g1, width = 10, height = 12)
ggsave(files$g1_1, width = 10, height = 12)

##==== RNPDNO y CENAPI por año ====##
cenapi %>% 
  filter(year >= 2010) %>% 
  group_by(year, estatus, fuente) %>% 
  summarise(tot = sum(tot, na.rm = T)) %>% 
  mutate(tot = case_when(year==2018 & fuente=="CENAPI" ~ anualizar(variable = tot, mes_anual = 4),
                         year==2020 & fuente=="RNPDNO" ~ anualizar(variable= tot, mes_anual = mes_anual),
                         T ~ tot)) %>% 
  pivot_wider(names_from = fuente, values_from = tot) %>% 
  ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin=CENAPI, ymax=RNPDNO), fill = "#969696" , alpha = .5) +
    geom_line(aes(y = RNPDNO, color = "RNPDNO"), size = 2) +
    geom_line(aes(y = CENAPI, color = "CENAPI"), size = 2) +
    geom_point(aes(y = RNPDNO, color = "RNPDNO"), size = 2) +
    geom_point(aes(y = CENAPI, color = "CENAPI"), size = 2) +
    facet_wrap(~estatus, nrow = 3, scales = "free") +
    scale_color_manual(values = c('RNPDNO' = "#0d3b66",'CENAPI' = "#f95738")) +
    scale_x_continuous(breaks = seq(from=2010, to=2020, by=1)) + 
    labs(title = "Registro de personas desaparecidas, no localizadas y localizadas en Morelos",
         subtitle = "por fuente y estatus de 2010 a 2020",
         caption = "Fuente: Elaboración propia con datos del CENAPI y RNPDNO.\nCENAPI 2018 y RNPDNO 2020 anualizados.",
         x = "", y = "Total de registros", color = " ") +
    tema +
    theme(legend.position = "top",
          axis.text.x = element_text(size = 12, face = "bold"))

ggsave(files$g2, width = 12, height = 12)
ggsave(files$g2_2, width = 12, height = 12)


cenapi %>% 
  filter(fuente == "RNPDNO" & year >= 2010) %>% 
  group_by(year, sexo, estatus) %>% 
  summarise(tot = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, sexo) %>% 
  mutate(total_d = sum(tot),
         porcent = round(tot/total_d, 4)) %>% 
  filter(sexo != "Indeterminado") %>% 
  ggplot(aes(x = year, y = porcent , fill = reorder(estatus, porcent))) +
    geom_area() +
    facet_wrap(~sexo, nrow = 2) +
    scale_fill_manual(values = c("#f4d35e","#f95738", "#0d3b66")) +
    scale_x_continuous(breaks = seq(from=2010, to=2020, by=1)) +
    scale_y_continuous(breaks = seq(from=0, to=1, by=0.2), labels = scales::percent) +
    labs(title = "Porcentaje de personas desaparecidas, no localizadas y localizadas en Morelos",
         subtitle = "por sexo de 2010 a 2020",
         caption = "Fuente: Elaboración propia con datos del RNPDNO.",
         x = "", y = "", fill = " ") +
    tema +
    theme(legend.position = "top",
          axis.text.x = element_text(size = 12, face = "bold"))

ggsave(files$g3, width = 10, height = 12)
ggsave(files$g3_3, width = 10, height = 12)

##==== RNPDNO municipios Morelos ====##
rnpdno_mor %>% 
  filter(sexo != "Indeterminado" & municipio != "Se Desconoce") %>% 
  group_by(municipio, sexo, pob) %>% 
  summarise(tot = sum(tot, na.rm = T)) %>% 
  mutate(tasa = round(tot*(100000/pob), 2)) %>% 
  filter(!is.na(tasa)) %>% 
  ggplot(aes(x=reorder(municipio, tasa), y=tasa, size=tasa, label=tasa, fill=sexo, color=sexo)) + 
  geom_point() +
  scale_color_manual(values=c("#f95738", "#0d3b66")) +
  geom_text_repel(color="black", size=3, position = position_dodge(1), family = "Barlow Condensed") +
  labs(title = "Tasa de personas desaparecidas y no localizadas en los municipios de Morelos", 
       subtitle="por sexo", y="Tasa por 100,000 habitantes", 
       x=" ", fill = " ", color = " ", 
       caption="Fuente: Elaboración propia con datos del RNPDNO y CONAPO.\nSe calcula la tasa dividiendo el acumulado de personas desaparecidas entre la población actual.") + 
  coord_flip() + 
  tema +
  guides(size = F)+
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, face = "bold"))

ggsave(files$g4, width = 12, height = 12)
ggsave(files$g4_4, width = 12, height = 12)

##==== Evolución por sexo ====##
cenapi %>% 
  filter(fuente == "RNPDNO" & sexo != "Indeterminado" & year >= 2010) %>% 
  group_by(year, sexo) %>% 
  summarise(tot = sum(tot, na.rm = T)) %>% 
  mutate(tot = ifelse(year == 2020, anualizar(tot, mes_anual), tot)) %>% 
  {
    ggplot(data = ., aes(x = year, y = tot, color = sexo)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      scale_color_manual(values = c("#f95738", "#0d3b66")) +
      scale_x_continuous(breaks = seq(from=min(.$year), to=max(.$year), by=1)) +
      scale_y_continuous(breaks = seq(from=0, to=max(.$tot)+5, by=50)) +
      labs(title = "Personas desaparecidas, no localizadas y localizadas en Morelos",
           subtitle = "Por sexo de 2010 a 2020",
           caption = "Fuente: Elaboración propia con datos del RNPDNO.\nRNPDNO 2020 anualizados.",
           x = "", y = "Total de registros", color = " ") +
      tema +
      theme(legend.position = "top",
            axis.text.x = element_text(size = 12, face = "bold")) 
  }
  
ggsave(files$g5, width = 12, height = 12)
ggsave(files$g5_5, width = 12, height = 12)

##==== SESNSP ====##
sesnsp %>% 
  filter(subtipo_de_delito != "Tráfico de menores") %>% 
  mutate(delito = case_when(subtipo_de_delito %in% c("Feminicidio", "Homicidio doloso") ~ "Homicidio + Feminicidio",
                            T ~ subtipo_de_delito)) %>%
  group_by(year, municipio, delito) %>% 
  mutate(total = ifelse(year == 2020, anualizar(tot, mes_anual), tot),
         tasa = round(total*(100000/pob), 2),
         nombre = case_when(delito == "Homicidio + Feminicidio" & tasa > 60 ~ 1,
                            delito == "Otros delitos que atentan contra la libertad personal" & tasa >20 ~ 1,
                            delito == "Trata de personas" & tasa > 0.8 ~ 1)) %>%
  {
    ggplot(data = ., aes(x = year, y = tasa)) +
      geom_point(aes(color = tasa), size = 4, shape = 20, color = "#0d3b66", position = "jitter", alpha = .5) +
      geom_label_repel(data = subset(., nombre == 1), aes(x = year, y = tasa, label = municipio), size = 2) + 
      facet_wrap(~delito, scales = "free", nrow = 3) + 
      scale_x_continuous(breaks = seq(from=min(.$year), to=max(.$year), by=1)) +
      labs(title = "Tasa de carpetas de investigación de delitos relacionados con desaparición", 
           subtitle="", y="Tasa por cada 100,000 personas", 
           x=" ", fill = " ", color = " ", 
           caption="Fuente: Elaboración propia con datos del SESNSP y CONAPO;\nSESNSP 2020 anualizado.") + 
      tema +
      guides(colour = F) 
  }

ggsave(files$g6, width = 12, height = 12)
ggsave(files$g6_6, width = 12, height = 12)
 
# done.
