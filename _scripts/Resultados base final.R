#************************************************************************* #
# BASE INDICADORES ATTACHEMENT ALC
#************************************************************************* #

source("_scripts/Libraries_Options.R")

### Crear variables de 
liensalc <- tibble::tribble(
   ~code3,            ~nomes,
    "ARG",       "Argentina",
    "BOL",         "Bolivia",
    "BRA",          "Brasil",
    "CHL",           "Chile",
    "COL",        "Colombia",
    "CRI",      "Costa Rica",
    "DOM", "Rep. Dominicana",
    "ECU",         "Ecuador",
    "SLV",     "El Salvador",
    "GTM",       "Guatemala",
    "HND",        "Honduras",
    "MEX",          "México",
    "NIC",       "Nicaragua",
    "PAN",          "Panamá",
    "PRY",        "Paraguay",
    "PER",            "Perú",
    "URY",         "Uruguay",
    "VEN",       "Venezuela"
  )
liensalc

### Importar data lazo de FILIACION
l1_filiacion <- read_csv("_resultados/l1_filiacion.csv")
l1_filiacion <- l1_filiacion %>% 
  select(
    code3, lf1, lf2, lf3, lf4 
  )
l1_filiacion

### Importar data lazo de PARTICIPACIÓN ELECTIVA
l2_pelectiva <- read_csv("_resultados/l2_pelectiva.csv")
l2_pelectiva

### Importar data lazo de PARTICIPACIÓN ORGÁNICA
l3_porganica <- read_csv("_resultados/l3_porganica_alc.csv")
l3_porganica

### Importar data lazo de CIUDADANÍA
l4_ciudadania <- read_csv("_resultados/l4_ciudadania.csv")
l4_ciudadania <- l4_ciudadania %>% 
  select(-pais_nom)
l4_ciudadania


### Merge
liensalc <- full_join(liensalc, l1_filiacion)
liensalc <- full_join(liensalc, l2_pelectiva)
liensalc <- full_join(liensalc, l3_porganica)
liensalc <- full_join(liensalc, l4_ciudadania)

liensalc


# Crear indicadores estandarizados
# formula : ( x - min) / (max - min)
indicst <- function(x) {
  x_std <- (
    x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)
  )
  return(x_std)
}

      
liensalc <- liensalc %>% 
  mutate(
    # estand. filiacion
    slf1 = indicst(lf1),
    slf2 = indicst(lf2),
    slf3 = indicst(lf3),
    slf4 = indicst(lf4),
    # estand. part. electiva
    slpe1 = indicst(lpe1),
    slpe2 = indicst(lpe2),
    slpe1_alt = indicst(lpe1_alt),
    # estand. part. orgánica
    slpo = indicst(lpo),
    # estand. ciudadanía
    slc1 = indicst(lc1),
    slc2 = indicst(lc2)
  )
liensalc

# Crea variable índice: ind_lc     
liensalc <- liensalc %>% 
  mutate(
    ind_lf = (slf1 + slf2 + slf3 + slf4) / 4,
    ind_lpe = (slpe1 + slpe2) / 2,
    ind_lpe_alt = (slpe1_alt + slpe2) / 2,
    ind_lpo = slpo,
    ind_lc = (slc1 + slc2) / 2
    )
liensalc  

### GUARDAR base INDICADORES - ALC ----
write_csv(liensalc, "_resultados/liensalc.csv")


#### Gráfico Filiación ----
graf_lf <- liensalc %>%
  ggplot(
    aes(x = reorder(code3, -ind_lf), 
        y = ind_lf,
        label = round(ind_lf, digits = 2))
  ) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  # Agregar los value labels
  geom_text(aes(y = ind_lf + 0.02),
            position = position_dodge(0.9), 
            vjust = 0, 
            size = 3) +
  labs(title = "Lazo de Filiación",
       x = "", 
       y = "index lf") +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lf

ggsave(
  filename = "_resultados/graf_filiacion_alc.png",
  plot = graf_lf,
  device = "png",
  scale = 1,
  width = 14.5,
  height = 9,
  units = "cm",
  dpi = 300
)


#### Gráfico Participación Electiva ----
graf_lpe <- liensalc %>%
  filter(lpe1 >= 0) %>% 
  ggplot(
    aes(x = reorder(code3, -ind_lpe), 
        y = ind_lpe,
        label = round(ind_lpe, digits = 2))
  ) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  # Agregar los value labels
  geom_text(aes(y = ind_lpe + 0.02),
            position = position_dodge(0.9), 
            vjust = 0, 
            size = 3) +
  labs(title = "Lazo de Participación Electiva",
       x = "", 
       y = "index lpe",
       caption = "Indicadores: 1) Miembro activo de una asociación humanitaria (WVS wave6)
       2) Donación a una asociación caritativa el último mes (Gallup, 2015)") +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lpe

ggsave(
  filename = "_resultados/graf_pelectiva_alc.png",
  plot = graf_lpe,
  device = "png",
  scale = 1,
  width = 14.5,
  height = 9,
  units = "cm",
  dpi = 300
)

#### Gráfico Participación Electiva (alternativo)----
graf_lpe_alt <- liensalc %>%
  filter(lpe1_alt >= 0) %>% 
  ggplot(
    aes(x = reorder(code3, -ind_lpe_alt), 
        y = ind_lpe_alt,
        label = round(ind_lpe_alt, digits = 2))
  ) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  # Agregar los value labels
  geom_text(aes(y = ind_lpe_alt + 0.02),
            position = position_dodge(0.9), 
            vjust = 0, 
            size = 3) +
  labs(title = "Lazo de Participación Electiva (alternativa)",
       x = "", 
       y = "index lpe_alt",
       caption = "Indicadores: 1) Voluntariado en una organización durante el último mes (Gallup, 2015)
       2) Donación a una asociación caritativa el último mes (Gallup, 2015)") +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lpe_alt

ggsave(
  filename = "_resultados/graf_pelectiva_alternat_alc.png",
  plot = graf_lpe_alt,
  device = "png",
  scale = 1,
  width = 14.5,
  height = 9,
  units = "cm",
  dpi = 300
)

plot(liensalc$lpe1, liensalc$lpe1_alt, 
     main = "lpe1 vs lpe1_alt",
     xlab = "member humanit", ylab = "volunteered time",
     pch = 19, frame = FALSE)

#### Gráfico Participación Orgánica ----
graf_lpo <- liensalc %>%
  ggplot(
    aes(x = reorder(code3, -ind_lpo), 
        y = ind_lpo,
        label = round(ind_lpo, digits = 2)
    )
  ) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  # Agregar los value labels
  geom_text(aes(y = ind_lpo + 0.02),
            position = position_dodge(0.9), 
            vjust = 0, 
            size = 3) +
  labs(title = "Lazo de Participación Orgánica",
       x = "", y = "index lpo") +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lpo

ggsave(
  filename = "_resultados/graf_porganica_alc.png",
  plot = graf_lpo,
  device = "png",
  scale = 1,
  width = 14.5,
  height = 9,
  units = "cm",
  dpi = 300
)



#### Gráfico Ciudadanía
graf_lc <- liensalc %>%
  ggplot(
    aes(x = reorder(code3, -ind_lc), 
        y = ind_lc,
        label = round(ind_lc, digits = 2))
  ) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  # Agregar los value labels
  geom_text(aes(y = ind_lc + 0.02),
            position = position_dodge(0.9), 
            vjust = 0, 
            size = 3) +
  labs(title = "Lazo de Ciudadanía",
       x = "", y = "index lc") +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica", size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )

ggsave(
  filename = "_resultados/graf_ciudadania_alc.png",
  plot = graf_lc,
  device = "png",
  scale = 1,
  width = 14.5,
  height = 9,
  units = "cm",
  dpi = 300
)


