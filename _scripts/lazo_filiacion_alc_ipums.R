#************************************************************************* #
# Indicadores Lien de Filiation
#
# Base des donnés : IMPUMS International
#
#************************************************************************* #

source("_scripts/Libraries_Options.R")


#************************************************************************* #
# Cargar resultados calculados en STATA ----
#************************************************************************* #

# Cargar LF1 
lf1data <- read_csv("_data/ipums_filiacion/lf1data.csv")

# Cargar LF2
lf2data <- read_csv("_data/ipums_filiacion/lf2data.csv")

# Cargar LF3
lf3data <- read_csv("_data/ipums_filiacion/lf3data.csv")

# Cargar LF3
lf4data <- read_csv("_data/ipums_filiacion/lf4data.csv")

# Fusionar bases
lfipums <- full_join(lf1data, lf2data)
lfipums <- full_join(lfipums, lf3data)
lfipums <- full_join(lfipums, lf4data)
lfipums

# Datos en porcentaje
lfipums <- lfipums %>% 
  mutate(
    lf1 = lf1 * 100,
    lf2 = lf2 * 100,
    lf3 = lf3 * 100,
    lf4 = lf4 * 100,
  )

lfipums <- lfipums %>% 
  separate(pais_ano, c("pais", "ano"), sep = "_") %>% 
  mutate(ronda =
    case_when(
      ano %in% c(1960:1968) ~ "1960",
      ano %in% c(1970:1979) ~ "1970",
      ano %in% c(1980:1989) ~ "1980",
      ano %in% c(1990:1999) ~ "1990",
      ano %in% c(2000:2009) ~ "2000",
      ano %in% c(2010:2019) ~ "2010",
    )
  )

## Recoding lfipums$pais into lfipums$code3
lfipums$code3 <- recode(lfipums$pais,
                        "Argentina" = "ARG",
                        "Bolivia" = "BOL",
                        "Brazil" = "BRA",
                        "Chile" = "CHL",
                        "Colombia" = "COL",
                        "Costa Rica" = "CRI",
                        "Dominican Republic" = "DOM",
                        "Ecuador" = "ECU",
                        "El Salvador" = "SLV",
                        "Guatemala" = "GTM",
                        "Honduras" = "HND",
                        "Mexico" = "MEX",
                        "Nicaragua" = "NIC",
                        "Panama" = "PAN",
                        "Paraguay" = "PRY",
                        "Peru" = "PER",
                        "Uruguay" = "URY",
                        "Venezuela" = "VEN"
)



lfipums <- lfipums %>% 
    unite(codan, c("code3", "ano"), sep = " ", remove = FALSE)

# Eliminar casos Mexico 1995 y 2015
lfipums <- lfipums %>% 
  filter(codan != "MEX 2015" & codan != "MEX 1995")

### BASE FINAL
lfipums <- lfipums %>% 
  # Seleecionar datos con indicadores completos
  filter(lf1 > 0 & lf2 > 0 & lf3 > 0 & lf4)


### Grafico LF1 por país y ronda ----
graf_lf1_ipums <- ggplot(lfipums) +
  aes(x = ronda,
      y = lf1,
      label = round(lf1, digits = 1)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(y = lf1 + 0.5),
            position = position_dodge(0.9), 
            vjust = 0,
            size = 2) +
  labs(title = "LF1: % 25-34 que viven con padre y/o madre",
       x = "", y = "") +
  theme_minimal() +
  facet_wrap(vars(pais),
             nrow = 6,
             ncol = 3,) +
  theme(
    text = element_text(family = "Helvetica", size = 9),
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lf1_ipums

ggsave(
  filename = "_resultados/graf_lf1_ipums.png",
  plot = graf_lf1_ipums,
  device = "png",
  scale = 1,
  width = 15,
  height = 16,
  units = "cm",
  dpi = 300
)


### Grafico LF2 por país y ronda ----
graf_lf2_ipums <- ggplot(lfipums) +
  aes(x = ronda,
      y = lf2,
      label = round(lf2, digits = 1)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(y = lf2 + 0.5),
            position = position_dodge(0.9), 
            vjust = 0,
            size = 2) +
  labs(title = "LF2: % 25-34, desempleados, que viven con padre y/o madre",
       x = "", y = "") +
  theme_minimal() +
  facet_wrap(vars(pais),
             nrow = 6,
             ncol = 3,) +
  theme(
    text = element_text(family = "Helvetica", size = 9),
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lf2_ipums

ggsave(
  filename = "_resultados/graf_lf2_ipums.png",
  plot = graf_lf2_ipums,
  device = "png",
  scale = 1,
  width = 15,
  height = 16,
  units = "cm",
  dpi = 300
)


### Grafico LF3 por país y ronda ----
graf_lf3_ipums <- ggplot(lfipums) +
  aes(x = ronda,
      y = lf3,
      label = round(lf3, digits = 1)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(y = lf3 + 0.5),
            position = position_dodge(0.9), 
            vjust = 0,
            size = 2) +
  labs(title = "LF3: % Personas de 75 años o mas que viven con algún hijo",
       x = "", y = "") +
  theme_minimal() +
  facet_wrap(vars(pais),
             nrow = 6,
             ncol = 3,) +
  theme(
    text = element_text(family = "Helvetica", size = 9),
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lf3_ipums

ggsave(
  filename = "_resultados/graf_lf3_ipums.png",
  plot = graf_lf3_ipums,
  device = "png",
  scale = 1,
  width = 15,
  height = 16,
  units = "cm",
  dpi = 300
)
### Grafico LF4 por país y ronda ----
graf_lf4_ipums <- ggplot(lfipums) +
  aes(x = ronda,
      y = lf4,
      label = round(lf4, digits = 1)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(y = lf4 + 0.5),
            position = position_dodge(0.9), 
            vjust = 0,
            size = 2) +
  labs(title = "LF4: % Personas de 75 años o mas que viven en hogares de más de dos personas",
       x = "", y = "") +
  theme_minimal() +
  facet_wrap(vars(pais),
             nrow = 6,
             ncol = 3,) +
  theme(
    text = element_text(family = "Helvetica", size = 9),
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title.y = element_text(size = 8, face = "italic"),
    axis.title.x = element_text(size = 8)
  )
graf_lf4_ipums

ggsave(
  filename = "_resultados/graf_lf4_ipums.png",
  plot = graf_lf4_ipums,
  device = "png",
  scale = 1,
  width = 15,
  height = 16,
  units = "cm",
  dpi = 300
)

### Seleccionar último registro del período para base indicadores final -----
l1_filiacion <- lfipums %>% 
  group_by(code3) %>% 
  mutate(ultimo = max(ano)) %>% 
  filter(ultimo == ano)
l1_filiacion <- ungroup(l1_filiacion)


### GUARDAR BASE Lazo de Filiacion ALC ----
write.csv(l1_filiacion, "_resultados/l1_filiacion.csv", fileEncoding = "UTF-8")
