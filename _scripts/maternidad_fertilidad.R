
#************************************************************************* #
# Maternidad y Fertlidad américa Latina
#************************************************************************* #


### Maternidad ----
# CEPAL, a partir de Censos de Población

source("_scripts/Libraries_Options.R")

library(readxl)
maternidad <- read_excel("_data/maternidad/maternidad.xlsx")

## Recoding maternidad$pais into maternidad$cod3
maternidad$cod3 <- fct_recode(maternidad$pais,
  "ARG" = "Argentina",
  "BOL" = "Bolivia (Estado Plurinacional de)",
  "BRA" = "Brasil",
  "CHL" = "Chile",
  "COL" = "Colombia",
  "CRI" = "Costa Rica",
  "ECU" = "Ecuador",
  "SLV" = "El Salvador",
  "GTM" = "Guatemala",
  "HND" = "Honduras",
  "MEX" = "México",
  "NIC" = "Nicaragua",
  "PAN" = "Panamá",
  "PRY" = "Paraguay",
  "PER" = "Perú",
  "DOM" = "República Dominicana",
  "URY" = "Uruguay",
  "VEN" = "Venezuela (República Bolivariana de)"
)


# Grupos de edad como factor
maternidad$edad <- as.factor(maternidad$edad)

## crear variable porcentaje madres 
maternidad <- maternidad %>% 
  mutate(madres_pc = madre / total_mujeres * 100)

## Madres de 25 a 34 años por década
mater25_34 <- maternidad %>% 
  filter(edad == "25_29" | edad == "30_34") %>% 
  group_by(cod3, decada) %>% 
  summarise(
    madre = sum(madre),
    total_mujeres = sum(total_mujeres))  %>% 
  mutate(madres_pct = madre / total_mujeres * 100)
 

write_csv(mater25_34, "_resultados/tablas/maternidad mujeres 25 a 34 anos.csv")
  
  
  
