#************************************************************************* #
# Indicadores Lien de Citoyenneté
#
# Base des donnés : Latinobarómetro
# Date Revisión : 20 / 11 / 2020
#************************************************************************* #

source("_scripts/Libraries_Options.R")
source("_scripts/importar_latinobarometro.R")

#**************************************************************************
# 1 SELECCIONAR y RENOMBRAR VARIABLES ----
#**************************************************************************

# Variable
# Conf. interpersomnal    confinter
# Conf. Poder Judicial    confpjud
# Numero investigacion    numinves
# identificación pais     idenpa
# Ingreso sybjetivo       ysubj
# Sexo                    sexo
# Edad                    edad
# Educación entrevistado  educ
# Ponderación             wt

### 2015
lb2015 <- select(
  d$lb2015,
  idenpa, numinves,
  P15STGBS, P16ST_H,
  S4, S12, S13, REEDUC_1,
  wt
)

lb2015 <- rename(lb2015,
  confinter = P15STGBS,
  confpjud = P16ST_H,
  ysubj = S4,
  sexo = S12,
  edad = S13,
  educ = REEDUC_1
)

# 2013
names(d$lb2013)
lb2013 <- select(
  d$lb2013,
  idenpa, numinves,
  P29STGBS, P26TGB_E,
  S10, S11, S6, REEDUC_1,
  wt
)

lb2013 <- rename(lb2013,
  confinter = P29STGBS,
  confpjud = P26TGB_E,
  sexo = S10,
  edad = S11,
  educ = REEDUC_1,
  ysubj = S6,
)

# 2011
names(d$lb2011)
lb2011 <- select(
  d$lb2011,
  idenpa, numinves,
  P25ST, P22ST_B,
  S16, S17, S10ICC12, REEDUC1,
  wt
)

lb2011 <- rename(lb2011,
  confinter = P25ST,
  confpjud = P22ST_B,
  sexo = S16,
  edad = S17,
  educ = REEDUC1,
  ysubj = S10ICC12,
)


# 2010
lb2010 <- select(
  d$lb2010,
  idenpa, numinves,
  P55ST, P20ST_B,
  S7, S8, REEDUC1, S4,
  wt
)

lb2010 <- rename(lb2010,
  confinter = P55ST,
  confpjud = P20ST_B,
  sexo = S7,
  edad = S8,
  educ = REEDUC1,
  ysubj = S4,
)

#**************************************************************************
# 2 REVISIÓN Y RECODIFICACIÓN ----
#**************************************************************************
### _Recode Identificación País (idenpa) ----
lb2015$idenpa <- remove_val_labels(lb2015$idenpa)
lb2013$idenpa <- remove_val_labels(lb2013$idenpa)
lb2011$idenpa <- remove_val_labels(lb2011$idenpa)
lb2010$idenpa <- remove_val_labels(lb2010$idenpa)

### _Número Investigación ----
lb2015$numinves <- as.numeric(to_character(lb2015$numinves))
lb2013$numinves <- as.numeric(to_character(lb2013$numinves))
lb2011$numinves <- as.numeric(to_character(lb2011$numinves))
lb2010$numinves <- as.numeric(to_character(lb2010$numinves))


### _Confianza Interpersonal ----
## Códigos utilizados en encuestas
## 2015
# "Se puede confiar en la mayoría de las personas" = "1",
# "Uno nunca es lo suficientemente cuidadoso en el trato con los demás" = "2",
# "NS / NR" = "-1"
## 2010
# 1.- Se puede confiar en la mayoría de las personas
# 2 Uno nunca es lo suficientemente cuidadoso en el trato con los demás
# -2 No sabe/No contesta

# Transforma valores labelled en numeric
lb2015$confinter <- remove_val_labels(lb2015$confinter)
lb2013$confinter <- remove_val_labels(lb2013$confinter)
lb2011$confinter <- remove_val_labels(lb2011$confinter)
lb2010$confinter <- remove_val_labels(lb2010$confinter)

### _Confianza en el Poder Judicial ----
# Transforma valores labelled en numeric
lb2015$confpjud <- remove_val_labels(lb2015$confpjud)
lb2013$confpjud <- remove_val_labels(lb2013$confpjud)
lb2011$confpjud <- remove_val_labels(lb2011$confpjud)
lb2010$confpjud <- remove_val_labels(lb2010$confpjud)

#**************************************************************************
# 3 Crear BBDD agregada----
#**************************************************************************
varsfin <- c("idenpa", "numinves", "confinter", "confpjud", "wt")

# variables finales
lb2010 <- select(lb2010, all_of(varsfin))
lb2011 <- select(lb2011, all_of(varsfin))
lb2013 <- select(lb2013, all_of(varsfin))
lb2015 <- select(lb2015, all_of(varsfin))

## Combrinar casos (bind_rows)
lb <- bind_rows(lb2010, lb2011, lb2013, lb2015)

## Eliminar casos de España
lb <- filter(lb, idenpa != 724)

## Códigos de país
# Reordering levels
lb$idenpa <- factor(lb$idenpa,
  levels = c(
    "32", "68", "76", "152", "170", "188", "214", "218", "222",
    "320", "340", "484", "558", "591", "600", "604", "858", "862"
  )
)
# Recoding idenpa
lb$code3 <- fct_recode(lb$idenpa,
  "ARG" =       "32",
  "BOL" =       "68",
  "BRA" =       "76",
  "CHL" =       "152",
  "COL" =       "170",
  "CRI" =       "188",
  "DOM" =       "214",
  "ECU" =       "218",
  "SLV" =       "222",
  "GTM" =       "320",
  "HND" =       "340",
  "MEX" =       "484",
  "NIC" =       "558",
  "PAN" =       "591",
  "PRY" =       "600",
  "PER" =       "604",
  "URY" =       "858",
  "VEN" =       "862"
)

## Recoding lb$code3 into lb$pais_nom
lb$pais_nom <- fct_recode(lb$code3,
  "Argentina" = "ARG",
  "Bolivia" = "BOL",
  "Brasil" = "BRA",
  "Chile" = "CHL",
  "Colombia" = "COL",
  "Costa Rica" = "CRI",
  "Rep. Dominicana" = "DOM",
  "Ecuador" = "ECU",
  "El Salvador" = "SLV",
  "Guatemala" = "GTM",
  "Honduras" = "HND",
  "México" = "MEX",
  "Nicaragua" = "NIC",
  "Panamá" = "PAN",
  "Paraguay" = "PRY",
  "Perú" = "PER",
  "Uruguay" = "URY",
  "Venezuela" = "VEN"
)


#**************************************************************************
# 4 RESULTADOS ----
#**************************************************************************

### _LC1 : Porcentaje de personas que afirma que se puede confiar en la mayorí de las personas ----
# Crear variable indicador confianza interpersonal (i_confinter)
lb <- lb %>% mutate(
  i_confinter = if_else(confinter == 1, 1, 0)
)
table(lb$code3, lb$i_confinter)

### _LC2 : Porcentaje de personas que tiene confianza en el poder judicial (Mucha + Algo de Confianza) ----
# Crear variable indicador confianza interpersonal (i_confinter)
lb <- lb %>% mutate(
  i_confpjud = if_else(confpjud == 1 | confpjud == 2, 1, 0)
)

### Lazo de ciudadanía (2010 - 2015) ----
## Indicadores en porcentaje, por año
l4_ciudadania_2010_2015 <- lb %>%
  group_by(numinves, code3, pais_nom) %>%
  summarise(
    lc1 = wtd.mean(i_confinter, weights = wt) * 100, # datos ponderados por wt
    lc2 = wtd.mean(i_confpjud, weights = wt) * 100 # datos ponderados por wt
  )
l4_ciudadania_2010_2015

## Indicadores en porcentaje, agregado 2010 - 2015
l4_ciudadania_agg_2010_2015 <- lb %>%
  group_by(code3, pais_nom) %>%
  summarise(
    lc1 = wtd.mean(i_confinter, weights = wt) * 100, # datos ponderados por wt
    lc2 = wtd.mean(i_confpjud, weights = wt) * 100 # datos ponderados por wt
  )
l4_ciudadania_agg_2010_2015

## LC1 2010 - 2015
lc1 <- l4_ciudadania_2010_2015 %>% 
  select(-lc2) %>% 
  pivot_wider(names_from = "numinves", values_from = "lc1")
lc1
## LC2 2010 - 2015
lc2 <- l4_ciudadania_2010_2015 %>% 
  select(-lc1) %>% 
  pivot_wider(names_from = "numinves", values_from = "lc2")
lc2

### GUARDAR BASE Lazo de Ciudadanía ALC ----
write.csv(l4_ciudadania_agg_2010_2015, "_resultados/l4_ciudadania.csv", fileEncoding = "UTF-8")
write.csv(lc1, "_resultados/l4_ciudadania_serie_lc1.csv", fileEncoding = "UTF-8")
write.csv(lc2, "_resultados/l4_ciudadania_serie_lc2.csv", fileEncoding = "UTF-8")




