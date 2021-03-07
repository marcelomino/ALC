#************************************************************************* #
# Indicadores Lien de Filiation
#
# ARGENTINA
# data: Encuesta Permanente de hogares (EPH) - 2014, 4to Trimestre
#************************************************************************* #

source("_scripts/Libraries_Options.R")

# Importar data personas
arg2014_pers <- read_dta(
  "_data/enc_hogares/arg14/arg2014_4trimestre_individual.dta"
)
# Importar data hogares
arg2014_hog <- read_dta(
  "_data/enc_hogares/arg14/arg2014_4trimestre_hogar.dta"
)

### Seleccionar y renombrar variables
## Variables base hogares
arg2014_hog <- arg2014_hog %>%
  select(CODUSU, nro_hogar, ix_tot, pondera) %>%
  rename(
    idviv = CODUSU, # id vivienda
    factorex = pondera, # factor de expansión
    npers = ix_tot
  ) # número de personas en el hogar

# Crear variable id_hogar
arg2014_hog$idhog <- paste(
  arg2014_hog$idviv, arg2014_hog$nro_hogar,
  sep = ""
)


## Variables base personas
arg2014_pers <- arg2014_pers %>%
  select(
    CODUSU,
    nro_hogar,
    pondera,
    ch03,
    ch06,
    estado
  ) %>%
  rename(
    idviv = CODUSU, # id vivienda
    factorex = pondera, # factor de expansión
    pco = ch03, # relación de parentezco (hogar)
    edad = ch06, # edad en años
    condact = estado
  ) # condición de actividad

# Crear variable id_hogar
arg2014_pers$idhog <- paste(
  arg2014_pers$idviv, arg2014_pers$nro_hogar,
  sep = ""
)

## Merge variables de hogar en base personas
arg2014 <- left_join(arg2014_pers, arg2014_hog)



## Identificar hijos del jefe de hogar mayores de 15 años o más
arg2014 <- arg2014 %>% mutate(
  hijo15_jh = if_else(edad >= 15 & pco == 3, 1, 0)
)

## Contar numero de padres, suegros e hijos del jefe de hogar presentes
arg2014 <- arg2014 %>%
  group_by(idhog) %>%
  mutate(npadres_jh = sum(pco == 6)) %>% # n padres jefe de hogar
  mutate(nsuegros_jh = sum(pco == 7)) %>% # n suegros jefe de hogar
  mutate(nhijos_jh = sum(pco == 3)) %>% # n hijos jefe de hogar %>%
  mutate(nhijos15_jh = sum(hijo15_jh == 1)) %>% # n hijos que de jefe de hogar que tienen más de 15 años
  mutate(nnietos_jh = sum(pco == 5)) %>% # n nietos jefe de hogar
  mutate(npareja_jh = sum(pco == 2)) # n parejas del jefe de hogar

arg2014 <- ungroup(arg2014)

# vivpar = personas viviendo con padre o madre
arg2014 <- arg2014 %>%
  mutate(vivpar = case_when(
    # jefe de hogar con padres en hogar
    pco == 1 & npadres_jh >= 1 ~ 1,
    # pareja de jefe de hogar, si suegros presentes
    pco == 2 & nsuegros_jh >= 1 ~ 1,
    # hijos del jefe de hogar
    pco == 3 ~ 1,
    # # Nietos jefe de hogar, si en el hogar hay hijos del jefe de hogar que tienen más de 15 años
    pco == 5 & nhijos15_jh >= 1 ~ 1,
    # Hermanos de jefe de hogar, si en hogar hay padres del jefe de hogar
    pco == 8 & npadres_jh >= 1 ~ 1,
    TRUE ~ 0
  ))


# vivhij = personas viviendo con hijos
arg2014 <- arg2014 %>%
  mutate(vivhij = case_when(
    # jefe de hogar con hijos presentes
    pco == 1 & nhijos_jh >= 1 ~ 1,
    # pareja de jefe de hogar, si hijos de jefe de hogar presentes
    pco == 2 & nhijos_jh >= 1 ~ 1,
    # hijos del jefe de hogar mayores de 15 años, si nietos presentes
    pco == 3 & edad >= 15 & nnietos_jh >= 1 ~ 1,
    # Nueras o yernos de jefe de hogar mayores de 15 años, si nietos presentes,
    pco == 4 & edad >= 15 & nnietos_jh >= 1 ~ 1,
    ## Padres o Madres del jefe de hogar
    pco == 6 ~ 1,
    # Suegros de jefe de hogar, si pareja de jefe de hogar presente
    pco == 8 & npareja_jh >= 1 ~ 1,
    TRUE ~ 0
  )
  )

arg <- unlabelled(arg2014)

### _Resultados ----

code3 <- c("ARG")
arg_lf <- data.frame(code3)

# __ LF1 ----
# % Personas de 25 a 34 años que viven con padre o madre
arg_lf <- arg %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # personas que viven con sus padres
  summarise(lf1 = weighted.mean(vivpar, w = factorex) * 100) %>%
  bind_cols(arg_lf)

# __ LF2 ----
# % Desocupados de 25 a 34 años que viven con padre o madre
arg_lf <- arg %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # seleccionar personada desocupadas
  filter(condact == "Desocupado") %>%
  # personas que viven con sus padres
  summarise(lf2 = weighted.mean(vivpar, w = factorex) * 100) %>%
  bind_cols(arg_lf)

### __ LF3 ----
### % Personas de personas de 75+ que viven con alguno de sus hijos
arg_lf <- arg %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # personas que viven con sus hijos
  summarise(lf3 = weighted.mean(vivhij, w = factorex) * 100) %>%
  bind_cols(arg_lf)

### __ LF4 ----
# % Personas de personas de 75+ que viven en hogares de más de dos personas
arg_lf <- arg %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # indicador de hogares de más de dos personas
  mutate(npers2 = if_else(npers > 2, 1, 0)) %>%
  # personas que viven con sus hijos
  summarise(lf4 = weighted.mean(npers2, w = factorex) * 100) %>%
  bind_cols(arg_lf)

### Datos ARG
arg_lf <- select(arg_lf,
       code3, lf1, lf2, lf3, lf4)

arg_lf

table(arg$edad, arg$condact)
