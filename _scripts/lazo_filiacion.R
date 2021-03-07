#************************************************************************* #
# Indicadores Lien de Filiation
#
# Base des donnés : Encuestas de Hogares
#
#************************************************************************* #

source("_scripts/Libraries_Options.R")

#************************************************************************* #
# ARGENTINA ----
# data: Encuesta Permanente de hogares (EPH) - 2014, 4to Trimestre
#************************************************************************* #

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

### _arg LF1 ----
### % Personas de 25 a 34 años que viven con padre o madre

## Identificar grupo edad (25-34)
arg2014 <- arg2014 %>% mutate(
  edad25_34 = if_else(edad >= 25 & edad <= 34, "25-34", "otro")
)

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

### RESULTADO LF1
lf1_arg <- as.data.frame(
  lprop(wtd.table(arg2014$edad25_34, arg2014$vivpar, weights = arg2014$factorex))
)

### _arg LF2 ----
### % Personas de 25 a 34 años que viven con padre o madre

# Identificar personas de 25-34 que están desocupados
arg2014 <- arg2014 %>% mutate(
  desocup_25_34 = if_else(edad >= 25 & edad <= 34
  & condact == 2, "Desocupado 25-34", "otro")
)

### RESULTADO LF2
lf2_arg <- as.data.frame(
  lprop(wtd.table(arg2014$desocup_25_34, arg2014$vivpar,
    weights = arg2014$factorex
  ))
)
lf2_arg

### _arg LF3 ----
### % Personas de personas de 75 años y más viviendo con uno de sus hijos

# Identificar personas de 75 años y más
arg2014 <- arg2014 %>% mutate(
  edad_75 = if_else(edad >= 75 & edad <= 98, "75 y más", "otro")
)

# vivhij = personas viviendo con hijos
arg2014 <- arg2014 %>%
  mutate(vivhij = case_when(
    # jefe de hogar con hijos presentes
    pco == 1 & nhijos_jh >= 1 ~ 1,
    # pareja de jefe de hogar, si hijos de jefe de hogar presentes (suegros)
    pco == 2 & nhijos_jh >= 1 ~ 1,
    # hijos del jefe de hogar mayores de 15 años, si nietos presentes
    pco == 3 & edad >= 15 & nnietos_jh >= 1 ~ 1,
    # Nueras o yernos de jefe de hogar, si nietos presentes,
    pco == 4 & edad >= 15 & nnietos_jh >= 1 ~ 1,
    ## Padres o Madres del jefe de hogar
    pco == 6 ~ 1,
    # Suegros de jefe de hogar, si pareja de jefe de hogar presente
    pco == 8 & npareja_jh >= 1 ~ 1,
    TRUE ~ 0
  ))


### RESULTADO LF3
lf3_arg <- as.data.frame(
  lprop(wtd.table(arg2014$edad_75, arg2014$vivhij,
    weights = arg2014$factorex
  ))
)
lf3_arg


### _arg LF4 ----
# Personas de 75+ viviendo en un hogar de más de dos personas

# Identifica hogares con dos personas o más
arg2014 <- arg2014 %>% mutate(
  n2pers = if_else(npers > 2, 1, 0)
)

### RESULTADO LF4
lf4_arg <- as.data.frame(
  lprop(wtd.table(arg2014$edad_75, arg2014$n2pers,
    weights = arg2014$factorex
  ))
)
lf4_arg

#************************************************************************* #
# MEXICO ----
# data: ENIGH 2014
#************************************************************************* #

### Importar bases de datos
# base personas
mex14_pers <- read_csv("_data/enc_hogares/mex14/poblacion_2014_csv.zip")
# base hogares concentrado
mex14_hogc <- read_csv("_data/enc_hogares/mex14/concentrado_2014_csv.zip")


### Seleccionar y renombrar variables

### Variables base personas (poblacion)
mex14_pers <- mex14_pers %>%
  select(
    folioviv, foliohog, numren,
    parentesco, sexo, edad,
    madre_hog, madre_id,
    padre_hog, padre_id,
    trabajo_mp, motivo_aus,
    act_pnea1, act_pnea2,
    num_trabaj
  )
# Crear variable id_hogar
mex14_pers$idhog <- paste(
  mex14_pers$folioviv, mex14_pers$foliohog,
  sep = ""
)

### Variables base hogares (concentrado)
mex14_hogc <- mex14_hogc %>%
  select(
    folioviv, foliohog,
    factor_hog, tot_integ
  )
# Crear variable id_hogar
mex14_hogc$idhog <- paste(
  mex14_hogc$folioviv, mex14_hogc$foliohog,
  sep = ""
)

## Merge variables de hogar en base personas
mex14 <- left_join(mex14_pers, mex14_hogc)

# crear id persona (idpers)
mex14$idpers <- paste(
  mex14$idhog, mex14$numren,
  sep = ""
)

### Recodificación variables ----

## Recoding mex14$sexo into mex14$sexo_rec
mex14$sexo_rec <- as.character(mex14$sexo)
mex14$sexo_rec <- fct_recode(mex14$sexo_rec,
  "Mujer" = "2",
  "Hombre" = "1"
)

## Recoding mex14$trabajo_mp into mex14$trabajo_mp_rec
mex14$trabajo_mp_rec <- as.character(mex14$trabajo_mp)
mex14$trabajo_mp_rec <- fct_recode(mex14$trabajo_mp_rec,
  "No trabajó" = "2",
  "Trabajó" = "1"
)
mex14$trabajo_mp_rec <- fct_explicit_na(mex14$trabajo_mp_rec, "No aplica")

# edad 25-34
mex14 <- mex14 %>% mutate(
  edad25_34 = if_else(edad >= 25 & edad <= 34, 1, 0)
)

table(mex14$edad, mex14$edad25_34)

### _condact : indicador condición de actividad ----
mex14 <- mex14 %>%
  mutate(
    condact = case_when(
      trabajo_mp == 2 & act_pnea1 == 1 ~ "desocupados",
      act_pnea1 >= 2 & act_pnea1 <= 5 ~ "inactivos",
      act_pnea1 == 6 ~ "otro",
      trabajo_mp == 1 ~ "ocupados",
      edad < 15 ~ "menor de 15",
      TRUE ~ "sin clasif"
    )
  )

### _vivpar : personas que habitan con madre o padre ----
mex14 <- mex14 %>% mutate(
  vivpar = if_else(madre_hog == 1 | padre_hog == 1, 1, 0)
)

### _vivhij : personas que viven con hijos ----
# identificar madres
mam <- mex14 %>%
  # filtrar casos de personas que declaron vivir con madre
  filter(madre_hog == 1) %>%
  select(idhog, idpers, parentesco, madre_hog, madre_id, tot_integ) %>%
  # Creamos un identificador de madres / padres (id_papmam)
  mutate(id_papmam = paste(idhog, madre_id, sep = "")) %>%
  # remover duplicados (id_papmam están repetidos cuando hay más de un hijo en el hogar)
  distinct(id_papmam, .keep_all = TRUE) %>%
  # crear variable indicador de la persona vive con hijo (vivhij)
  mutate(vivhij = 1)
# identificar padres
pap <- mex14 %>%
  # filtrar casos de personas que declaron vivir con madre
  filter(padre_hog == 1) %>%
  select(idhog, idpers, parentesco, padre_hog, padre_id, tot_integ) %>%
  # Creamos un identificador de madres / padres (id_papmam)
  mutate(id_papmam = paste(idhog, padre_id, sep = "")) %>%
  # remover duplicados (id_papmam están repetidos cuando hay más de un hijo en el  hogar)
  distinct(id_papmam, .keep_all = TRUE) %>%
  # crear variable indicador que la persona vive con hijo (vivhij)
  mutate(vivhij = 1)
# base consolidada de padres y madres
papmam <- bind_rows(padre = pap, madre = mam, .id = "padre_madre")

# seleccionar variables de la base de padres y madres
papmam <- papmam %>%
  # asignamos identificador de padre o madre como idpers para fusionar las tablas
  mutate(idpers = id_papmam) %>%
  # selección de variables para fusionar tablas
  select(idpers, vivhij, padre_madre)
## Merge variables de padres/madres en base completa
mex14 <- left_join(mex14, papmam)

# asignar valor 0 a casos que no viven con hijos
mex14 <- replace_na(x = mex14, vivhij, value = 0)


### _Resultados ----

code3 <- c("MEX")
mex_lf <- data.frame(code3)

# __ LF1 ----
# % Personas de 25 a 34 años que viven con padre o madre
mex_lf <- mex14 %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # seleccionar personada desocupadas
  # personas que viven con sus padres
  summarise(lf1 = weighted.mean(vivpar, w = factor_hog) * 100) %>%
  bind_cols(mex_lf)

# __ LF2 ----
# % Desocupados de 25 a 34 años que viven con padre o madre
mex_lf <- mex14 %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # seleccionar personada desocupadas
  filter(condact == "desocupados") %>%
  # personas que viven con sus padres
  summarise(lf2 = weighted.mean(vivpar, w = factor_hog) * 100) %>%
  bind_cols(mex_lf)

### __ LF3 ----
### % Personas de personas de 75+ que viven con alguno de sus hijos
mex_lf <- mex14 %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # personas que viven con sus hijos
  summarise(
    lf3 = weighted.mean(vivhij, w = factor_hog) * 100
  ) %>%
  bind_cols(mex_lf)


### __ LF4 ----
# % Personas de personas de 75+ que viven en hogares de más de dos personas
mex_lf <- mex14 %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # indicador de hogares de más de dos personas
  mutate(npers2 = if_else(tot_integ > 2, 1, 0)) %>%
  # personas que viven con sus hijos
  summarise(lf4 = weighted.mean(npers2, w = factor_hog) * 100) %>%
  bind_cols(mex_lf)

### Datos MEX
mex_lf


#************************************************************************* #
# URUGUAY ----
# data: Encuesta Continua de Hogares (ECH) 2015
#************************************************************************* #

### Importar bases de datos
ury15 <- read_sav("_data/enc_hogares/ury15/ECH 2015 Formato sav/HyP_2015_Terceros.sav")


# Seleccionar variables
ury <- ury15 %>%
  select(
    numero, # número de hogar
    nper, # número de la persona
    pesoano, # fact. expansión año
    pesotri, # fact. expansión trimestre
    pesosem, # fact. expansión semana
    pesomen, # fact. expansión mes
    d25, # n personas en el hogar
    e26, # sexo
    e27, # edad
    e30, # parentesco (con jefe de hogar)
    e31, # Madre del nieto/a u otro pariente/no pariente (<18a)
    e32, # Padre del nieto/a u otro pariente/no pariente (<18a)
    e186_1, # Cantidad de hijos (nacidos vivos) que viven en el hogar
    pobpcoac
  ) # condición de actividad
names(ury)
# renombrar variables
ury <- ury %>%
  rename(
    idhog = numero,
    orden = nper,
    factorex = pesoano,
    npers = d25,
    sexo = e26,
    edad = e27,
    pco = e30,
    mam_nieto_otro = e31,
    pap_nieto_otro = e32,
    muj_nhij = e186_1
  )

### Recodificación variables ----

### _condact : indicador condición de actividad ----
ury <- ury %>%
  mutate(
    condact = case_when(
      pobpcoac == 2 ~ "ocupados",
      pobpcoac >= 3 & pobpcoac <= 5 ~ "desocupados",
      pobpcoac >= 6 ~ "inactivos",
      edad < 15 ~ "14 años o menos",
      TRUE ~ "otro"
    )
  )


### _vivpar : personas de 25 a 24 que viven con sus padres ----
##  Padres, suegros, pareja e hijos del jefe de hogar presentes
ury <- ury %>%
  # identificar hijos jefe de hogar
  # Se consideran hijos del jefe y de su pareja
  mutate(hijos_jh = if_else(pco %in% c(3:5), 1, 0)) %>%
  # Agrupar por hogar para contar personas según parentesco
  group_by(idhog) %>%
  mutate(
    nhijos_jh = sum(hijos_jh == 1), # n hijos jefe de hogar
    npadres_jh = sum(pco == 7), # n padres jefe de hogar
    nsuegros_jh = sum(pco == 8), # n suegros jefe de hogar
    nnietos_jh = sum(pco == 11) # n nietos jefe de hogar
  )
# Desagrupar base
ury <- ungroup(ury)

## calculo del indicador vivpar
ury <- ury %>%
  mutate(vivpar = case_when(
    # jefe de hogar, si padres en hogar
    pco == 1 & npadres_jh >= 1 ~ 1,
    # pareja de jefe de hogar, si suegros presentes
    pco == 2 & nsuegros_jh >= 1 ~ 1,
    # hijos del jefe de hogar, de la pareja o de ambos
    hijos_jh == 1 ~ 1,
    # hermano del jefe de hogar, si padres del jefe presentes
    pco == 9 & npadres_jh >= 1 ~ 1,
    # nietos del jefe de hogar
    pco == 11 
    & ((mam_nieto_otro > 0 & mam_nieto_otro < 99)
       | (pap_nieto_otro > 0 & pap_nieto_otro < 99)) 
    ~ 1,
    # otros parientes y no parientes
    pco %in% c(12,13) 
    & ((mam_nieto_otro > 0 & mam_nieto_otro < 99)
       | (pap_nieto_otro > 0 & pap_nieto_otro < 99)) 
    ~ 1,
    # resto de casos
    TRUE ~ 0
    )
  )


