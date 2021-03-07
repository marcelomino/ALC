#************************************************************************* #
# MEXICO 
# data: ENIGH 2014
#************************************************************************* #

source("_scripts/Libraries_Options.R")

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
      edad < 15 ~ "14 años o menos",
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
