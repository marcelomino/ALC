#************************************************************************* #
# Guatemala
# data: Encuesta Nacional de Condiciones de Vida (ENCOVI) 2014
#************************************************************************* #

source("_scripts/Libraries_Options.R")


### Importar bases de datos
# Base personas
gtm14_p <- read_sav("_data/enc_hogares/gtm14/personas.sav")
# Base Hogares
gtm14_h <- read_sav("_data/enc_hogares/gtm14/hogares.sav")

# Base procesamiento
gtm <- gtm14_p
names(gtm) <- tolower(names(gtm)) # nombres de variables en minúsculas

# Seleccionar y renombrar variables
gtm <- gtm %>% 
  select(
    numhog, # id hogar
    id, # orden de persona en hogar
    thogar, # tamaño del hogar
    factor, # factor de expansión
    ppa02, # sexo
    ppa03, # edad
    ppa05, # parentesco con jefe de hogar
    p04a01a, # padre vive en el hogar
    p04a01b, # Código del padre
    p04a04a, # Madre vive en el hogar
    p04a04b, # Código de la madre
    p10a02, #Actividad principal la semana pasada
    p10b04, # categoría ocupacional (categ)
    p10a06, # buscó trabajó durante la semana pasada
    p10a07, # buscó trabajo durante el útimo mes 
    p10e01, # Tiempo de búsqueda de trabajo (semanas)
    p10f01 # razón por la cual no trabajó la semana pasada (inactivos)
  ) %>% 
  rename(
    idhog = numhog, # id hogar
    orden = id, # orden de persona en hogar
    npers = thogar, # tamaño del hogar
    factorex = factor, # factor de expansión
    sexo = ppa02, # sexo
    edad = ppa03, # edad
    pco = ppa05, # parentesco con jefe de hogar
    padrehog = p04a01a, # padre vive en el hogar
    padrecod = p04a01b, # Código del padre
    madrehog = p04a04a, # Madre vive en el hogar
    madrecod = p04a04b, # Código de la madre
    activp = p10a02, # Actividad principal la semana pasada
    categ = p10b04, # categoría ocupacional (categ)
    buscatrab_sem = p10a06, # buscó trabajó durante la semana pasada
    buscatrab_mes = p10a07, # buscó trabajo durante el útimo mes 
    buscatrab_tiempo = p10e01, # Tiempo de búsqueda de trabajo (semanas)
    inactivraz = p10f01 # razón por la cual no trabajó la semana pasada (inactivos)
  )


### Recodificación variables ----

# Crear identificación de persona (idpers). 
# Para formatear los números de línea de "idhog" y "orden" utilizamos la funcion formatC
gtm$idhog <- formatC(gtm$idhog, width = 5, format = "d", flag = "0")
gtm$orden <- formatC(gtm$orden, width = 2, format = "d", flag = "0")
# "pegamos" las columnas idhogar y orden. 
gtm$idpers <- paste(gtm$idhog, gtm$orden, sep = "")

describe(gtm)

### _condact : indicador condición de actividad ----
gtm <- gtm %>% 
  mutate(
    condact = case_when(
      # ocupados
      categ >= 1 & categ <= 9 ~ "ocupados",
      # inactivos
      inactivraz >= 1 & inactivraz <= 98 ~ "inactivos",
      # desocupados
      buscatrab_sem == 1 | buscatrab_mes == 1 ~ "desocupados",
      # otro
      TRUE ~ "otro"
    )
  )


### _vivpar : personas que viven con sus padres ----
gtm <- gtm %>% 
  mutate(vivpar = if_else(padrehog == 1 | madrehog == 1, 1, 0)) %>% 
  replace_na(vivpar, value = 0)

### _vivhij : personas que viven con hijos ----
# seleccionar casos de personas que viven con MADRE
mam <- gtm %>%
  filter(madrecod > 0) %>% 
  select(idhog, madrecod)
  # cambiar formato de código de la madre
  mam$madrecod <- formatC(mam$madrecod, width = 2, format = "d", flag = "0")
  # crear identificador de padres id_papmam
  mam <- mam %>% 
    mutate(id_papmam = paste(idhog, madrecod, sep = "")) %>% 
    # remover duplicados 
    # (id_papmam están repetidos cuando hay más de un hijo en el  hogar)
    distinct(id_papmam, .keep_all = TRUE) 
  
  pap <- gtm %>%
    filter(padrecod > 0) %>% 
    select(idhog, padrecod)
  # cambiar formato de código del padre
  pap$padrecod <- formatC(pap$padrecod, width = 2, format = "d", flag = "0")
  # crear identificador de padres id_papmam
  pap <- pap %>% 
    mutate(id_papmam = paste(idhog, padrecod, sep = "")) %>% 
    # remover duplicados (id_papmam están repetidos cuando hay más de un hijo en el  hogar)
    distinct(id_papmam, .keep_all = TRUE) 

# Base consolidada de padres y madres 
papmam <- bind_rows(padre = pap, madre = mam, .id = "padre_madre")
papmam <- papmam %>% 
  # crear variable vivhij
  mutate(vivhij = 1) %>% 
  # renombrar id_papmam como idpers para pegar con base general
  rename(idpers = id_papmam) %>% 
  # seleccionar variables
  select(idpers, padre_madre, vivhij)

# merge variables de padre y madre
gtm <- left_join(gtm, papmam) 
# reemplazar NA por 0 en variable vivhij
gtm <- replace_na(x = gtm, vivhij, value = 0)

freq(gtm$vivhij)



### Resultados ----

code3 <- c("GTM")
gtm_lf <- data.frame(code3)

# __ LF1 ----
# % Personas de 25 a 34 años que viven con padre o madre
gtm_lf <- gtm %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # personas que viven con sus padres
  summarise(lf1 = weighted.mean(vivpar, w = factorex) * 100) %>% 
  bind_cols(gtm_lf)

# __ LF2 ----
# % Desocupados de 25 a 34 años que viven con padre o madre
gtm_lf <- gtm %>%
  # seleccionar personas de 25 - 34 años
  filter(edad >= 25 & edad <= 34) %>%
  # seleccionar personada desocupadas
  filter(condact == "desocupados") %>%
  # personas que viven con sus padres
  summarise(lf2 = weighted.mean(vivpar, w = factorex) * 100) %>%
  bind_cols(gtm_lf)

### __ LF3 ----
### % Personas de personas de 75+ que viven con alguno de sus hijos
gtm_lf <- gtm %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # personas que viven con sus hijos
  summarise(lf3 = weighted.mean(vivhij, w = factorex) * 100) %>%
  bind_cols(gtm_lf)


### __ LF4 ----
# % Personas de personas de 75+ que viven en hogares de más de dos personas
gtm_lf <- gtm %>%
  # personas de 75 años y más
  filter(edad >= 75) %>%
  # indicador de hogares de más de dos personas
  mutate(npers2 = if_else(npers > 2, 1, 0)) %>%
  # personas que viven con sus hijos
  summarise(lf4 = weighted.mean(npers2, w = factorex) * 100) %>%
  bind_cols(gtm_lf)

### Datos GTM
select(gtm_lf,
       code3, lf1, lf2, lf3, lf4)





