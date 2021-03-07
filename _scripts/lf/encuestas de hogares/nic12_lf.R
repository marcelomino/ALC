#*************************************************************************
# Nicaragua (NIC)
# data: Encuesta Continua de hogares (ENAHO) 2015
#************************************************************************* #

source("_scripts/Libraries_Options.R")

### Importar bases de datos
# Base personas
nic12 <- read_sav("_data/enc_hogares/nic12/DATOS SOCIODEMOGRAFICOS OCTUBRE 1412-DICIEMBRE3412.sav")

# Base procesamiento
nic <- nic12
names(nic) <- tolower(names(nic)) # nombres de variables en minúsculas

# Seleccionar y renombrar variables
nic <- nic %>% 
  select(
    id, 
    idupm,
    idvivienda,
    s05p04, # número de personas en el hogar
    s07p00, # Código de la persona
    s07p08, # Parentesco con el jefe de hogar
    s07p09, # Sexo
    s07p10, # Edad
    pet, # condición de actividad (población en edad de trabajar)
    fajustexproyeccion, # factor de expansión
  ) %>% 
  rename(
    idhog = id, 
    npers = s05p04, # número de personas en el hogar
    orden = s07p00, # Código de la persona
    pco = s07p08, # Parentesco con el jefe de hogar
    sexo = s07p09, # Sexo
    edad = s07p10, # Edad
    condact = pet, # condición de actividad (población en edad de trabajar)
    factorex = fajustexproyeccion, # factor de expansión
  )


### Recodificación variables ----

# Crear identificación de persona (idpers). 
# Para formatear los números de línea de "idhog" y "orden" utilizamos la funcion formatC
nic$idhog <- formatC(nic$idhog, width = 4, format = "d", flag = "0")
nic$orden <- formatC(nic$orden, width = 2, format = "d", flag = "0")
# "pegamos" las columnas idhogar y orden. 
nic$idpers <- paste(nic$idhog, nic$orden, sep = "")

### _vivpar : personas que viven con sus padres ----
nic <- nic %>% 
  mutate(vivpar = if_else(padrehog == 1 | madrehog == 1, 1, 0)) %>% 
  replace_na(vivpar, value = 0)







