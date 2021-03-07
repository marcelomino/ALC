#*************************************************************************
# CHILE (CHL)
# data: Encuesta de Carcterización Socieconómica (CASEN) 2015
#************************************************************************* #

source("_scripts/Libraries_Options.R")

casen2015 <- read_dta("_data/enc_hogares/chl15/casen2015.dta")

chl <- casen2015

names(chl)

chl <- chl %>% 
  select(
    folio,
    o, # orden de persona en el hogar
    hogar,
    nucleo,
    expr, # factor de expansión regional
    numper, # número de personas en el hogar
    sexo,
    edad,
    pco1, # parentesco con el jefe de hogar
    pco2, # parentesco con el jefe de nucleo 
    activ # condición de actividad
  ) %>% 
  rename(
    idhog = folio,
    orden = o,
    factorex = expr,
    condact = activ
  )

describe(chl)


### Recodificación variables ----

# Crear identificación de persona (idpers). 
# Para formatear los número de línea de "orden" utilizamos la funcion formatC
chl$orden <- formatC(chl$orden, width = 2, format = "d", flag = "0")
# "pegamos" las columnas idhogar y orden. 
chl$idpers <- paste(chl$idhog, chl$orden, sep = "")


### _vivpar : personas que viven con sus padres ----

