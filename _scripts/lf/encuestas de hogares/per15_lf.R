#*************************************************************************
# Perú (PER)
# data: Encuesta Nacional de hogares (ENAHO) 2015
#************************************************************************* #

source("_scripts/Libraries_Options.R")

### Importar bases de datos
# Base personas
per15_p <- read_dta("_data/enc_hogares/per15/498_Modulo02 - personas/enaho01-2015-200.dta")
# Base Hogares
per15_h <- read_dta("_data/enc_hogares/per15/498-Modulo01 - hogar/enaho01-2015-100.dta")
# Base módulo empleo
per15_emping_p <- read_dta("_data/enc_hogares/per15/498-Modulo05 - empleo e ingresos/enaho01a-2015-500.dta")

# base procesamiento
per <- per15_p 
names(per)

# Seleccionar y renombrar variables
per <- per %>% 
  select(
    conglome, # número de conglomerado
    vivienda, # número de vivienda
    hogar, # número de hogar
    codperso, # número de orden de la persona
    p203a, # número de núcleo familiar
    p203, # relación de parentesco con jefe de hogar
    p203b, # relación de parentesco con jefe de núcleo familiar
    p207, # sexo
    p208a, # edad
    facpob07 # factor de población anual
  ) %>% 
  rename(
    orden = codperso, # número de orden de la persona
    nucleo = p203a, # número de núcleo familiar
    pco1 = p203, # relación de parentesco con jefe de hogar
    pco2 = p203b, # relación de parentesco con jefe de núcleo familiar
    sexo = p207, # sexo
    edad = p208a, # edad
    factorex = facpob07 # factor de población anual
  )

# Crear identificación de vivienda (idvivienda). 
per$idvivienda <- paste(per$conglome, per$vivienda, sep = "")
# Crear identificación de hogar (idhog). 
per$idhog <- paste(per$conglome, per$vivienda, per$hogar, sep = "")
# Crear identificación de persona (idpers). 
per$idpers <- paste(per$idhog, per$orden, sep = "")
# Crear identificación de nucleo (idnucleo).

per$idpers <- paste(per$idhog, per$nucleo, sep = "")

describe(per)

freq(per$idvivienda)
freq(per15_p$p204)

freq(per15_h$result)

select(per, idhog, idpers, nucleo)

