#************************************************************************* #
# URUGUAY
# data: Encuesta Continua de Hogares (ECH) 2015
#************************************************************************* #

source("_scripts/Libraries_Options.R")


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

# Crear identificación de persona (idpers). 
# Para formatear los número de línea de "orden" utilizamos la funcion formatC
ury$orden <- formatC(ury$orden, width = 2, format = "d", flag = "0")
# "pegamos" las columnas idhogar y orden. 
ury$idpers <- paste(ury$idhog, ury$orden, sep = "")

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


### _vivpar : personas que viven con sus padres ----
ury <- ury %>%
  # identificar hijos jefe de hogar. Se consideran hijos del jefe y de su pareja
  mutate(hijos_jh = if_else(pco %in% c(3:5), 1, 0)) %>%
  # Agrupar por hogar para contar personas según parentesco
  group_by(idhog) %>%
  mutate(
    npareja_jh = sum(pco == 2), # n parejas jefe de hogar
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

### _vivhij : personas que viven con hijos ----

# indicador madres (vivhij1)
# La encuesta incluye una pregunta que permite identificar a las madres de 14 años y más que viven con sus hijos. No hay una pregunta similar para el caso de los hombres. 
ury <-  ury %>% 
  mutate(vivhij1 = if_else(muj_nhij > 0, 1, 0)) 

# indicador padres o madres (vivhij2), a partir de variable parentesco (pco)
ury <- ury %>% 
  mutate(vivhij2 = case_when(
    # jefes de hogar con hijos presentes
    pco == 1 & nhijos_jh >= 1 ~ 1,
    # pareja de jefe de hogar, si hijos de jefe de hogar presentes
    pco == 2 & nhijos_jh >= 1 ~ 1,
    # padres del jefe de hogar
    pco == 7 ~ 1,
    # suegros del jefe de hogar, si pareja del jefe de hogar presente
    pco == 8 & npareja_jh >= 1 ~ 1,
    TRUE ~ 0
    )
  )

# Indicador padres o madres (vivhij3), a partir de variables de maternidad / paternidad de nietos, otros parientes y no parientes.

# identificamos a las MADRES de los casos de nietos y otros parientes/no parientes
mam_otros <- ury %>% 
  # seleccionar variables
  select(idhog, orden, pco, mam_nieto_otro) %>% 
  # seleccionar casos de Nietos, otros parientes y no parientes
  filter(pco %in% c(11,12,13)) %>% 
  # Casos que viven con madre
  filter(mam_nieto_otro >= 1 & mam_nieto_otro < 99) 
  # formato de variable número de madre
  mam_otros$mam_nieto_otro <- formatC(mam_otros$mam_nieto_otro, 
                                      width = 2, format = "d", flag = "0")
  # Crear un identificador de madres (id_papmam)
  mam_otros <- mam_otros %>% 
  mutate(id_papmam = paste(idhog, mam_nieto_otro, sep = "")) %>% 
  # remover duplicados (id_papmam están repetidos cuando hay más de un hijo en el  hogar)
  distinct(id_papmam, .keep_all = TRUE) 

# identificamos a los PADRES de los casos de nietos y otros parientes/no parientes
  pap_otros <- ury %>% 
    # seleccionar variables
    select(idhog, orden, pco, pap_nieto_otro) %>% 
    # seleccionar casos de Nietos, otros parientes y no parientes
    filter(pco %in% c(11,12,13)) %>% 
    # Casos que viven con padre
    filter(pap_nieto_otro >= 1 & pap_nieto_otro < 99) 
  # formato de variable número de padre
  pap_otros$pap_nieto_otro <- formatC(pap_otros$pap_nieto_otro, 
                                      width = 2, format = "d", flag = "0")
  # Crear un identificador de padres (id_papmam)
  pap_otros <- pap_otros %>% 
    mutate(id_papmam = paste(idhog, pap_nieto_otro, sep = "")) %>% 
    # remover duplicados (id_papmam están repetidos cuando hay más de un hijo en el  hogar)
    distinct(id_papmam, .keep_all = TRUE) 
  
# Base consolidada de padres y madres de nietos, otros parientes y no parientes
  papmam_otros <- bind_rows(padre = pap_otros, madre = mam_otros, .id = "padre_madre_otr")
  
# seleccionar variables de la base de padres y madres
  papmam_otros <- papmam_otros %>%
    # Creamos variable indicadora vivhij3 
    mutate(vivhij3 = 1) %>% 
    # asignamos identificador de padre o madre como idpers para fusionar con base total
    rename(idpers = id_papmam) %>%
    # selección de variables para fusionar tablas
    select(idpers, vivhij3, padre_madre_otr)
  ## Merge variables de padres/madres en base completa
  ury <- left_join(ury, papmam_otros)
  # reemplazar NA por 0 en variable vivhij3
  ury <- replace_na(x = ury, vivhij3, value = 0)

  # Calculo vivhij final (agrupa vivhij1, vivhij2, vivhij3)
  ury <- ury %>% 
    mutate(vivhij = if_else(vivhij1 == 1 | vivhij2 == 1 | vivhij3 ==1, 1, 0))

# Conversión de base de datos a factores / numeros
ury <- unlabelled(ury)

### _Resultados ----
  
code3 <- c("URY")
ury_lf <- data.frame(code3)
  
# __ LF1 ----
# % Personas de 25 a 34 años que viven con padre o madre
ury_lf <- ury %>%
    # seleccionar personas de 25 - 34 años
    filter(edad >= 25 & edad <= 34) %>%
    # personas que viven con sus padres
    summarise(lf1 = weighted.mean(vivpar, w = factorex) * 100) %>%
    bind_cols(ury_lf)
  
# __ LF2 ----
# % Desocupados de 25 a 34 años que viven con padre o madre
  ury_lf <- ury %>%
    # seleccionar personas de 25 - 34 años
    filter(edad >= 25 & edad <= 34) %>%
    # seleccionar personada desocupadas
    filter(condact == "desocupados") %>%
    # personas que viven con sus padres
    summarise(lf2 = weighted.mean(vivpar, w = factorex) * 100) %>%
    bind_cols(ury_lf)

### __ LF3 ----
### % Personas de personas de 75+ que viven con alguno de sus hijos
  ury_lf <- ury %>%
    # personas de 75 años y más
    filter(edad >= 75) %>%
    # personas que viven con sus hijos
    summarise(
      lf3 = weighted.mean(vivhij, w = factorex) * 100
    ) %>%
    bind_cols(ury_lf)
  
  
### __ LF4 ----
# % Personas de personas de 75+ que viven en hogares de más de dos personas
  ury_lf <- ury %>%
    # personas de 75 años y más
    filter(edad >= 75) %>%
    # indicador de hogares de más de dos personas
    mutate(npers2 = if_else(npers > 2, 1, 0)) %>%
    # personas que viven con sus hijos
    summarise(lf4 = weighted.mean(npers2, w = factorex) * 100) %>%
    bind_cols(ury_lf)
  
  ### Datos URY
  ury_lf
  