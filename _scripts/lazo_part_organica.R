#************************************************************************* #
# Indicadores Lien de Participation Organique (LPO)
#
# Date Revisión : 8 / 11 / 2020
#************************************************************************* #
# Cargar packages y opciones
source("_scripts/Libraries_Options.R")

# Cargar UN Codes
uncodes <- read_csv("~/Documents/BBDD/UN - Standard country or area codes.csv")
describe(uncodes)
names(uncodes)
uncodes <- uncodes %>% 
  select(`Sub-region Name`, 
         `Intermediate Region Name`,
         `M49 Code`,
         `ISO-alpha2 Code`,
         `ISO-alpha3 Code`,
         `Country or Area`) %>% 
  rename(subregion = `Sub-region Name`,
         interregion = `Intermediate Region Name`,
         m49code = `M49 Code`,
         code2 = `ISO-alpha2 Code`,
         code3 = `ISO-alpha3 Code`,
         country = `Country or Area`)

glimpse(uncodes)

#************************************************************************* #
# LPO_1 : Empleos cubiertos por una convención Colectiva - ECCV (Porcentaje) ----
#************************************************************************* #

## Cargar indicador de la base de datos ILOSTAT
# Tabla de contenidos
ilo_toc_bargaining <- get_ilostat_toc(search = "bargaining")
describe(ilo_toc_bargaining)
# cargar data
eccv_oit <- get_ilostat(id = ilo_toc_bargaining$id, segment = 'indicator', type = 'both')
describe(eccv_oit)
# Rename identificador país, 3 dígitos (code3)
eccv_oit <- rename(eccv_oit, code3 = ref_area) 
# Merge UN Codes
eccv_oit <- left_join(eccv_oit, uncodes)

### Datos de América latina y disponibilidad  
eccv_alc <- eccv_oit %>% 
  filter(subregion == "Latin America and the Caribbean")


# Resumen disponibilidad de datos en serie de tiempo
eccv_alc_disp <- eccv_alc %>%
  select(code3, time) %>% 
  # 2005 - 2009
  mutate(disp_05_09 = if_else(time >= 2005 & time <= 2009, 1, 0)) %>%
  # 2010 - 2015
  mutate(disp_10_15 = if_else(time >= 2010 & time <= 2015, 1, 0)) %>%
  # 2016 - 2009
  mutate(disp_16_20 = if_else(time >= 2016 & time <= 2020, 1, 0))
eccv_alc_disp <- eccv_alc_disp %>%  
  # agrupar según países
  group_by(code3) %>%
  # indicador de siponibilidad según periodos
  mutate(disp_05_09 = max(disp_05_09),
         disp_10_15 = max(disp_10_15),
         disp_16_20 = max(disp_16_20)
         ) %>% 
  # eleiminar columna tiempo
  select(-time) %>% 
 # seleccionar primer registro del grupo
 slice(1)
eccv_alc_disp <- ungroup(eccv_alc_disp)

# merge disponibilidad en base ALC
eccv_alc <- left_join(eccv_alc, eccv_alc_disp)

### Seleccionar periodo 2005 - 2015
eccv_alc <- eccv_alc %>% filter(time >= 2005 & time <= 2015) 
# Seleccionar último registro del período
eccv_alc <- eccv_alc %>% 
  group_by(code2) %>% 
  mutate(ultimo = max(time)) %>% 
  filter(time == ultimo) 
eccv_alc <- ungroup(eccv_alc)

### Datos Convención colectiva ALC - FINAL
eccv_alc <- eccv_alc %>%  
  select(
  code3, code2, country,
  time, obs_value,
  source.label,
  note_source.label,
  note_indicator.label
  ) %>% 
  rename(eccv_oit = obs_value)

### GUARDAR Convención Colectiva ALC ----
write_csv(eccv_alc, "_resultados/lpo_eccv_alc.csv")


#************************************************************************* #
# LPO_2 : Empleo informal ----
#************************************************************************* #

## Cargar indicador de la OIT
empleo_informal_OIT_2018 <- read_excel(
  "_data/informalidad empleo/empleo_informal_OIT_2018.xlsx", 
  sheet = "data")

### Base informalidad ALC
informal_alc <- empleo_informal_OIT_2018 %>% 
  filter(region == "Latin America and the Caribbean")
  

### GUARDAR Convención Colectiva ALC ----
write_csv(informal_alc, "_resultados/lpo_informal_alc.csv")


#************************************************************************* #
# LPO : Base final ALC ----
#************************************************************************* #

### _Consolidar base indicadores ----

eccv_alc <- read_csv("_resultados/lpo_eccv_alc.csv")
informal_alc <- read_csv("_resultados/lpo_informal_alc.csv")

names(eccv_alc)
names(informal_alc)

### base de datos lpo
# Seleccionar variables para merge
eccv_alc <- eccv_alc %>% select(
  code3, eccv_oit
)

informal_alc <- informal_alc %>% 
  select(code, obs) %>% 
  rename(code3 = code,
         informal = obs)

### Merge bases 
lpo_data_alc <- full_join(informal_alc, eccv_alc)

# Seleccionar datos para ALC-18
lpo_data_alc <- lpo_data_alc %>% 
  filter(code3 != "BLZ" &
         code3 != "CUB" & 
         code3 != "VCT") %>% 
# Ordenar variables 
  select(code3, eccv_oit, informal)


### _Calcular Calcular indicadores de resumen ----

# Porcentaje de empleo formal
lpo_data_alc <- lpo_data_alc %>% 
  mutate(formal = 100 - informal)  %>% 
# Indicateur final LPO = (ECCV) x (EF) / 100
  mutate(lpo = (eccv_oit * formal) / 100)


### GUARDAR base LPO - ALC ----
write_csv(lpo_data_alc, "_resultados/l3_porganica_alc.csv")


