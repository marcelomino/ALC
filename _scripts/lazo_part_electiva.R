#************************************************************************* #
# Indicadores participación Electiva (LPE)
#
# Date Revisión : 5 / 12 / 2020
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


# Importar World Giving Index data
# datos reportados a partir de World Gallup Poll 2015
# Indicators wgi
# helped = Helped a stranger, or someone you didn’t know who needed help? 
# donated = Donated money to a charity?
# volunteer = Volunteered your time to an organisation? 

worldgiv <- read_excel(
  "_data/world giving index/World Giving Index 2016 Dataset.xlsx", 
  sheet = "data_alc"
  )
worldgiv


# Importar WVS
# Miembros activos de una asociación humanitaria
wvs_humanit <- read_excel("_data/wvs/alc_membership_charitablehumanitarian_organization.xlsx", sheet = "lpe1")
wvs_humanit

wvs_humanit <- wvs_humanit %>% 
  select(-wave,-pais) %>% 
  rename(
    humanit= obs
  )


# Seleccionar y renombrar variables 
l2_pelectiva <- worldgiv %>% 
  select(code3, volunteer, donated) 

l2_pelectiva <- full_join(l2_pelectiva, wvs_humanit)

l2_pelectiva <- l2_pelectiva %>% 
  mutate(
    lpe1 = humanit,
    lpe2 = donated,
    lpe1_alt = volunteer
  )

l2_pelectiva

### GUARDAR BASE Lazo de Participación Electiva ALC ----
write_csv(l2_pelectiva, "_resultados/l2_pelectiva.csv")




