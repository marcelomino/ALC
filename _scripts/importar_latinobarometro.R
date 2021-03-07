source("_scripts/Libraries_Options.R")

# Crear listado de archivos .dat en carpeta BBDD
lblist <- Sys.glob("/Users/Marcelo/Documents/BBDD/Latinobarometro/*.dta")
lblist <- as.list(lblist)
lblist

# Leer archivos y crear listado de dataframes
# "Prior to Stata 14, Stata on Mac and Linux use a different default encoding : "latin1"
d <- map(lblist, read_dta, encoding = "latin1")

# Editar nombres de objetos que componen el listado
lbnoms <- str_replace_all(
  lblist, ".dta", ""
)
lbnoms <- str_replace_all(
  lbnoms, "/Users/Marcelo/Documents/BBDD/Latinobarometro/", ""
)
lbnoms <- str_replace_all(
  lbnoms, "Latinobarometro_", "lb"
)
lbnoms
d <- set_names(d, lbnoms)





