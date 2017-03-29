# Script que calcula el Estado de Salud Infante, el cual fue definido como "Obesidad Infantil"
# En la reunión del día 16/11/2016. Las ciudades a estudiar son: Copiapó, La Serena/Coquimbo,
# Gran Santiago, Temuco, Gran Concepción, y Valdivia.
# La fuente de los datos correspnde a la base de datos de la Serie P, la cual contiene información
# Sobre el Diagnóstico Nutricional Integrado en el Programa de Salud del Niño, provisto por la
# DEIS (Departamento de Estadísticas e Información de Salud).
# Es necesario señalar que los datos corresponden a niños entre 0 y 9 años que se atienden en el sistema
# de salud pública. Los niños que se atienden en sistema privado no están considerados debido a 
# la ausencia de información#. 
# Adicionalmente, Los niños entre 10 y 14 años no se encuentran cubiertos por ningún programa
# Y no se conocen fuentes completas sobre su estado nutricional.

##### Procesamiento de datos #####

# Primero, se cargan las librerías a utilizar.

require(rgdal); library(data.table); library(dplyr)

# Se fija la carpeta a trabajar

setwd("E:/Cedeus Sustainability Indicators/Datos/Obesidad Infantil/")

# Se lee la base de datos de la serie P, la cual incluye los datos del Diagnóstico Nutricional Integrado

seriep <- as.data.frame(fread("serieP.csv", sep= ",", header= T))

# Se definen los códigos de los estados nutricionales a analizar. Estas corresponden a:
# P2070501: Riesgo/Bajo Peso
# P2070502: Desnutrido
# P2070503: Sobrepeso/Riesgo de Obesidad
# P2070504: Obeso
# P2070505: Normal
# P2070506: Desnutrición secundaria

Obesidadclass <- c("P2070501","P2070502","P2070503",
                   "P2070504","P2070505", "P2070506")

# Subset de los datos del Diagnóstico Nutricional Integrado. Se seleccionan las columnas del 1 al 13,
# Las columnas del 1 al 12 contienen info sobre mes, año, geografía, y código del dato, entre otros.
# La columna 13 contiene la cantidad total de niños obsesos.

peso <- seriep[seriep$CodigoPrestacion %in% Obesidadclass ,1:13]

# Leer datos de las ciudades, y agrupar en función de ciudades de estudio.

setwd("E:/Cedeus Sustainability Indicators/Datos/")

# Archivo con las ciudades a analizar
city_codes <- fread("Study_codes.csv", sep=",", header= T)
city_codes <- as.data.frame(city_codes)[,c(1,3)]

# Subset de pesosummary con las ciudades a analizar. También se le añade el nombre de las ciudades

peso_ciudades <- as.data.frame(peso[peso$IdComuna %in% city_codes$Codigo,])
peso_ciudades$Ciudad <- city_codes[match(peso_ciudades[["IdComuna"]], city_codes[["Codigo"]] ), "Ciudad"]

# Se resume la información según Ciudad para obtener: 
# Gordos: Total niños con sobrepeso + obesidad
# Total: TOtal de niños: suma de todos los niños
# %Gordos: Porcentaje de Niños con sobreso + obesidad

pesosummary <- peso_ciudades %>%
  group_by(Ciudad) %>%
  summarize(Gordos = sum(Col01[CodigoPrestacion == "P2070504" | CodigoPrestacion == "P2070503"], na.rm = T),
            T_niños = sum(Col01, na.rm = T)) %>%
  mutate("%Gordos" = round(Gordos/T_niños*100, 2))

# Exportar a .csv

setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(pesosummary, "6. Estado de Salud Infante.csv", row.names = F)






