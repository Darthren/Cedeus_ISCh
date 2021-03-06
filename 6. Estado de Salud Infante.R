# Script que calcula el Estado de Salud Infante, el cual fue definido como "Obesidad Infantil"
# En la reuni�n del d�a 16/11/2016. Las ciudades a estudiar son: Copiap�, La Serena/Coquimbo,
# Gran Santiago, Temuco, Gran Concepci�n, y Valdivia.
# La fuente de los datos correspnde a la base de datos de la Serie P, la cual contiene informaci�n
# Sobre el Diagn�stico Nutricional Integrado en el Programa de Salud del Ni�o, provisto por la
# DEIS (Departamento de Estad�sticas e Informaci�n de Salud).
# Es necesario se�alar que los datos corresponden a ni�os entre 0 y 9 a�os que se atienden en el sistema
# de salud p�blica. Los ni�os que se atienden en sistema privado no est�n considerados debido a 
# la ausencia de informaci�n#. 
# Adicionalmente, Los ni�os entre 10 y 14 a�os no se encuentran cubiertos por ning�n programa
# Y no se conocen fuentes completas sobre su estado nutricional.

##### Procesamiento de datos #####

# Primero, se cargan las librer�as a utilizar.

require(rgdal); library(data.table); library(dplyr)

# Se fija la carpeta a trabajar

setwd("E:/Cedeus Sustainability Indicators/Datos/Obesidad Infantil/")

# Se lee la base de datos de la serie P, la cual incluye los datos del Diagn�stico Nutricional Integrado

seriep <- as.data.frame(fread("serieP.csv", sep= ",", header= T))

# Se definen los c�digos de los estados nutricionales a analizar. Estas corresponden a:
# P2070501: Riesgo/Bajo Peso
# P2070502: Desnutrido
# P2070503: Sobrepeso/Riesgo de Obesidad
# P2070504: Obeso
# P2070505: Normal
# P2070506: Desnutrici�n secundaria

Obesidadclass <- c("P2070501","P2070502","P2070503",
                   "P2070504","P2070505", "P2070506")

# Subset de los datos del Diagn�stico Nutricional Integrado. Se seleccionan las columnas del 1 al 13,
# Las columnas del 1 al 12 contienen info sobre mes, a�o, geograf�a, y c�digo del dato, entre otros.
# La columna 13 contiene la cantidad total de ni�os obsesos.

peso <- seriep[seriep$CodigoPrestacion %in% Obesidadclass ,1:13]

# Leer datos de las ciudades, y agrupar en funci�n de ciudades de estudio.

setwd("E:/Cedeus Sustainability Indicators/Datos/")

# Archivo con las ciudades a analizar
city_codes <- fread("Study_codes.csv", sep=",", header= T)
city_codes <- as.data.frame(city_codes)[,c(1,3)]

# Subset de pesosummary con las ciudades a analizar. Tambi�n se le a�ade el nombre de las ciudades

peso_ciudades <- as.data.frame(peso[peso$IdComuna %in% city_codes$Codigo,])
peso_ciudades$Ciudad <- city_codes[match(peso_ciudades[["IdComuna"]], city_codes[["Codigo"]] ), "Ciudad"]

# Se resume la informaci�n seg�n Ciudad para obtener: 
# Gordos: Total ni�os con sobrepeso + obesidad
# Total: TOtal de ni�os: suma de todos los ni�os
# %Gordos: Porcentaje de Ni�os con sobreso + obesidad

pesosummary <- peso_ciudades %>%
  group_by(Ciudad) %>%
  summarize(Gordos = sum(Col01[CodigoPrestacion == "P2070504" | CodigoPrestacion == "P2070503"], na.rm = T),
            T_ni�os = sum(Col01, na.rm = T)) %>%
  mutate("%Gordos" = round(Gordos/T_ni�os*100, 2))

# Exportar a .csv

setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(pesosummary, "6. Estado de Salud Infante.csv", row.names = F)






