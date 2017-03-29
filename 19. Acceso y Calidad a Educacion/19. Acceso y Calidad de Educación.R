# Script para calcular la accesibilidad a la educación en función de la
# calidad de los colegios, la cual estará determinada por la evaluación
# SNED de los colegios, la cual considera, entre otros: Inclusión, puntaje SIMCE
# Calidad laboral, calidad docente, etc.
# El Script se compone de dos partes importantes.
# La primera parte constituye el trabajo con las bases de datos obtenidas desde
# el centro de estudios del mineduc, donde se utilizaron las bases de datos
# Directorio Oficial Establecimientos Educacionales (2015)
# Evaluación SNED (Periodo 2014 - 2015).
# Se utilizó el DOEE de 2015 dado que la evaluación SNED corresponde al periodo 2014-2015

# Leer las librerías necesarias
library(rgdal); library(readxl); library(dplyr)

# Directorio donde se encuentra la carpeta con los archivos asociados al indicador.
# Cambiar según el usuario.
#setwd("E:/Cedeus Sustainability Indicators/Datos/19. Acceso y Calidad a Educación/")
setwd("C:/Users/Rodrigo Villegas/Documents/Cedeus_ISCh/Cedeus_ISCh/19. Acceso y Calidad a Educacion/")

# Csv con los códigos de las comunas y las ciudades a las que estas pertenecen.
ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")

# Excel con los datos sobre calidad de educación (Indicador INDICER), sleccionar
# Sólo las columnas con las ID de los Colegios (RBD y DV_RBD), el nombre (NOMB_RBD), 
# el código de la comuna (COD_COM_RBD), y el puntaje INDICER (INDICER).
# No se utilizó la variable "RURAL_RBD", la cual contiene la identificación sobre
# si el colegio es rural o urbano, puesto que para la accesibilidad sólo se utilizan
# manzanas en la zona urbana, y existen casos de colegios con clasificación confusa
calidad <- read_excel("Raw Data/20150409_SNED_2014_2015.xlsx") %>%
  subset(select = c("RBD", "DV_RBD", "NOM_RBD", "COD_COM_RBD","INDICER" ))

# Añadir nombre de ciudades en función del código de cada comuna
calidad$Ciudad <- ciudades[match(calidad[["COD_COM_RBD"]], ciudades[["Codigo"]] ), "Ciudad"]
# Seleccionar las ciudades, agrupar los datos en función de éstas,
# Y seleccionar sólo el top 35%.
calidad <- calidad[(!is.na(calidad$Ciudad)),] %>%
  group_by(Ciudad) %>%
  mutate(breakpoint = quantile(INDICER, .35)[[1]]) %>%
  subset(INDICER > breakpoint)

# Leer el Directorio Oficial de Establecimientos Educacionales para el año 2015
# Y seleccionar las columnas relevantes para el análisis
edu <- read.csv("Raw Data/20151007_Directorio_Oficial_EE_2015_20150831_PUBL.csv", sep = ";") %>%
  subset(select = c("RBD", "DGV_RBD", "NOM_RBD","COD_COM_RBD","DIR_RBD", 
                    "LATITUD", "LONGITUD","PAGO_MENSUAL"))
# seleccionar los colegios dentro de las ciudades cedeus
edu <- edu[edu$COD_COM_RBD %in% ciudades$Codigo, ]

# Combinar ambos data.frames según las ID de cada establecimiento.
# Y luego hacer un subset, seleccionando aquellos colegios que tengan puntaje
# INDICER, y sea gratuitos, o con un costo menor a $25.000.
edu.calidad <- merge(edu, calidad, by.x = c("RBD", "DGV_RBD"), by.y = c("RBD", "DV_RBD"), all = T) %>%
  subset(select = c("RBD", "DGV_RBD", "NOM_RBD.x", "COD_COM_RBD.x", "DIR_RBD", 
                    "LONGITUD", "LATITUD", "INDICER",
                    "PAGO_MENSUAL", "Ciudad")) %>%
  subset(!is.na(INDICER) & PAGO_MENSUAL %in% c("GRATUITO", "$1.000 A $10.000", "$10.001 A $25.000") )

# Las columnas de longitud y latitud tienen comas en vez de puntos.
# Cambiar a puntos.

edu.calidad$LONGITUD <- as.character(edu.calidad[,"LONGITUD"])
edu.calidad$LONGITUD <- as.numeric(gsub(",", ".", lon))
edu.calidad$LATITUD <- as.character(edu.calidad[,"LATITUD"])
edu.calidad$LATITUD <- as.numeric(gsub(",", ".", lat))
coords <- data.frame(lon = edu.calidad$LONGITUD , lat = edu.calidad$LATITUD)
# Crear un objeto espacial a partir de las coordenadas, y exportarlo a shp para posterior visualización
edu.spatial <- SpatialPointsDataFrame(coords = coords, data = edu.calidad, 
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
writeOGR(edu.spatial, dsn = "GIS", layer = "Colegios_BB", driver = "ESRI Shapefile", overwrite_layer = T)

