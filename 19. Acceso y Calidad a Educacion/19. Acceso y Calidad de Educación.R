# Indicador 
library(rgdal); library(readxl); library(dplyr)

setwd("E:/Cedeus Sustainability Indicators/Datos/19. Acceso y Calidad a Educación/")

# Csv con ciudades
ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")

# Excel con los datos sobre calidad de educación (Indicador INDICER), sleccionar
# Sólo las columnas con las ID de los Colegios, el nombre, el codigo de la comuna
# Y el puntaje INDICER
calidad <- read_excel("Raw Data/20150409_SNED_2014_2015.xlsx") %>%
  subset(select = c("RBD", "DV_RBD", "NOM_RBD", "COD_COM_RBD","RURAL_RBD","INDICER" ))

# Añadir nombre de ciudades en función de comuna
calidad$Ciudad <- ciudades[match(calidad[["COD_COM_RBD"]], ciudades[["Codigo"]] ), "Ciudad"]
# Seleccionar las ciudades, agrupar los datos en función de éstas,
# Y seleccionar sólo el top 35%.
calidad <- calidad[(!is.na(calidad$Ciudad)),] %>%
  group_by(Ciudad) %>%
  mutate(breakpoint = quantile(INDICER, .35)[[1]]) %>%
  subset(INDICER > breakpoint)

edu <- read.csv("Raw Data/20151007_Directorio_Oficial_EE_2015_20150831_PUBL.csv", sep = ";") %>%
  subset(select = c("RBD", "DGV_RBD", "NOM_RBD","COD_COM_RBD","RURAL_RBD","DIR_RBD", 
                    "LATITUD", "LONGITUD","PAGO_MENSUAL"))

#edu <- edu[edu$PAGO_MENSUAL %in% c("GRATUITO", "$1.000 A $10.000", "$10.001 A $25.000") &
#                     edu$COD_COM_RBD %in% ciudades$Codigo, ]

edu <- edu[edu$COD_COM_RBD %in% ciudades$Codigo, ]


edu.calidad <- merge(edu, calidad, by.x = c("RBD", "DGV_RBD"), by.y = c("RBD", "DV_RBD"), all = T) %>%
  subset(select = c("RBD", "DGV_RBD", "NOM_RBD.x", "NOM_RBD.y", 
                    "COD_COM_RBD.x", "COD_COM_RBD.y", "DIR_RBD", "LONGITUD", "LATITUD", "INDICER",
                    "PAGO_MENSUAL", "Ciudad")) %>%
  subset(!is.na(INDICER) & PAGO_MENSUAL %in% c("GRATUITO", "$1.000 A $10.000", "$10.001 A $25.000") )


lon <- as.character(edu.calidad[,"LONGITUD"])
lon <- as.numeric(gsub(",", ".", lon))
lat <- as.character(edu.calidad[,"LATITUD"])
lat <- as.numeric(gsub(",", ".", lat))
coords <- data.frame(lon = lon, lat = lat)

edu.spatial <- SpatialPointsDataFrame(coords = coords, data = edu.calidad, 
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


writeOGR(edu.spatial, dsn = "GIS", layer = "Colegios_BB", driver = "ESRI Shapefile", overwrite_layer = T)
