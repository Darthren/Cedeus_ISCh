# Leer librerías
library(xlsx); library(dplyr)
setwd("E:/Cedeus Sustainability Indicators/GIS/Consumo Agua Potable/")
# Enumerar los archivos excel
xlsx.files <- paste("Raw Data", list.files("Raw Data", pattern = ".xlsx"), sep = "/")
# Dataframe vacio para almacenar datos de excel
agua.consumo.raw <- data.frame()
# Column Class para aumentar velocidad de lectura
colclass <- c("numeric", "character", rep("numeric", 2), rep("character", 3), rep("numeric", 2))
# Loop que lee cada excel con los datos de consumo y los almacena en agua.consumo.raw
# Si los nombres de las columnas no calzan, asigna el nombre del último archivo leído
for (i in seq(xlsx.files)){
  tmp.file <- read.xlsx2(xlsx.files[i], sheetIndex = 1, 
                        encoding = "UTF-8", colClasses = colclass)
  if (!(colnames(agua.consumo.raw) == colnames(tmp.file)) & nrow(agua.consumo.raw) > 1){
    colnames(agua.consumo.raw) <- colnames(tmp.file)
  }
  agua.consumo.raw <- rbind(agua.consumo.raw, tmp.file)
}
# Algunos datos tienen valores negativos. Eso no debería ser, por lo que se multiplican por -1
agua.consumo.raw$M3.Ap[agua.consumo.raw$M3.Ap < 0] <- -(agua.consumo.raw$M3.Ap[agua.consumo.raw$M3.Ap < 0])

# Se agregan los datos en función de la comuna
agua.consumo.comuna <- agua.consumo.raw %>%
  group_by(Comuna, Tipo.Cliente, Año) %>%
  summarise("Consumo" = sum(M3.Ap))

# Talcahuano no tiene datos para 2016, así que se usarán datos de 2015.
Talcahuano <- agua.consumo.comuna[agua.consumo.comuna$Comuna == "TALCAHUANO",]
Talcahuano$Año <- 2016
agua.consumo.comuna <- rbind(agua.consumo.comuna, Talcahuano)
# Seleccionar los datos Residenciales que correspondan al año 2016
agua.consumo.comuna <- agua.consumo.comuna[agua.consumo.comuna$Tipo.Cliente == "Residencial" &
                                               agua.consumo.comuna$Año == 2016,]
# Filtrar por comunas en las ciudades Cedeus
agua.consumo.comuna <- agua.consumo.comuna[agua.consumo.comuna$Comuna %in% toupper(ciudades$Comuna_Uppercase),]
# Leer csv con las ciudades cedeus y asignar ciudad en función de comuna
ciudades <- as.data.frame(read.csv("Otros/Ciudades_Cedeus.csv", sep=";"))
agua.consumo.comuna$Ciudad <- ciudades[match(agua.consumo.comuna[["Comuna"]], ciudades[["Comuna_Uppercase"]] ), "Ciudad"]
# Leer datos de población y agregarlos según ciudad.
pob_2016 <- read.xlsx("Otros/poblacion_2016.xlsx", sheetIndex = 1, encoding = "UTF-8" )
pob_2016 <- pob_2016[pob_2016$`Nombre.comuna` %in% ciudades$Comuna & pob_2016$Label == "Total",]
pob_2016$Ciudad <- ciudades[match(pob_2016[["Nombre.comuna"]], ciudades[["Comuna"]] ), "Ciudad"]
pob_2016 <- pob_2016 %>%
  group_by(Ciudad) %>%
  summarise("Población" = sum(Población, na.rm = T))
# comunar datos de población y consumo para obtener consumo per cápita.
agua.consumo.ciudad <- merge(agua.consumo.comuna, pob_2016, by = "Ciudad") %>%
  group_by(Ciudad) %>% 
  summarise("Consumo per cápita" = sum(Consumo)/mean(Población))
# Escribir los resultados
setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(agua.consumo.ciudad, "3. Consumo de agua potable.csv", row.names = F)



