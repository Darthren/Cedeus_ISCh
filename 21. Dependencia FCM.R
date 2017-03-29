# Script para calcular el indicador de _______
# Que consiste en el porcentaje de Dependencia al Fondo Común Municipal (ifcm)

library(xlsx); library(dplyr)

setwd("E:/Cedeus Sustainability Indicators/Datos/21. Deuda Municipal/")

# Leer archivos en bruto: Ciudades, Población, e Ingresos por Fondo Comun Municipal (ifcm).
# A su vez, combinarlos.
ciudades <- read.csv("Ciudades_Cedeus.csv")

pob <- read.xlsx("poblacion_2015.xlsx", sheetIndex = 1, encoding = "UTF-8")

pob <- pob[pob$Nombre.comuna %in% ciudades$Comuna & pob$Label == "Total",
           c("Nombre.comuna", "Poblacion")]
pob$Ciudad <- ciudades[match(pob[["Nombre.comuna"]], ciudades[["Comuna"]] ), "Ciudad"]
pob$Codigo <- ciudades[match(pob[["Nombre.comuna"]], ciudades[["Comuna"]] ), "Codigo"]

ifcm <- read.xlsx(file = "Ingresos y Aportes a Fondo Comun Municipal.xlsx",
                      sheetIndex = 1, startRow = 3, encoding = "UTF-8",
                      colClasses = c("numeric", "character", "numeric", "numeric"))
ifcm <- ifcm[ifcm$CODIGO %in% ciudades$Codigo,]
pob$Ciudad <- ciudades[match(pob[["Nombre.comuna"]], ciudades[["Comuna"]] ), "Ciudad"]

ifcm <- merge(ifcm, pob, by.x = "CODIGO", by.y = "Codigo")
# Hacer el resumen, obtener la cantidad per cápita, y multiplicarla por 1000 (ya que la unidad
# está en miles de pesos)

ifcm.summary <- ifcm %>%
  group_by(Ciudad) %>%
  summarise("Ingresos FCM per capita" = sum(X2015-X2015.1)*1000/sum(Poblacion))

# Exportar resultado
setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(residuos, "21. Dependencia FCM.csv", row.names = F)


