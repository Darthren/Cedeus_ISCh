# Script que calcula la calidad de salud para las ciudades de Concepción, Coquimbo, 
# Copiapó, Temuco , Santiago and Valdivia.
# La Calidad de Salud se entiende como la razón entre el número de muertes prevenibles dividido
# Por el número total de muertes.
# Las muertes prevenibles vienen dadas por el estudio "La mortalidad evitable y no 
# evitable: distribución geográfica en áreas pequeñas de España (1990-2001)" - Vergara, M., 2007.
# Los datos usados consisten en:
#   Base de datos de defunciones para el año 2014.
#   Rangos_muertesprevenibles. Una categorización de los códigos CEI-10 y rangos de edad para 
#                             las muertes prevenibles
#   study_codes. Un archivo .csv que contiene los códigos de las comunas a estudiar.

# Cargar las librerías a usar, y el directorio de trabajo

library(data.table); library(dplyr); library(readxl)
setwd("E:/Cedeus Sustainability Indicators/Datos/")

# Cargar la base de datos de defunciones, los códigos de enfermedades prevenibles, 
# y los códigos de las ciudades

defunctions <- fread("DEF2014.csv", sep= ";", header= T, select = c(5,7,8,17,18,19,20))
prev_codes <- read_excel(path = "Rangos_muertesprevenibles.xlsx" )
city_codes <- as.data.frame(fread("Study_codes.csv", sep=",", header= T))

# Subset de las defunciones cuya Comuna esté en las ciudades a estudiar

defunctions <- defunctions[defunctions$COMUNA %in% city_codes$Codigo,]

# Añadir el nombre de la ciudad.

defunctions$Ciudad <- city_codes[match(defunctions[["COMUNA"]], city_codes[["Codigo"]] ), "Ciudad"]

# Fijar como EDAD_CANT = 0 a todos los niños cuya edad sea menor a 1 (EDAD_TIPO > 1)

defunctions[!(test$EDAD_TIPO == 1 | defunctions$EDAD_TIPO == 9)]$EDAD_CANT <- 0

# Descartar casos donde EDAD_TIP == 9, ya que 9 es null.

defunctions <- defunctions[!(defunctions$EDAD_TIPO == 9)]

# Summarizar el total de muertes, según ciudad, y lugar de defunción.

total_def <- defunctions %>%
  group_by(Ciudad) %>%
  summarise("Defunciones totales en Hospital o Clinica" = sum(LOCAL_DEF == 1), 
            "Defunciones totales en Casa Habitacion" = sum(LOCAL_DEF == 2), 
            "Defunciones totales en Otro" = sum(LOCAL_DEF == 3),
            "Defunciones en Total" = (n()))

# Una pequeña función para hacer un subset por cada tipo de defunción
# inputdata hace referencia al dataframe "defunctions"
# inputcodes hace referencia a los códigos de defunciones.

prev_filter <- function(inputdata, inputcodes){
  # Crea un dataframe vacio para almacenar resultados
  finaldata <- data.frame()
  # Por cada fila en el dataframe con los códigos de muertes prevenibles, subset aquellos que
  # son iguales o mayores que la edad mínima; igual o menores a la edad máxima, y que cuya 
  # diagnosis esté entre los brackes de códigos CEI-10 definidos por "Char_Min" y "Char_Max".

  for (i in 1:nrow(inputcodes)){
    # subset data basandose en el código de muerte preenible número "i"
    tmpdata <- inputdata[inputdata$EDAD_CANT >= inputcodes$Edad_min[i] &
                           inputdata$EDAD_CANT <= inputcodes$Edad_max[i] &
                           inputdata$DIAG1 >= inputcodes$Char_Min[i] &
                           inputdata$DIAG1 <= inputcodes$Char_Max[i],]
    # unir tmpdata con finaldata
    finaldata <- rbind(finaldata, tmpdata)
  }
  return(finaldata)
}

# Aplicar la función a los datos, y summarizar por ciudad y lugar de muerte

prev_defunctions <- prev_filter(defunctions, prev_codes) %>%
  group_by(Ciudad) %>%
  summarise("Defunciones prevenibles Hospital o Clinica" = sum(LOCAL_DEF == 1), 
            "Defunciones prevenibles Casa Habitacion" = sum(LOCAL_DEF == 2), 
            "Defunciones prevenibles Otro" = sum(LOCAL_DEF == 3),
            "Defunciones prevenibles Total" = (n()))

# Combinar ambos dataframes (prev_defunctions y total_def), y calcular los ratios de muertes
# prevenibles para cada lugar, y en total.

health_quality <- merge(prev_defunctions, total_def, by = "Ciudad") %>%
  mutate("Razon defunciones prevenibles Hospital o Clinica" = round(`Defunciones prevenibles Hospital o Clinica`/`Defunciones totales en Hospital o Clinica`, 4),
         "Razon defunciones prevenibles Casa Habitacion" = round(`Defunciones prevenibles Casa Habitacion`/`Defunciones totales en Casa Habitacion`, 4),
         "Razon defunciones prevenibles Otro" = round(`Defunciones prevenibles Otro`/`Defunciones totales en Otro`, 4),
         "Razon defunciones prevenibles Total" = round(`Defunciones prevenibles Total`/`Defunciones en Total`, 4))

# Re-ordenar los datos, para que tengamos "Muertes prevenibles", "Muertes totales", 
# "razón muertes prevenibles" para cada lugar y ciudad

health_quality <- as.data.frame(health_quality)
heatlh_quality <- health_quality[,c(1,2,6,10,3,7,11,4,8,12,5,9,13)]

# Exportar a .csv

setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(heatlh_quality, "4. Calidad de Salud.csv", row.names = F)
