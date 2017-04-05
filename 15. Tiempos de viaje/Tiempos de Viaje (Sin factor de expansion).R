# Script to calculate the total ratio of travel means to work vs total travels to work.
# It uses data from the Encuesta Origen-Destino published by SECTRA (Subsecretaria de Transporte)
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Code written by Rodrigo Villegas. E-Mail: rdgo.villegas@gmail.com

# Loading libraries and setting working directory

library(data.table); library(dplyr); library(chron); library(lubridate)
#setwd("E:/Cedeus Sustainability Indicators/Datos/EOD")
setwd("C:/Users/Rodrigo Villegas/Documents/Cedeus_ISCh/Cedeus_ISCh/15. Tiempos de viaje/")

# funciones de ayuda

# Corrector.nombre:
#   Input: nombres de columna, data.frame, variable a buscar (persona, tiempo, etc...)
#          variable a cambiar (IDPersona, TiempoViajes, etc)
#   Output: Dataset con nombre de variable cambiado.
# Nota: Acepta sólo una variable.
Corrector.nombre <- function(dataset, nombres, variable.buscar, variable.cambiar, exact.match = F){
  if (exact.match == T){
    colname <- grep(paste("^", variable.buscar, "$", sep = ""), nombres, ignore.case = T)
  } else {
    colname <- grep(variable.buscar, nombres, ignore.case = T)
  }
  if (colnames(dataset)[colname] != variable.cambiar){
    colnames(dataset)[colname] <- variable.cambiar
  }
  return(dataset)
}

ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
ciudades <- unique(ciudades$Ciudad)
# Attempt to clean data
setwd("Raw Data/")
files.viajes <- list.files(pattern = "viajes")
files.personas <- list.files(pattern = "personas")
files.modo <- list.files(pattern = "Modo")
total.ciudad <- data.frame()
for (c in seq(files.viajes)){
  tmp.viajes <- as.data.frame(fread(files.viajes[c], sep = ";", header = T))
  tmp.personas <- as.data.frame(fread(files.personas[c], sep = ";", header = T))
  tmp.modo <- as.data.frame(fread(files.modo[c], sep = ";", header = T))
  nombre.ciudad <- sub( "_.*$", "", files.viajes[c])
  # Almacenar los nombres de las columnas.
  column_names <- colnames(tmp.viajes)
  if (grepl("Santiago", files.viajes[c])){
    tmp.viajes <- Corrector.nombre(tmp.viajes, colnames(tmp.viajes), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.viajes), "hogar", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.viajes), "viaje", "IDViaje", exact.match = T) %>%
      Corrector.nombre(colnames(tmp.viajes), "laboralnormal", "Factor_Viaje") %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "TiempoViaje", "Factor_Viaje"))
   # El campo $TiempoViaje está en minutos. dividir en 60*24 convertirlo en tiempo
    tmp.viajes$Factor_Viaje <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.viajes$Factor_Viaje))
    tmp.viajes$TiempoViaje <- chron(times = tmp.viajes$TiempoViaje/(24*60))
    tmp.viajes <- tmp.viajes[!is.na(tmp.viajes$TiempoViaje) & !is.na(tmp.viajes$Factor_Viaje),]
    
    # Estandarizar Tabla de personas
    tmp.personas <- Corrector.nombre(tmp.personas, colnames(tmp.personas), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.personas), "hogar", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.personas), "laboralnormal", "Factor_Persona") %>%
      subset(select = c("IDFolio", "IDPersona", "Factor_Persona"))
    # Unificar ambas tablas
    tmp.personas$Factor_Persona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$Factor_Persona))
    tmp.personas$IDFolio <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$IDFolio))
    tmp.personas$IDPersona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$IDPersona))
    
    
    tmp <- merge(tmp.personas, tmp.viajes, by = c("IDFolio", "IDPersona"))
    
    tmp.summary.total <- tmp %>%
      group_by(IDFolio, IDPersona, IDViaje) %>%
      summarise(TiempoViaje.tmp = sum((as.numeric(TiempoViaje)*(24*60*60)*240))
                , Factor_Persona = mean(Factor_Persona)) %>%
      ungroup() %>%
      summarise(Ciudad = nombre.ciudad,
                Promedio_Anual_Tiempo_Viaje = round(seconds_to_period(sum(TiempoViaje.tmp)/length(unique(IDPersona)))))
    total.ciudad <- rbind(total.ciudad, tmp.summary.total)
    
  }  else {
    tmp.viajes <- Corrector.nombre(tmp.viajes, colnames(tmp.viajes), "tiempo", "TiempoViaje") %>%
      Corrector.nombre(colnames(tmp.viajes), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.viajes), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.viajes), "idviaje", "IDViaje") %>%
      Corrector.nombre(colnames(tmp.viajes), "Factor", "Factor_Viaje", exact.match = T) %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "TiempoViaje", "Factor_Viaje"))
    
    # Si los tiempos de viaje están en formato "YMD HMS", corregir
    if (nchar(tmp.viajes$TiempoViaje[1]) == 18){
      tmp.viajes$TiempoViaje <- strptime(tmp.viajes$TiempoViaje, format = "%d-%m-%Y %H:%M:%S") 
      tmp.viajes$TiempoViaje <- format(tmp.viajes$TiempoViaje,"%H:%M:%S")
    }
    # Transformar las unidades de tiempo de char a tiempo
    tmp.viajes$TiempoViaje <- chron(times= tmp.viajes$TiempoViaje)
    
    
    if (class(tmp.viajes$Factor_Viaje) != "numeric"){
      tmp.viajes$Factor_Viaje <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.viajes$Factor_Viaje))
    }
    
    # Importar tabla con población
    tmp.personas <- Corrector.nombre(tmp.personas, colnames(tmp.personas), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.personas), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.personas), "factor", "Factor_Persona") %>%
      subset(select = c("IDFolio", "IDPersona", "Factor_Persona"))
    
    if (class(tmp.personas$Factor_Persona) != "numeric"){
      tmp.personas$Factor_Persona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$Factor_Persona))
    }
    
    tmp <- merge(tmp.personas, tmp.viajes, by = c("IDFolio", "IDPersona"))
    
    tmp.summary.total <- tmp %>%
      group_by(IDFolio, IDPersona, IDViaje) %>%
      summarise(TiempoViaje.tmp = sum((as.numeric(TiempoViaje)*(24*60*60)*240))
                , Factor_Persona = mean(Factor_Persona)) %>%
      ungroup() %>%
      summarise(Ciudad = nombre.ciudad,
                Promedio_Anual_Tiempo_Viaje = round(seconds_to_period(sum(TiempoViaje.tmp)/length(unique(paste(IDFolio, IDPersona, sep = ""))))))
    total.ciudad <- rbind(total.ciudad, tmp.summary.total)
    
  }
  # Estandarizar nombres
  
}  


#TODO Incorporar modo agregado

tmp.summary.modoagregado <- tmp %>%
  group_by(IDFolio, IDPersona, IDViaje) %>%
  summarise(TiempoViaje.tmp = sum((as.numeric(TiempoViaje)*(24*60*60)*365*Factor_Viaje))
            , Factor_Persona = mean(Factor_Persona)) %>%
  ungroup() %>%
  summarise(Promedio_Anual_Tiempo_Viaje = round(seconds_to_period(sum(TiempoViaje.tmp)/(sum(Factor_Persona)))))
