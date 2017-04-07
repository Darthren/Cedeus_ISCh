# Script para calcular la partición modal de cada ciudad.
# Input: Tablas de Viajes, encontradas dentro de la EOD de cada ciudad.
#        Tablas con las reclasificaciones de los modos de transporte de cada viaje.
#         para agruparlos en 5 categorías.
# Output: Tabla con la partición modal de cada ciudad.

library(data.table); library(dplyr)
# Carpeta donde se encuentra el script. Modificar según necesidades del usuario.
setwd("C:/Users/Rodrigo Villegas/Documents/Cedeus_ISCh/Cedeus_ISCh/14. Partición Modal/")

# Funciones de ayuda:

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

# Estandarizador:
#   Input: Tabla EOD en bruto
#   Output: Tabla EOD estandarizada
#   Nota: cuando una variable tiene el sufijo "_", se ejecuta "exact.macth" de Corrector.nombre
Estandarizador <- function(Tabla_EOD, variables.buscar, variables.cambiar){
  for (i in seq(variables.buscar)){
    if (grepl("_", variables.buscar[i])){
      variables.buscar[i] <- gsub("_", "", variables.buscar[i])
      Tabla_EOD <- Corrector.nombre(Tabla_EOD, colnames(Tabla_EOD), variables.buscar[i], variables.cambiar[i], exact.match = T)
    } else { 
      Tabla_EOD <- Corrector.nombre(Tabla_EOD, colnames(Tabla_EOD), variables.buscar[i], variables.cambiar[i])
    }
  }
  return(Tabla_EOD)
}

ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
ciudades <- unique(ciudades$Ciudad)
ciudades <- ciudades[-3]
files.viajes <- list.files("Raw_Data", pattern = "viajes")
files.modo <- list.files("Raw_Data", pattern = "ModoReclass")

particion.modal <- data.table()

for (c in seq(files.viajes)){
  ciudad.nombre <- ciudades[c]
  
  if (ciudad.nombre %in% c("Gran Coquimbo" , "Gran Santiago") | c == 3){
    next()
  }
  
  viajes <- read.csv(paste("Raw_Data/", files.viajes[c], sep = ""), sep = ";") %>%
    Estandarizador(c("folio", "persona", "idViaje", "Factor_"),
                   c("IDFolio", "IDPersona", "IDViaje", "Factor_Viaje")) %>%
    subset(select = c("IDFolio",  "IDPersona", "IDViaje", "Factor_Viaje", "IDModo"))
  
  viajes.modo <- read.csv(paste("Raw_Data/", files.modo[c], sep = ""))
  viajes <- merge(viajes, viajes.modo, by = "IDModo")
  
  viajes.summary <- viajes %>%
    summarise(Ciudad = ciudad.nombre,
              'Transporte Privado/Auto' = sum(Factor_Viaje[ModoAgregado == "Transporte Privado/Auto"])/sum(Factor_Viaje),
              'Transporte Publico' = sum(Factor_Viaje[ModoAgregado == "Transporte Publico"])/sum(Factor_Viaje),
              Bicicleta = sum(Factor_Viaje[ModoAgregado == "Bicicleta"])/sum(Factor_Viaje),
              Caminata = sum(Factor_Viaje[ModoAgregado == "Caminata"])/sum(Factor_Viaje),
              Otros = sum(Factor_Viaje[ModoAgregado == "Otros"])/sum(Factor_Viaje))
  
  particion.modal <- rbind(particion.modal, viajes.summary)
}

            