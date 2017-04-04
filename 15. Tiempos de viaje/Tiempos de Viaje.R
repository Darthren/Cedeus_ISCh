# Script to calculate the total ratio of travel means to work vs total travels to work.
# It uses data from the Encuesta Origen-Destino published by SECTRA (Subsecretaria de Transporte)
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Code written by Rodrigo Villegas. E-Mail: rdgo.villegas@gmail.com

# Loading libraries and setting working directory

library(data.table); library(dplyr); library(chron)
setwd("E:/Cedeus Sustainability Indicators/Datos/EOD")

#### Temuco ####

# Reading Temuco EOD

temuco <- as.data.frame(fread("Temuco_viajes.csv", sep=";", header= T)) 
temuco$TiempoViaje <- strptime(temuco$TiempoViaje, "%d-%m-%Y %H:%M:%S")
temuco$TiempoViaje <- format(temuco$TiempoViaje,"%H:%M:%S")
temuco$TiempoViaje <- chron(times= temuco$TiempoViaje)
temuco$Factor <- as.numeric(gsub(pattern = ",",replacement = ".", x = temuco$Factor))

temuco.summary <- temuco %>%
  group_by(IDFolio, IdPersona, IdViaje) %>%
  summarise(Tiempo.viaje.total = sum((as.numeric(TiempoViaje)*(24*60*60)*365*Factor)),
            Factor = mean(Factor)) %>%
  group_by(IDFolio, IdPersona) %>%
  summarise(Tiempo.viaje.promedio.anual = sum(Tiempo.viaje.total)) %>%
  ungroup(IDFolio, IdPersona) %>%
  summarise(Tiempo.viaje.promedio.anual = sum(Tiempo.viaje.promedio.anual)/sum(Factor)) %>%
  mutate(Tiempo.viaje.promedio.anual = round(seconds_to_period(Tiempo.viaje.promedio.anual)))

