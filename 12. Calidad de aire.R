# TODO: Traducir al español
## Script that calculates ratio of days over 100 PM2.5 over the year.
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Data files consist of PM2.5 data from monitoring stations in comunas of interest.
# Code written by Rodrigo Villegas. E-Mail: rdgo.villegas@gmail.com

library(data.table); library(dplyr); library(readxl)
setwd("E:/Cedeus Sustainability Indicators/Datos/Calidad Aire")

# Create an empty dataframe to store the results
city_df <- data.frame()

# Store the original working directory
oldwd <- getwd()

# For every folder:
for (e in 1:length(list.files(oldwd))){
  # Go to the original folder (where all the folders with the data are stored)
  setwd(oldwd)
  # Store the folder name to analyze, which also is the city name.
  city <- list.files(oldwd)[e]
  # Set as working directory the folder with the stations data
  newwd <- setwd(list.files(oldwd)[e])
  # For every file in folder:
  for (i in 1:length(list.files())){
    # Reading file [i]
    station_data <- as.data.frame(fread(list.files()[i], sep=";", dec = ",",header= T))
    # Getting the station name from the file name.
    station_name <- list.files()[i]
    # years to analyze: 2013, 2014, 2015
    years <- c("13","14","15")
    # For every year of analysis:
    for (u in 1:length(years)) {
      # Setting initial date and final date in teh format YYMMDD
      year_ini <- paste(years[u],"0101", sep="")
      year_end <- paste(years[u],"1231", sep="")
      # Subsetting the data for the years of interest.
      tmp <- station_data[station_data$`FECHA (YYMMDD)` >= year_ini & station_data$`FECHA (YYMMDD)` <= year_end,]
      # variable to store number of total registers
      reg_days <- 0
      # variabe to store number of days PM2.5 >= 100
      over_100 <- 0
      # For each column with registers
      for (o in 3:6){
        # Data has "," as decimal sep. So, we'll replace it in order to convert it to numeric class.
        tmp[,o] <- as.numeric(gsub("," , ".", tmp[,o]))
        # Number of days with registers
        reg_days <- sum(reg_days,!(is.na(tmp[,o])))
        # Number of days over 100 PM2.5
        over_100 <- sum(over_100, tmp[,o] >= 100, na.rm=T)
      }
      # The year field. Combining the "20" and YY character in the YYMMDD field in the raw data.
      year <- paste("20",years[u], sep="")
      # The ratio of days over 100 PM2.5 and total registered days
      ratio <- over_100/reg_days
      # The ratio of days over 100 PM2.5 and total days in year.
      ratio_365 <- over_100/365
      # Merging all these variables in a dataframe
      tmp_df <- data.frame(station_name, year, over_100, reg_days, ratio, ratio_365, city)
      # Merging tmp_df with the output dataframe
      city_df <- rbind(city_df, tmp_df)
    }
    
  }
}


# Summarizing the data by city and by year.

city_summary <- city_df %>%
  group_by(city, year) %>%
  summarise(avg_over_100 = round(mean(over_100),3), 
            max_over_100 = max(over_100), 
            median_over_100 = median(over_100))

# Exporting the final results

setwd("E:/Cedeus Sustainability Indicators/Indicadores/Indicadores")
write.csv(city_summary, "12. Calidad de Aire.csv", row.names = F)
