path <- "/Users/mikolajmalec/Desktop/projekt inter/"

library(readr)
library(dplyr)
library(lubridate)

#load data
df_read <- read_delim( paste0( path,"household_power_consumption.txt"),";", escape_double = FALSE, trim_ws = TRUE)
df <- df_read

df <- df[sample(1:1000),]

#data engenirng

# active energy consumed every minute (in watt hour) in the household by electrical equipment not measured in sub-meterings 1, 2 and 3. 
df$Sub_metering_4 <- df$Global_active_power*1000/60 - df$Sub_metering_1 - df$Sub_metering_2 - df$Sub_metering_3

#house energy usage wat-hour = S1 + S2 + S3 + S4
df$House_metering <- df$Global_active_power*1000/60

#date
df$Date <- as.POSIXct( paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M")
df$Time <-NULL
#year, month, day, hour, minte
df$year <- as.numeric( format(df$Date, "%Y"))
df$month <- as.numeric( format(df$Date, "%m"))
df$day <- as.numeric( format(df$Date, "%d"))
df$hour <- as.numeric( format(df$Date, "%H"))
df$minte <- as.numeric( format(df$Date, "%M"))
df$Date <- NULL

##note (active_power^2 + reactive_power^2)^-1/2 == voltage * intensity
#kW1 <- (df$Global_active_power^2 + df$Global_reactive_power^2)^(1/2)
#kW2 <- (df$Voltage * df$Global_intensity) / 1000
#boxplot( abs( kW1-kW2))






