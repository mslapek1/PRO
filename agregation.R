path <- "/Users/mikolajmalec/Desktop/projekt inter/"

library(readr)
library(dplyr)
library(lubridate)

df_dirty <- read_delim( paste0( path,"household_power_consumption.txt"),";", escape_double = FALSE, trim_ws = TRUE)
#cleaing
df <- na.omit(df_dirty)

df$Date <- as.POSIXct( paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M")
df$Time <-NULL

df_hourly <- df %>%
  group_by(month=floor_date(Date, "hour")) %>%
  summarise( Global_intensity=sum(Global_intensity), 
             Sub_metering_1 = sum(Sub_metering_1), 
             Sub_metering_2 = sum(Sub_metering_2), 
             Sub_metering_3 = sum(Sub_metering_3)) %>%
  na.omit()

df_daily <- df %>%
  group_by(month=floor_date(Date, "day")) %>%
  summarise( Global_intensity=sum(Global_intensity), 
             Sub_metering_1 = sum(Sub_metering_1), 
             Sub_metering_2 = sum(Sub_metering_2), 
             Sub_metering_3 = sum(Sub_metering_3)) %>%
  na.omit()

df_weekly <- df %>%
  group_by(month=floor_date(Date, "week", week_start = getOption("lubridate.week.start", 7))) %>%
  summarise( Global_intensity=sum(Global_intensity), 
             Sub_metering_1 = sum(Sub_metering_1), 
             Sub_metering_2 = sum(Sub_metering_2), 
             Sub_metering_3 = sum(Sub_metering_3)) %>%
  na.omit()

df_monthly <- df %>%
  group_by(month=floor_date(Date, "month")) %>%
  summarise( Global_intensity=sum(Global_intensity), 
             Sub_metering_1 = sum(Sub_metering_1), 
             Sub_metering_2 = sum(Sub_metering_2), 
             Sub_metering_3 = sum(Sub_metering_3)) %>%
  na.omit()

df_yearly <- df %>%
  group_by(month=floor_date(Date, "year")) %>%
  summarise( Global_intensity=sum(Global_intensity), 
             Sub_metering_1 = sum(Sub_metering_1), 
             Sub_metering_2 = sum(Sub_metering_2), 
             Sub_metering_3 = sum(Sub_metering_3)) %>%
  na.omit()

