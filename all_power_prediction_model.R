path <- "/Users/mikolajmalec/Desktop/projekt inter/"

library(readr)
library(dplyr)
library(lubridate)

#load data
df_read <- read_delim( paste0( path,"household_power_consumption.txt"),";", escape_double = FALSE, trim_ws = TRUE)
df <- df_read

###data engenirng and agregation to days

#date
df$Date <- as.POSIXct( paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M")
df$Time <-NULL

#house energy usage wat-hour = S1 + S2 + S3 + S4
df$House_metering <- df$Global_active_power*1000/60

#agregegation
df <- df %>%
  group_by(day=floor_date(Date, "day")) %>%
  summarise( Global_intensity=sum(House_metering))


df$day_of_week <- as.numeric( format( df$day, "%w"))

#### na replace
#na will be raplaced with mean of 4 neigbors in the same week_day (-14,-7,+7,+14)
dfGi <- df$Global_intensity

na_vec <- which( is.na(dfGi))
n <- length(dfGi)

for (i in na_vec) {
  s <- 0
  k <- 0
  if(i>14){
    if( !is.na(dfGi[i-14])){
      s <- s + dfGi[i-14]
      k <- k+1
    }
  }
  if(i>7){
    if( !is.na(dfGi[i-7])){
      s <- s + dfGi[i-7]
      k <- k+1
    }
  }
  if(i<n-14){
    if( !is.na(dfGi[i+14])){
      s <- s + dfGi[i+14]
      k <- k+1
    }
  }
  if(i<n-7){
    if( !is.na(dfGi[i+7])){
      s <- s + dfGi[i+7]
      k <- k+1
    }
  }
  if(k!=0){
    dfGi[i] <- s/k
  }
}
#where all na-value replaced?
all(!is.na(dfGi))

df$Global_intensity <- dfGi

df_save <- df
df <- df_save

#viual check if replace points are good with model
plot(df$day, df$Global_intensity)
df_na <- df[na_vec,]
points(df_na$day, df_na$Global_intensity, col="red")

####modeling
#assumptions:
#1.periodic poweer usage with pick ~01.01
#2.day of week inrelewant

#day counting from begining of the year
#[1,365] -> [0,2pi] day 366 i thaken into consideration
funtion_day_to_cos_rad_of_day_of_year <- function(v_day){
  
  day_of_year <- as.numeric( strftime(v_day, format = "%j"))
  
  day_of_year_in_rad <- (day_of_year-1) / 365 * 2*pi
  return( cos(day_of_year_in_rad))
  
}

df$cos_day_of_year_in_rad <- funtion_day_to_cos_rad_of_day_of_year(df$day)

plot(Global_intensity~cos_day_of_year_in_rad, df)

#linear model can be aplied
model_l <- lm(Global_intensity~cos_day_of_year_in_rad, df)

summary(model_l)

plot(Global_intensity~cos_day_of_year_in_rad, df)
abline(model_l,col = 'red')


#plot in normal
pred <- predict(model_l, data.frame( cos_day_of_year_in_rad = df$cos_day_of_year_in_rad))

plot(df$day, df$Global_intensity)
points(df$day, pred, col="red", cex=0.1)






