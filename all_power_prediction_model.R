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
  summarise( House_metering=sum(House_metering))

#date data enginirning
df$day_of_week <- as.numeric( format( df$day, "%w"))

#checked: year is not singificat
#df$year <- as.numeric( format( df$day, "%Y")) - 2000

#can be assumed that day of week is irrelewant
plot(House_metering~day, df, 
     col=df$day_of_week, pch=df$day_of_week,
     main="power usage by the day of the week")

#### na replace
#na will be raplaced with mean of 4 neigbors in the same week_day (-14,-7,+7,+14)
dfGi <- df$House_metering

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

df$House_metering <- dfGi

df_save <- df

#reaches best adjR^2 in 120 days
i<-n
df <- df_save[(n-i):n,]

#viual check if replace points are good with model
plot(df$day, df$House_metering,
     main="replaced points in red")
df_na <- df[na_vec,]
points(df_na$day, df_na$House_metering, 
       col="red")

####modeling
#assumptions:
#1.periodic poweer usage with pick ~14.01
#2.day of week are inrelewant
#3.spring and autum behave the same

#day counting from begining of the year
#[1,365] -> [0,2pi] day 366 i thaken into consideration
funtion_day_to_cos_rad_of_day_of_year <- function(v_day){
  
  day_of_year <- as.numeric( strftime(v_day, format = "%j"))
  
  day_of_year_in_rad <- (day_of_year-1) / 365 * 2*pi
  return( cos(day_of_year_in_rad))
  
}

funtion_day_week_to_cos <- function(week){
  
  day_week_in_rad <- week / 7 * 2*pi
  return( cos(day_week_in_rad))
  
}
#model which starts from 01.01: adjR^2 = 0.367
#model which starts from 14.01: adjR^2 = 0.39
#model which starts from 14.01
# and week taken into acount  : adjR^2 = 0.408

#add month for making the 14.01 the zero day
df$cos_day_of_year_in_rad <- funtion_day_to_cos_rad_of_day_of_year( df$day %m+% days(-14))

df$cos_day_week_in_rad <- funtion_day_week_to_cos( df$day_of_week)

#linear model can be aplied
model_l <- lm(House_metering~cos_day_of_year_in_rad,df)


plot(House_metering~cos_day_of_year_in_rad, df,
     main="model in red")
abline(model_l,col = 'red')


#plot in normal
pred <- predict(model_l, df[,c("cos_day_of_year_in_rad")])

plot(df$day, df$House_metering,
     main="model in red")
points(df$day, pred, col="red", cex=0.1)

summary(model_l)


####test
df_save <- df

#reaches best adjR^2 in 120 days
i<-20
df_test <- df_save[(n-i):(n-1),]
df_train <- df_save[(n-i*9):(n-i),]

model_test <- lm(House_metering~cos_day_of_year_in_rad,df_train)

df_test$predicted <- predict(model_test, df_test[,c("cos_day_of_year_in_rad")])

plot(df_test$day, df_test$House_metering,
     main="model in red")
points(df_test$day, df_test$predicted, col="red", cex=0.1)

diff_pred <- abs( df_test$predicted - df_test$House_metering)

plot(diff_pred)
