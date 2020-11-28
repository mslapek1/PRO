path <- "/Users/mikolajmalec/Desktop/projekt inter/"

library(readr)
library(dplyr)
library(lubridate)

#load data
df_read <- read_delim( paste0( path,"household_power_consumption.txt"),";", escape_double = FALSE, trim_ws = TRUE)
df <- df_read

###data engenirng and agregation to weeks

#date
df$Date <- as.POSIXct( paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M")
df$Time <-NULL

#house energy usage wat-hour = S1 + S2 + S3 + S4
df$House_metering <- df$Global_active_power*1000/60

#agregegation
df <- df %>%
  group_by(week=floor_date(Date, "week")) %>%
  summarise( House_metering=sum(House_metering))

#### na replace
#na will be raplaced with mean of 4 neigbors  (-2,-1,+1,+2)
dfGi <- df$House_metering

na_vec <- which( is.na(dfGi))
n <- length(dfGi)

for (i in na_vec) {
  s <- 0
  k <- 0
  if(i>14){
    if( !is.na(dfGi[i-2])){
      s <- s + dfGi[i-2]
      k <- k+1
    }
  }
  if(i>7){
    if( !is.na(dfGi[i-1])){
      s <- s + dfGi[i-1]
      k <- k+1
    }
  }
  if(i<n-14){
    if( !is.na(dfGi[i+2])){
      s <- s + dfGi[i+2]
      k <- k+1
    }
  }
  if(i<n-7){
    if( !is.na(dfGi[i+1])){
      s <- s + dfGi[i+1]
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


i<-n
df <- df_save[(n-i):n,]

#viual check if replace points are good with model
plot(df$week, df$House_metering,
     main="replaced points in red")
df_na <- df[na_vec,]
points(df_na$week, df_na$House_metering, 
       col="red")

####modeling
#assumptions:
#1.periodic poweer usage with pick ~14.01
#2.spring and autum behave the same

#week counting from begining of the year
#[1,52] -> [0,2pi]
funtion_week_to_cos_rad_of_week_of_year <- function(v_week){
  
  week_of_year <- as.numeric( strftime(df$week, format = "%V"))
  
  week_of_year_in_rad <- (week_of_year-1) / 52 * 2*pi
  return( cos(week_of_year_in_rad))
  
}
#model which starts from 01.01: adjR^2 = 0.367
#model which starts from 14.01: adjR^2 = 0.39
#model which starts from 14.01
# and week taken into acount  : adjR^2 = 0.408


df$cos_week_of_year_in_rad <- funtion_week_to_cos_rad_of_week_of_year( df$week %m+% weeks(-2) )


#linear model can be aplied
model_l <- lm(House_metering~cos_week_of_year_in_rad,df)


plot(House_metering~cos_week_of_year_in_rad, df,
     main="model in red")
abline(model_l,col = 'red')


#plot in normal
pred <- predict(model_l, df[,c("cos_week_of_year_in_rad")])

plot(df$week, df$House_metering,
     main="model in red")
points(df$week, pred, col="red", cex=0.1)

#adjR^2 = 0.545
summary(model_l)

