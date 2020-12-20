
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

#FILE IS NOT IN RESPIRATORY
df_read <- read_delim( "household_power_consumption.txt",";", escape_double = FALSE, trim_ws = TRUE)
#save
df <- df_read
#only from 2010
df <- df %>% filter( str_detect( df$Date, "2010"))
#power usage
df$use..kW. <- df$Global_active_power*1000/60
#char to date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

df <- df %>% select( Date, Time, use..kW.)
#time -> hour (10:20:30 -> 10)
df$hour <- as.numeric( substr( df$Time, 1, 2))
df$Time <- NULL

#some heve Na
df[ which( is.na(df$use..kW.)),]
#we will get mean of power at the same point of day one week before and later (10080 minutes before and after)
n <- nrow(df)
for (int_to_raplace in which( is.na(df$use..kW.))) {
  if (int_to_raplace <= 10080) {
    df[int_to_raplace,"use..kW."] <- df$use..kW.[ int_to_raplace + 10080]
  }
  else if (int_to_raplace >= n - 10080) {
    df[int_to_raplace,"use..kW."] <- df$use..kW.[ int_to_raplace - 10080]
  }
  else {
    df[int_to_raplace,"use..kW."] <- mean( c( df$use..kW.[ int_to_raplace - 10080], df$use..kW.[ int_to_raplace + 10080]), na.rm = TRUE)
  }
}
#we heve no Na
df[ which( is.na(df$use..kW.)),]

df_filtered <- df %>% group_by( Date, hour) %>%
  summarise( use..kW. = sum( use..kW.))

#day of week, -1 so sunday =0
df_filtered$day <- wday( df_filtered$Date) -1

#visual check
plot(df_filtered$Date, df_filtered$use..kW.)
plot( df_filtered$Date[df_filtered$hour==12], df_filtered$use..kW.[df_filtered$hour==12])
#values seams ok

write.csv( df_filtered, "domek_we_francji.csv")
