path <- "/Users/mikolajmalec/Desktop/projekt inter/"

library(ggplot2)
library(readr)
df <- read_delim( paste0( path,"household_power_consumption.txt"),";", escape_double = FALSE, trim_ws = TRUE)

colnames(df)


apply(is.na(df),2, sum) # w każdej kolumnie z NA jest tyle samo NA!

k <- 0
for (i in 1:(nrow(df))){
  if(is.na(df$Voltage[i]))
    k <- k + 1
}

k2 <- 0
for (i in 1:nrow(df)){
  if(is.na(df$Voltage[i]) && is.na(df$Global_active_power[i]) && is.na(df$Global_reactive_power[i]) && is.na(df$Global_intensity[i]) && is.na(df$Sub_metering_1[i]) && is.na(df$Sub_metering_2[i]) && is.na(df$Sub_metering_3[i]))
    k2 <- k2 + 1
}
# k = k2
# that means we cannot use machine learning prediction to impute NAs
# the only imputation methods:  local average  or data deletion


# DATA IMPUTATION = DELETION, AFTER CONSULTATION WITH TEAM [ continuity in plots is not needed]:
df2 <- na.omit(df)



pattern <- "^[1-3]*[0-9]/[1]*[0-9]/20[0-9][0-9]$"
length(df2[-grep(df2$Date, pattern = pattern),]$Date) # 0 niepasujacych, 2049280 pasujących

pattern <- "^[0-2]*[0-9]:[0-5]*[0-9]:[0-5][0-9]$"
length(df2[-grep(df2$Time, pattern = pattern),]$Time) # 0 niepasujacych, 2049280 pasujących

boxplot(df2$Global_active_power, main = "Global_active_power")
boxplot(df2$Global_reactive_power, main = "Global_reactive_power")
boxplot(df2$Voltage, main = "Voltage")
boxplot(df2$Global_intensity, main = "Global_intensity")
min(df2$Global_active_power) # 0.076
max(df2$Global_active_power) # 11.122
min(df2$Global_reactive_power) # 0
max(df2$Global_reactive_power) # 1.39
min(df2$Voltage) # 223.2
max(df2$Voltage) # 254.15
min(df2$Global_intensity) # 0.2
max(df2$Global_intensity) # 48.4


ggplot(df2, aes(x = Sub_metering_1)) + geom_histogram() + scale_y_log10()
ggplot(df2, aes(x = Sub_metering_2)) + geom_histogram() + scale_y_log10()
ggplot(df2, aes(x = Sub_metering_3)) + geom_histogram() + scale_y_log10()

levels(as.factor(df2$Sub_metering_1)) # 0-88 all values  and OK
levels(as.factor(df2$Sub_metering_2)) # 0-80 all values  and OK
levels(as.factor(df2$Sub_metering_3)) #0-31 all values  and OK

