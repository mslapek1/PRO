library(dplyr)
data <- read.csv("cleaned_HomeC.csv")
data <- data %>%
  select(time, use..kW.) %>%
  mutate(time = gsub("2016", "2020", time)) %>%
  mutate(time = as.POSIXlt(time, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(time = round(time - 30*60, units = "hours")) %>% 
  group_by(time) %>% 
  summarise(use..kW. = sum(use..kW.)) %>% 
  mutate(day = time$wday, hour = time$hour)


write.csv(data, file = "forBarplot_HomeC.csv")
