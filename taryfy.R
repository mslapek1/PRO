library(dplyr)

# Standard 
standard_Innogy <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.3930),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.3930)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Standardowa", dostawca = "Innogy")

standard_PGE <- rbind(data.frame(hour = c(6:12, 15:21), 
                               price = 0.3539),
                    data.frame(hour = c(0:5, 13:14, 22:23),
                               price = 0.3539)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Standardowa", dostawca = "PGE")

standard_Tauron <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.4064),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.4064)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Standardowa", dostawca = "Tauron")

standard_Enea <- rbind(data.frame(hour = c(6:12, 15:21), 
                                price = 0.3569),
                     data.frame(hour = c(0:5, 13:14, 22:23),
                                price = 0.3569)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Standardowa", dostawca = "Enea")

standard_Energa <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.3675),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.3675)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Standardowa", dostawca = "Energa")


standard <- standard_Energa %>% 
  rbind(standard_Enea) %>% 
  rbind(standard_Tauron) %>% 
  rbind(standard_PGE) %>% 
  rbind(standard_Innogy)

# Dzien noc
dn_Innogy <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.4166),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.3688)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Nocna", dostawca = "Innogy")
  
dn_PGE <- rbind(data.frame(hour = c(6:12, 15:21), 
                                 price = 0.4039),
                      data.frame(hour = c(0:5, 13:14, 22:23),
                                 price = 0.2560)) %>% 
  arrange(hour) %>% 
    mutate(taryfa = "Nocna", dostawca = "PGE")

dn_Tauron <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.4797),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.2764)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Nocna", dostawca = "Tauron")

dn_Enea <- rbind(data.frame(hour = c(6:12, 15:21), 
                                price = 0.4159),
                     data.frame(hour = c(0:5, 13:14, 22:23),
                                price = 0.2697)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Nocna", dostawca = "Enea")

dn_Energa <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price = 0.4584),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price = 0.3044)) %>% 
  arrange(hour) %>% 
  mutate(taryfa = "Nocna", dostawca = "Energa")

dn <- dn_Energa %>% 
  rbind(dn_Enea) %>% 
  rbind(dn_Tauron) %>% 
  rbind(dn_PGE) %>% 
  rbind(dn_Innogy)

weekendowa_Innogy <- 
  data.frame(hour = rep(c(0:5, 22:23), times = 5), 
             price = 0.3765, 
             day = rep(1:5, each = 8)) %>% 
  rbind(data.frame(hour = rep(6:21, times = 5),
                   price = 0.4311, 
                   day = rep(1:5, each = 16))) %>% 
  rbind(data.frame(hour = rep(0:23, times = 2), 
                   price = 0.3765, 
                   day = rep(6:7, each = 24))) %>% 
  arrange(day, hour) %>% 
  mutate(taryfa = "Weekendowa", dostawca = "Innogy")

weekendowa_Tauron <- 
  data.frame(hour = rep(c(0:5, 22:23), times = 5), 
             price = 0.2376, 
             day = rep(1:5, each = 8)) %>% 
  rbind(data.frame(hour = rep(6:21, times = 5),
                   price = 0.4895, 
                   day = rep(1:5, each = 16))) %>% 
  rbind(data.frame(hour = rep(0:23, times = 2), 
                   price = 0.2376, 
                   day = rep(6:7, each = 24))) %>% 
  arrange(day, hour) %>% 
  mutate(taryfa = "Weekendowa", dostawca = "Tauron")

weekendowa_Enea <- 
  data.frame(hour = rep(c(0:5, 21:23), times = 5), 
             price = 0.2831, 
             day = rep(1:5, each = 9)) %>% 
  rbind(data.frame(hour = rep(6:20, times = 5),
                   price = 0.4353, 
                   day = rep(1:5, each = 15))) %>% 
  rbind(data.frame(hour = rep(0:23, times = 2), 
                   price = 0.2831, 
                   day = rep(6:7, each = 24))) %>% 
  arrange(day, hour) %>% 
  mutate(taryfa = "Weekendowa", dostawca = "Enea")

weekendowa_Energa <- 
  data.frame(hour = rep(c(0:5, 13:14, 22:23), times = 5), 
             price = 0.2831, 
             day = rep(1:5, each = 10)) %>% 
  rbind(data.frame(hour = rep(c(6:12, 15:21), times = 5),
                   price = 0.4353, 
                   day = rep(1:5, each = 14))) %>% 
  rbind(data.frame(hour = rep(0:23, times = 2), 
                   price = 0.2831, 
                   day = rep(6:7, each = 24))) %>% 
  arrange(day, hour) %>% 
  mutate(taryfa = "Weekendowa", dostawca = "Energa")

weekendowa <- weekendowa_Energa %>% 
  rbind(weekendowa_Enea) %>% 
  rbind(weekendowa_Tauron) %>% 
  rbind(weekendowa_Innogy)
