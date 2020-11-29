library(ranger)
library(treeshap)

set <- read.csv("HomeC.csv")
# set2 <- set
# set <- set2
set$time <- as.numeric(levels(set$time))[set$time]
set <- na.omit(set)
set$time <- (set$time - set$time[1]) * 60 + set$time[1]
set$time2 <- as.POSIXlt(set$time, origin = "1970-01-01")

# Building features
set$hour <- set$time2$hour
set$wday <- set$time2$wday
set$month <- set$time2$mon
set$mday <- set$time2$mday

set$season <- "winter"
set$season[set$mday > 7 & set$mday < 11] <- "autumn"
set$season[set$mday > 4 & set$mday < 8] <- "summer"
set$season[set$mday > 2 & set$mday < 5] <- "spring"

set$dayp <- "morning"
set$dayp[set$hour > 10 & set$hour < 17] <- "noon"
set$dayp[set$hour > 16 & set$hour < 23] <- "afternoon"
set$dayp[set$hour > 22 | set$hour < 6] <- "night"
x <- set[, c(2, 3, 20:32, 34:39)]

# building models for each target
set.seed(12)
samp <- sample(1:nrow(x), 0.2 * nrow(x))
for(i in 1:1) {
  # creating dataset
  yi <- set[, 4 + i]
  xi <- cbind(x, yi)
  #xi <- x1[1:50000, ]
  xi_train <- xi[-samp, ]
  xi_test <- xi[samp, ]
  
  ranger_model <- ranger(yi ~ ., xi_train, num.trees = 400)
  saveRDS(ranger_model, file = paste0("models1/ranger", i, ".RDS"))
}

um <- ranger.unify(ranger_model, xi_train)
shaps <- treeshap(um, xi_train[1:11000, ])
plot_feature_importance(shaps)

p1 <- predict(ranger_model, xi_test[1:100, ])
p2 <- xi_test[1:100, "y1"]
df <- data.frame(p1 = p1$predictions, p2 = p2)




# liczenie taryf czy sie opłaca
library(dplyr)
taryfa_Innogy <- rbind(data.frame(hour = c(6:12, 15:21), 
                            price.dn = 0.4166, 
                            price.standard = 0.3930),
                 data.frame(hour = c(0:5, 13:14, 22:23),
                            price.dn = 0.3688, 
                            price.standard = 0.3930)) %>% 
  arrange(hour)

taryfa_PGE <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price.dn = 0.4039, 
                                  price.standard = 0.3539),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price.dn = 0.2560, 
                                  price.standard = 0.3539)) %>% 
  arrange(hour)

taryfa_Tauron <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price.dn = 0.4797, 
                                  price.standard = 0.4064),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price.dn = 0.2764, 
                                  price.standard = 0.4064)) %>% 
  arrange(hour)

taryfa_Enea <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price.dn = 0.4159, 
                                  price.standard = 0.3569),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price.dn = 0.2697, 
                                  price.standard = 0.3569)) %>% 
  arrange(hour)

taryfa_Energa <- rbind(data.frame(hour = c(6:12, 15:21), 
                                  price.dn = 0.4584, 
                                  price.standard = 0.3675),
                       data.frame(hour = c(0:5, 13:14, 22:23),
                                  price.dn = 0.3044, 
                                  price.standard = 0.3675)) %>% 
  arrange(hour)

taryfy <- list(Innogy = taryfa_Innogy, PGE = taryfa_PGE, Tauron = taryfa_Tauron, Energa = taryfa_Energa, Enea = taryfa_Enea)

for (i in 1:5) {
  taryfa <- taryfy[[i]]
  print(names(taryfy)[i])
  
  day_from <- 0
  day_to <- 349
  full_saving <- x[(1 + 60 * 24 * day_from):(60 * 24 * day_to), ] %>% 
    select(use..kW., hour, month) %>% 
    left_join(taryfa, by = "hour") %>% 
    summarise(sum.dn = sum(use..kW. * price.dn / 60), 
              sum.standard = sum(use..kW. * price.standard / 60)) %>% 
    mutate(saving = sum.standard - sum.dn) %>% 
    pull(saving)
  print(full_saving)
  
  saving_montly <- x[(1 + 60 * 24 * day_from):(60 * 24 * day_to), ] %>% 
    select(use..kW., hour, month) %>% 
    left_join(taryfa, by = "hour") %>% 
    group_by(month) %>% 
    summarise(sum.dn = sum(use..kW. * price.dn / 60), 
              sum.standard = sum(use..kW. * price.standard / 60)) %>% 
    mutate(saving = sum.standard - sum.dn, month = month + 1) %>% 
    select(month, saving)
  print(saving_montly)
}

