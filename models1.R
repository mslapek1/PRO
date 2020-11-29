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




