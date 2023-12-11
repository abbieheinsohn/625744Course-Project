install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("hms")
library(hms)
install.packages("dplyr")
library(dplyr)
install.packages("chron")
library(chron)
install.packages("psych")
library(psych)
install.packages("Metrics")
library(Metrics)

data <- read.csv("/Users/abbieheinsohn/Desktop/Modeling, 
                 Simulation and Monte Carlo/Course Project/flights.csv")

data$DATE <- as.Date(paste(data$YEAR, data$MONTH, data$DAY, sep = "-"))
data1 <- data[data$MONTH <= 6,]
data1 <- na.omit(data1)

variables_to_remove <- c('TAXI_OUT', 'TAXI_IN', 'WHEELS_ON', 'WHEELS_OFF', 'YEAR', 
                         'DAY_OF_WEEK', 'AIR_SYSTEM_DELAY',
                       'SECURITY_DELAY', 'AIRLINE_DELAY', 'LATE_AIRCRAFT_DELAY',
                       'WEATHER_DELAY', 'CANCELLATION_REASON',
                       'FLIGHT_NUMBER', 'TAIL_NUMBER', 'AIR_TIME','DT','Date_time')
new_data <- data1[,!(names(data1) %in% variables_to_remove)]
df <- na.omit(new_data)

## Southwest airlines
sw <- df[df$AIRLINE == 'WN',]
## origin airport - BWI
sw_bwi <- sw[sw$ORIGIN_AIRPORT == 'BWI',]
# Change Scheduled Departure Time from Integer to Minutes
times <- sw_bwi$SCHEDULED_DEPARTURE
times1 <- c()
times2 <- c()
for (i in 1:length(times)) {
  if (nchar(times[i]) == 3) {
    times1[i] <- substr(times[i],1,1)
    times2[i] <- substr(times[i],2,3)
  } else { 
    times1[i] <- substr(times[i],1,2)
    times2[i] <- substr(times[i],3,4)
  }
}
times1 <- as.numeric(times1)
times2 <- as.numeric(times2)
times_min <- times1*60 + times2
sw_bwi$SCHEDULED_DEPARTURE <- times_min

# Change Departure Time from Integer to Minutes
times_dt <- sw_bwi$DEPARTURE_TIME
times1_dt <- c()
times2_dt <- c()
for (i in 1:length(times_dt)) {
  if (nchar(times_dt[i]) == 3){
    times1_dt[i] <- substr(times_dt[i],1,1)
    times2_dt[i] <- substr(times_dt[i],2,3)
  } else if (nchar(times_dt[i]) == 4){
    times1_dt[i] <- substr(times_dt[i],1,2)
    times2_dt[i] <- substr(times_dt[i],3,4)
  } else if(nchar(times_dt[i]) == 2){
    times1_dt[i] <- 0
    times2_dt[i] <- substr(times_dt[i],1,2)
  } else {
    times1_dt[i] <- 0
    times2_dt[i] <- substr(times_dt[i],1,1)
  }
}
times1_dt <- as.numeric(times1_dt)
times2_dt <- as.numeric(times2_dt)
times_min_dt <- times1_dt*60 + times2_dt
sw_bwi$DEPARTURE_TIME <- times_min_dt

# Change Scheduled Arrival Time from Integer to Minutes
times_sa <- sw_bwi$SCHEDULED_ARRIVAL
times1_sa <- c()
times2_sa <- c()
for (i in 1:length(times_sa)) {
  if (nchar(times_sa[i]) == 3){
    times1_sa[i] <- substr(times_sa[i],1,1)
    times2_sa[i] <- substr(times_sa[i],2,3)
  } else if (nchar(times_sa[i]) == 4){
    times1_sa[i] <- substr(times_sa[i],1,2)
    times2_sa[i] <- substr(times_sa[i],3,4)
  } else if(nchar(times_sa[i]) == 2){
    times1_sa[i] <- 0
    times2_sa[i] <- substr(times_sa[i],1,2)
  } else {
    times1_sa[i] <- 0
    times2_sa[i] <- substr(times_sa[i],1,1)
  }
}
times1_sa <- as.numeric(times1_sa)
times2_sa <- as.numeric(times2_sa)
times_min_sa <- times1_sa*60 + times2_sa
sw_bwi$SCHEDULED_ARRIVAL <- times_min_sa

# Change Arrival Time from Integer to Minutes
times_at <- sw_bwi$ARRIVAL_TIME
times1_at <- c()
times2_at <- c()
for (i in 1:length(times_at)) {
  if (nchar(times_at[i]) == 3){
    times1_at[i] <- substr(times_at[i],1,1)
    times2_at[i] <- substr(times_at[i],2,3)
  } else if (nchar(times_at[i]) == 4){
    times1_at[i] <- substr(times_at[i],1,2)
    times2_at[i] <- substr(times_at[i],3,4)
  } else if(nchar(times_at[i]) == 2){
    times1_at[i] <- 0
    times2_at[i] <- substr(times_at[i],1,2)
  } else {
    times1_at[i] <- 0
    times2_at[i] <- substr(times_at[i],1,1)
  }
}
times1_at <- as.numeric(times1_at)
times2_at <- as.numeric(times2_at)
times_min_at <- times1_at*60 + times2_at 
sw_bwi$ARRIVAL_TIME <- times_min_at



# Repeated CV using Linear Regression Model
train <- sw_bwi[sw_bwi$DAY < 16,]
test <- sw_bwi[sw_bwi$DAY >= 16,]
set.seed(22342)
tc <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 5,savePredictions = TRUE)
model <- train(DEPARTURE_DELAY ~ SCHEDULED_DEPARTURE + SCHEDULED_TIME + 
                 ELAPSED_TIME+DISTANCE + SCHEDULED_ARRIVAL, data = train, 
               method = "glm", trControl = tc)
print(model)
model$resample
model$finalModel
summary(model)
predict <- predict(model, test)
RMSE <- rmse(test$DEPARTURE_DELAY,predict)


################################# format time for all of SW
# make into minutes sch dept
sw <- df[df$AIRLINE == 'WN',]
sw <- na.omit(sw)
times <- aa$SCHEDULED_DEPARTURE
times1 <- c()
times2 <- c()
for (i in 1:length(times)) {
  if (nchar(times[i]) == 3){
    times1[i] <- substr(times[i],1,1)
    times2[i] <- substr(times[i],2,3)
  if (nchar(times[i]) == 4){
      times1[i] <- substr(times[i],1,2)
      times2[i] <- substr(times[i],3,4)
  }
  }else{
    times1[i] <- 0
    times2[i] <- substr(times[i],1,2)
  }
}
times1 <- as.numeric(times1)
times2 <- as.numeric(times2)
times_min <- times1*60 + times2
aa$SCHEDULED_DEPARTURE <- times_min

# make into minutes dept time
times_dt <- aa$DEPARTURE_TIME
times1_dt <- c()
times2_dt <- c()
for (i in 1:length(times_dt)) {
  if (nchar(times_dt[i]) == 3){
    times1_dt[i] <- substr(times_dt[i],1,1)
    times2_dt[i] <- substr(times_dt[i],2,3)
    if (nchar(times_dt[i]) == 4){
      times1_dt[i] <- substr(times_dt[i],1,2)
      times2_dt[i] <- substr(times_dt[i],3,4)
    }
  }else{
    times1_dt[i] <- 0
    times2_dt[i] <- substr(times_dt[i],1,2)
  }
}
times1_dt <- as.numeric(times1_dt)
times2_dt <- as.numeric(times2_dt)
times_min_dt <- times1_dt*60 + times2_dt
aa$DEPARTURE_TIME <- times_min_dt

# make into minutes sch arr
times_sa <- aa$SCHEDULED_ARRIVAL
times1_sa <- c()
times2_sa <- c()
for (i in 1:length(times_sa)) {
  if (nchar(times_sa[i]) == 3){
    times1_sa[i] <- substr(times_sa[i],1,1)
    times2_sa[i] <- substr(times_sa[i],2,3)
    if (nchar(times_sa[i]) == 4){
      times1_sa[i] <- substr(times_sa[i],1,2)
      times2_sa[i] <- substr(times_sa[i],3,4)
    }
  }else{
    times1_sa[i] <- 0
    times2_sa[i] <- substr(times_sa[i],1,2)
  }
}
times1_sa <- as.numeric(times1_sa)
times2_sa <- as.numeric(times2_sa)
times_min_sa <- times1_sa*60 + times2_sa
aa$SCHEDULED_ARRIVAL <- times_min_sa

# make into minutes arrival time
times_at <- aa$ARRIVAL_TIME
times1_at <- c()
times2_at <- c()
for (i in 1:length(times_at)) {
  if (nchar(times_at[i]) == 3){
    times1_at[i] <- substr(times_at[i],1,1)
    times2_at[i] <- substr(times_at[i],2,3)
    if (nchar(times_at[i]) == 4){
      times1_at[i] <- substr(times_at[i],1,2)
      times2_at[i] <- substr(times_at[i],3,4)
    }
  }else{
    times1_at[i] <- 0
    times2_at[i] <- substr(times_at[i],1,2)
  }
}
times1_at <- as.numeric(times1_at)
times2_at <- as.numeric(times2_at)
times_min_at <- times1_at*60 + times2_at 
aa$ARRIVAL_TIME <- times_min_at

# Repeated CV for all Southwest Flights
train1 <- sw[sw$DAY < 16,]
test1 <- sw[sw$DAY >= 16,]
set.seed(22342)
tc <- trainControl(method = "repeatedcv",
                   number =10, repeats = 5, savePredictions = TRUE)
model <- train(DEPARTURE_DELAY ~ SCHEDULED_DEPARTURE + SCHEDULED_TIME + 
                 ELAPSED_TIME+DISTANCE+ SCHEDULED_ARRIVAL, data = train1, 
               method = "glm", trControl = tc,
               metric = "RMSE")
print(model)
model$resample
model$finalModel
summary(model)
predict <- predict(model, test1)
RMSE <- rmse(test1$DEPARTURE_DELAY,predict)

hist(sw_bwi$DEPARTURE_DELAY, breaks = 40, main = "Histogram of Departure Delays at BWI (Southwest Airlines)",
     xlab = "Value", ylab = "Frequency")

##### Multiple Comparisons
unique(df$AIRLINE)
nk <- df[df$AIRLINE == 'NK',]
nk <- nk[,c(3,8)]
ha <- df[df$AIRLINE == 'HA',]
ha <- ha[,c(3,8)]
b6 <- df[df$AIRLINE == 'B6',]
b6 <- b6[,c(3,8)]
aa <- df[df$AIRLINE == 'AA',]
aa <- aa[,c(3,8)]
us <- df[df$AIRLINE == 'US',]
us <- us[,c(3,8)]
ua <- df[df$AIRLINE == 'UA',]
ua <- ua[,c(3,8)]
dl <- df[df$AIRLINE == 'DL',]
dl <- dl[,c(3,8)]
oo <- df[df$AIRLINE == 'OO',]
oo <- oo[,c(3,8)]
f9 <- df[df$AIRLINE == 'F9',]
f9 <- f9[,c(3,8)]
ev <- df[df$AIRLINE == 'EV',]
ev <- ev[,c(3,8)]
wn <- df[df$AIRLINE == 'WN',]
wn <- wn[,c(3,8)]
mq <- df[df$AIRLINE == 'MQ',]
mq <- mq[,c(3,8)]
as <- df[df$AIRLINE == 'AS',]
as <- as[,c(3,8)]
vx <- df[df$AIRLINE == 'VX',]
vx <- vx[,c(3,8)]

mc <-rbind(nk,ha,b6,aa,us,ua,dl,oo,f9,ev,wn,mq,as,vx)
mc$AIRLINE <- as.factor(mc$AIRLINE)
model1 <- aov(DEPARTURE_DELAY~AIRLINE, data = mc)
anova(model1)
TukeyHSD(model1, conf.level = 0.90)
plot(TukeyHSD(model1, conf.level=.95), las = 2)

