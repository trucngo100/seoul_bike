bike.df<-read.csv('./data/bike_cleaned.csv', na.strings='')

dim(bike.df)
summary(bike.df)

str(bike.df)
bike.df<-bike.df[, c('bike_count', 'temp', 'humid', 'wind', 'visibility', 'dew_temp', 
                     'radiation', 'rainfall', 'snowfall', 'season', 'holiday', 'dat_type', 
                     'time_in_day')]
str(bike.df)
bike.df$season<-factor(bike.df$season)
bike.df$holiday<-factor(bike.df$holiday)
bike.df$dat_type<-factor(bike.df$dat_type)
bike.df$time_in_day<-factor(bike.df$time_in_day)

str(bike.df)

library(caret)
dummy.vars<-dummyVars(bike_count~., data=bike.df)
bike.dummy<-data.frame(predict(dummy.vars, newdata=bike.df))
bike.dummy$bike_count<-bike.df$bike_count

str(bike.dummy)

# 1. split 먼저
set.seed(1)
train.index<-sample(rownames(bike.dummy), 0.6*dim(bike.dummy)[1])
valid.index<-setdiff(row.names(bike.dummy), train.index)
train.df<-bike.dummy[train.index,]
valid.df<-bike.dummy[valid.index,]

train.scaled <- train.df
valid.scaled <- valid.df

# 2. train set 기준으로 scaling 객체 생성
scale.values <- caret::preProcess(train.scaled[, -c(9:22)], method = "range", rangeBounds = c(0, 1))

train.scaled <- predict(scale.values, train.df[, -c(9:22)])
valid.scaled <- predict(scale.values, valid.df[, -c(9:22)])

# 3. 다시 factor 더하기
train.scaled <- cbind(train.scaled, train.df[, c(9:22)])
valid.scaled <- cbind(valid.scaled, valid.df[, c(9:22)])

library(neuralnet)
nn.bike <- neuralnet(bike_count ~ ., data = train.scaled, linear.output = TRUE, 
                     hidden = c(10, 5), stepmax = 1e+07)
plot(nn.bike)

#train
library(caret)
input.vars <- setdiff(names(train.df), "bike_count")
training.pred <- compute(nn.bike, train.scaled[, input.vars])
head(training.pred$net.result, n=20)
nonscaled.training.pred = training.pred$net.result * 
                              (max(train.df$bike_count) - min(train.df$bike_count)) + min(train.df$bike_count)
head(nonscaled.training.pred, 10)


RMSE(nonscaled.training.pred, train.df$bike_count)

actual <- train.df$bike_count 
predicted <- nonscaled.training.pred 

mae <- mean(abs(actual - predicted))
cat("MAE:", mae, "\n")

ss_res <- sum((actual - predicted)^2)
ss_tot <- sum((actual - mean(actual))^2)
r_squared <- 1 - (ss_res / ss_tot)
cat("R-squared:", r_squared, "\n")

#valid
valid.pred <- compute(nn.bike, valid.scaled[, input.vars])
nonscaled.valid.pred <- (valid.pred$net.result * 
                           (max(train.df$bike_count) - min(train.df$bike_count))) + min(train.df$bike_count)
head(nonscaled.valid.pred, n=5)

RMSE(nonscaled.valid.pred, valid.df$bike_count)

actual <- valid.df$bike_count 
predicted <- nonscaled.valid.pred 

mae <- mean(abs(actual - predicted))
cat("MAE:", mae, "\n")

ss_res <- sum((actual - predicted)^2)
ss_tot <- sum((actual - mean(actual))^2)
r_squared <- 1 - (ss_res / ss_tot)
cat("R-squared:", r_squared, "\n")

df=data.frame(actual=bike.dummy[valid.index, 23],
              predicted=nonscaled.valid.pred)
head(df, n=10)
str(train.df)

new.df<-data.frame(temp=21, humid=66, wind=1, visibility=2100, 
                   dew_temp=13, radiation=0, rainfall=0, snowfall=0,
                   season.Autumn=0, season.Spring=0, season.Summer=1,
                   season.Winter=0, holiday.Holiday=0, holiday.No.Holiday=1,
                   dat_type.weekday=1, dat_type.weekend=0, time_in_day.afternoon=0,
                   time_in_day.dawn=0, time_in_day.early.morning=0, time_in_day.evening=0,
                   time_in_day.morning=0, time_in_day.night=1)

scale.values.new <- caret::preProcess(train.df[, 1:8], method = "range", rangeBounds = c(0, 1))

new.scaled <- predict(scale.values.new, new.df[, 1:8])
new.scaled <- cbind(new.scaled, new.df[, c(9:22)])
new.pred <- compute(nn.bike, new.scaled[, input.vars])
new.pred$net.result
nonscaled.new.pred = new.pred$net.result * 
  (max(train.df$bike_count) - min(train.df$bike_count)) + min(train.df$bike_count)
nonscaled.new.pred
