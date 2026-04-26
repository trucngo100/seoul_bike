bike.df<-read.csv('./data/bike_cleaned.csv', na.strings='')

dim(bike.df)
summary(bike.df)

str(bike.df)
bike.df<-bike.df[, c('bike_count', 'temp', 'humid', 'wind', 'visibility', 
                     'dew_temp', 'radiation', 'rainfall', 'snowfall', 
                     'season', 'holiday', 'dat_type', 'time_in_day')]
str(bike.df)
bike.df$bike_count<-as.numeric(bike.df$bike_count)
bike.df$humid<-as.numeric(bike.df$humid)
bike.df$visibility<-as.numeric(bike.df$visibility)
bike.df$season<-factor(bike.df$season)
bike.df$holiday<-factor(bike.df$holiday)
bike.df$dat_type<-factor(bike.df$dat_type)
bike.df$time_in_day<-factor(bike.df$time_in_day)

str(bike.df)

library(caret)
dummy.vars<-dummyVars(bike_count~., data=bike.df)
bike.dummy<-data.frame(predict(dummy.vars, newdata=bike.df))
bike.dummy$bike_count<-bike.df$bike_count

set.seed(1)
spl<-sample(c(1:3), size=nrow(bike.dummy), 
            replace=TRUE, prob=c(0.6, 0.2, 0.2))
train.df<-bike.dummy[spl==1, ]
valid.df<-bike.dummy[spl==2, ]
test.df<-bike.dummy[spl==3, ]

preProc<-preProcess(train.df[, -23], method=c('center', 'scale'))
train.norm.df<-train.df
train.norm.df[, -23]<-predict(preProc, train.df[, -23])
valid.norm.df<-valid.df
valid.norm.df[, -23]<-predict(preProc, valid.df[, -23])
test.norm.df<-test.df
test.norm.df[, -23]<-predict(preProc, test.df[, -23])

str(train.norm.df)

summary(train.norm.df)

library(FNN)
rmse.df<-data.frame(k=seq(1, 20, 1), RMSE = rep(0, 20))
for(i in 1:20){
  knn.t.pred<-knn.reg(train=train.norm.df[, -23], test=valid.norm.df[, -23], 
                      y=train.norm.df[ , 'bike_count'], k=i)
  rmse<-sqrt(mean((valid.norm.df$bike_count - knn.t.pred$pred)^2))
  rmse.df[i, 2]<-rmse
}
signif(rmse.df, 3)

pred<-knn.reg(train=train.norm.df[, -23], test=test.norm.df[, -23], 
              y=train.norm.df$bike_count, k=6)
res.df<-cbind(test.df, prediction=pred$pred)
head(res.df)


library(ggplot2)
ggplot(res.df, aes(x = bike_count, y = prediction)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "KNN Predictions vs Actual", x = "Actual bike count", 
       y = "Predicted bike count")

RMSE(pred$pred, test.norm.df$bike_count)

library(Metrics)
library(ModelMetrics)

actual <- test.norm.df$bike_count
predicted <- pred$pred
rmse_value <- RMSE(predicted, actual)
mae_value <- mae(actual, predicted)
sse <- sum((actual - predicted)^2)
sst <- sum((actual - mean(actual))^2)
r2_value <- 1 - sse / sst

cat("RMSE:", round(rmse_value, 4), "\n")
cat("MAE:", round(mae_value, 4), "\n")
cat("R² :", round(r2_value, 4), "\n")

