bike <- read.csv("bike_cleaned.csv")
table(bike$season)
table(bike$time_in_day)

# 데이터 변수들 확인
str(bike)
dim(bike)
colnames(bike)
#  [1] "date"        "bike_count"  "hour"        "temp"        "humid"       "wind"        "visibility"  "dew_temp"   
# [9] "radiation"   "rainfall"    "snowfall"    "season"      "holiday"     "dat_type"    "time_in_day"


###########################
## Regression model 1 : 단순한 회귀분석 모델
###########################
model0 <- lm(bike_count ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall + season 
             + holiday + dat_type + time_in_day, data = bike)
summary(model0)


###########################
## Regression model 2: 조절 효과과
###########################
model_moderated <- lm(bike_count ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall + 
               season + holiday + dat_type*time_in_day, data = bike)
summary(model_moderated)



###########################
## Regression model 3: 이항로짓모형
###########################
# demand_binary 변수 생성: mean보다 높으면 1, mean보다 작은면 0
hist(bike$bike_count)
mean(bike$bike_count)
bike$demand_binary <- ifelse(bike$bike_count > 700, 1, 0)

model_logit <- glm(demand_binary ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall + season 
                   + holiday + dat_type + time_in_day,
                   data = bike, 
                   family = binomial)
summary(model_logit)



###########################
## Regression model 4: 다항로짓모형
###########################
summary(bike$bike_count)

# 통계 결과를 바탕으로 bin 만들기 
# load necessary packages
install.packages("mlogit")
library(mlogit)

# Create a target variable with various bins
breaks <- c(0, 214, 542, 729.2, 1084, Inf)
labels <- c("Very Low", "Low", "Medium", "High", "Very High")

bike$demand_cat <- cut(bike$bike_count, 
                       breaks = breaks, 
                       labels = labels, 
                       include.lowest = TRUE, 
                       right = TRUE)

# Data preprocessing step 1: Mean-center continuous variables
bike$temp0       <- bike$temp       - mean(bike$temp, na.rm = TRUE)
bike$humid0      <- bike$humid      - mean(bike$humid, na.rm = TRUE)
bike$wind0       <- bike$wind       - mean(bike$wind, na.rm = TRUE)
bike$visibility0 <- bike$visibility - mean(bike$visibility, na.rm = TRUE)
bike$dew_temp0   <- bike$dew_temp   - mean(bike$dew_temp, na.rm = TRUE)
bike$radiation0  <- bike$radiation  - mean(bike$radiation, na.rm = TRUE)
bike$rainfall0   <- bike$rainfall   - mean(bike$rainfall, na.rm = TRUE)
bike$snowfall0   <- bike$snowfall   - mean(bike$snowfall, na.rm = TRUE)

# Data preprocessing step 2
bike$id <- 1:nrow(bike)

# Reshape to mlogit-compatible long format before building the model
bike_long <- mlogit.data(data = bike,
                         choice = "demand_cat",
                         shape = "wide",
                         id.var = "id")

# Build multinomial logistic regression model
model_mlogit <- mlogit(demand_cat ~ 1 | season + holiday + dat_type + 
                         temp0 + humid0 + wind0 + visibility0 + dew_temp0 +
                         radiation0 + rainfall0 + snowfall0,
                       data = bike_long)
summary(model_mlogit)
