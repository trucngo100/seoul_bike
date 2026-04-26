# Predict bike_count with weather variables
#####################
# Part 0. Load Libraries
#####################
library(dplyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(randomForest)
library(gains)
library(tidyverse)
library(tidyr)

#####################
# Part 1. Load and Prepare Data
#####################
# Load cleaned bike dataset
head(bike)
str(bike)

#####################
# Part 2. Data Partition (60:40 split)
#####################
# Set random seed for reproducibility
set.seed(1)

train.index <- sample(rownames(bike), 0.6*dim(bike)[1])
valid.index <- setdiff(row.names(bike), train.index)
train.df <- bike[train.index,]
valid.df <- bike[valid.index,]
                      
#####################
# Part 3. Build Regression Tree
#####################
boxplot(bike$bike_count)$stats

# Using all other variables: #날씨 조건, #계절, #공휴일, #요일/주말, #시간대
tree.model <- rpart(bike_count ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall 
                    + season + holiday + dat_type + time_in_day,
                    data = train.df, 
                    method = "anova") # anova for regression
tree.model

## Enhanced visualization with custom colors
## Indicate palette
node_col <- function(x) {
  case_when(
    x < 500 ~ "gray",        # Gray for <500
    x >= 500 & x <= 1000 ~ "lightblue",  # Blue for 500-1000
    x > 1000 ~ "indianred1"  # Red for >1000
  )
}

prp(tree.model, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 2, 
    varlen = -10,
    box.col = node_col(tree.model$frame$yval),  # Apply our color function
    main = "Bike Count Regression Tree",
    nn = FALSE,  # Show node numbers
    branch = 0.3,  # Make branches curved
    shadow.col = "gray")  # Add shadows for depth


prp(tree.model, 
    type = 1, 
    extra = 1, 
    under = TRUE, 
    split.font = 2, 
    varlen = -10,
    box.col = node_col(tree.model$frame$yval),  # Apply our color function
    main = "Bike Count Regression Tree",
    nn = FALSE,  # Show node numbers
    branch = 0.3,  # Make branches curved
    shadow.col = "gray")  # Add shadows for depth

# Print model's result


#####################
# Part 4. Decision Tree's Model Evaluation
#####################
# Function to calculate regression metrics
regression_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  return(data.frame(RMSE = rmse))
}

# Predict on train and validation sets
train.pred <- predict(tree.model, newdata = train.df)
valid.pred <- predict(tree.model, newdata = valid.df)

# Calculate RMSE
train.rmse <- sqrt(mean((train.pred - train.df$bike_count)^2))
valid.rmse <- sqrt(mean((valid.pred - valid.df$bike_count)^2))

# Create a results data frame (single row since it's just one model)
results <- data.frame(
  Train_RMSE = train.rmse,
  Valid_RMSE = valid.rmse,
  Overfit_Gap = train.rmse - valid.rmse
)

# Print results
print(results)

 

#####################
# Part 5. Random Forest Model
#####################
set.seed(1)
# Trial with different tree values and seeing if there is any overfitting problem
# Small gap between train and valid set >> Good generalization
ntree.values <- c(100, 200, 300, 500, 1000)
results <- data.frame(
  ntree = ntree.values,
  Train_RMSE = NA,
  Valid_RMSE = NA,
  Overfit_Gap = NA
)

# Random Forest's Model Evaluation 
for (i in seq_along(ntree.values)) {
  # Train model with specific number of trees
  model <- randomForest(bike_count ~ bike_count ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall 
                        + season + holiday + dat_type + time_in_day,
                        data = train.df,
                        ntree = ntree.values[i],
                        importance = TRUE)
  
  # Predict on train and validation sets
  train.pred <- predict(model, newdata = train.df)
  valid.pred <- predict(model, newdata = valid.df)
  
  # Calculate RMSE
  train.rmse <- sqrt(mean((train.pred - train.df$bike_count)^2))
  valid.rmse <- sqrt(mean((valid.pred - valid.df$bike_count)^2))
  
  # Store results
  results[i, "Train_RMSE"] <- train.rmse
  results[i, "Valid_RMSE"] <- valid.rmse
  results[i, "Overfit_Gap"] <- train.rmse - valid.rmse
}

print(results)


# Variable importance
varImpPlot(rf.model, main = "Variable Importance")


#####################
# Part 6. Booosted Tree
#####################
# Use gradient boosting algorithm for regression task. 
# We learned adaboost in class but it is used for classification task.
library(gbm)

# Train the boosted tree model (regression)
boost <- gbm(
  bike_count ~ bike_count ~ temp + humid + wind + visibility + dew_temp + radiation + rainfall + snowfall 
  + season + holiday + dat_type + time_in_day,
  data = train.df,
  distribution = "gaussian",  # For regression (use "bernoulli" for classification)
  n.trees = 50,             # Number of boosting iterations
  interaction.depth = 3,      # Tree depth
  shrinkage = 0.1,            # Learning rate
  cv.folds = 5               # Optional: Cross-validation
)

# Graph of Variable importance
summary(boost, plotit = TRUE)  

# Calculate RMSE for train dataset
bt.train.pred <- predict(boost, train.df, n.trees = 50)
bt.train.rmse <- sqrt(mean((train.pred - train.df$bike_count)^2))

# Calculate RMSE for valid dataset
bt.valid.pred <- predict(boost, valid.df, n.trees = 50)
bt.valid.rmse <- sqrt(mean((bt.pred - valid.df$bike_count)^2))


# Create a results data frame (single row since it's just one model)
results <- data.frame(
  Train_RMSE = bt.train.rmse,
  Valid_RMSE = bt.valid.rmse,
  Overfit_Gap = bt.train.rmse - bt.valid.rmse # Check overfitting 
)

print(results)




