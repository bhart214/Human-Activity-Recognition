library(tidyverse)
library(caret)
library(randomForest)

# read in clean data
ha <- read_rds("human_activity.rds")
glimpse(ha)

# split data into training and testing sets
set.seed(928)
split <- createDataPartition(y = ha$class, p = 0.7, list = FALSE)
train <- ha[split,]
test<- ha[-split,]


### Let's try a random forest
# we set importance = TRUE so we can get the importance of the predictors
rf <- randomForest(class ~ ., data = train, mtry = 4, importance = TRUE) # sqrt(18) ~ 4
rf

# get predicted values for the test set
ha_predictions <- predict(rf, test)

# confusion matrix
confusionMatrix(ha_predictions, test$class)

# variable importance
importance(rf)
varImpPlot(rf)



# We are going to use caret so we can pick the best number of predictors to use at each split (mtry)
# using the train set we defined previously
# (takes a while to run, so be patient!)
rf_caret <- train(class ~ ., train,
                  preProc = c('center', 'scale'), # center and scale the predictors
                  method = 'rf',
                  importance = TRUE)

rf_caret
# using mtry=7 give us the lowest root MSE. 
rf_caret$bestTune
# in out final model, rm and lstat are also the most important predictors
varImpPlot(rf_caret$finalModel)