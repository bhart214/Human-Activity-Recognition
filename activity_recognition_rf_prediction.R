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


