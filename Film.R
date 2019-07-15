
library(dummies)
library(dummy)
library(varhandle)

film_df <- read.csv('Film-StatsCalc.csv')


binary_duration <- to.dummy(film_df$Time_code, "duration")

dataset_to_model <- cbind(film_df, binary_duration)
dataset_to_model <- subset(dataset_to_model, select = c(Time, 
                                                    Description, 
                                                    Origin, 
                                                    duration.long, 
                                                    Good))


#shuffle data
dataset_to_model <- dataset_to_model[sample(nrow(dataset_to_model)), ]

#prepare training and test data set
library(caTools)
set.seed(75)
split <- sample.split(dataset_to_model$Good, SplitRatio = 0.8)
training_set <- subset(dataset_to_model, split == TRUE)
test_set <- subset(dataset_to_model, split == FALSE)


# Fit the model

fit <- glm(Good ~ ., data = training_set)
summary(fit)

# Test the model

prob_predit <- predict(fit, type='response', newdata = subset(test_set, select = -c(Good)))
y_pred <- ifelse(prob_predit > 0.5, 1, 0)

prob_predit_df <- as.data.frame(prob_predit)

cm = table(test_set[ ,c('Good')], y_pred)

result <- cbind(test_set, prob_predit_df, y_pred)
