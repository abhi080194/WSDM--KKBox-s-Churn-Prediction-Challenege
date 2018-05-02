data <- df

# Factor variables
data$is_churn <- as.factor(data$is_churn)

# Partition data
set.seed(511)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random forest model
library(randomForest)
system.time(rf <- randomForest(is_churn ~ + payment_method_id + payment_plan_days + 
                                 plan_list_price + actual_amount_paid + is_auto_renew +
                                 transaction_date + membership_expire_date + is_cancel, train,
                                  importance = T,
                                  ntree = 100,
                                  mtry = 8))
rf

# confusion matrix - train data
#install.packages("caret")
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$is_churn)

# confusion matrix - test
p2 <- predict(rf, test)
confusionMatrix(p2, test$is_churn)

# Variable importance
varImpPlot(rf)
plot(rf)

# Tune 
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = T,
            ntreeTry = 100,
            trace = T,
            improve = 0.05)

hist(treesize(rf))
