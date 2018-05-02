train_v2_data = read.csv(file.choose(), header = T)

transaction_v2_data = read.csv(file.choose(), header = T)

train_transaction_v2 <- merge(train_v2_data, transaction_v2_data, by="msno")

#ind <-sample(2,nrow(train_transaction_v2), replace = T, prob = c(0.5, 0.5))
#data_sample_v2 <- train_transaction_v2[ind ==1,] ## taking 1% sample data

data_sample_v2 <- train_transaction_v2 # all data
df <- data_sample_v2

## Transfering date columns from integer to date type
library(lubridate)
df$transaction_date <- parse_date_time(as.character(data_sample_v2$transaction_date), "%Y%m%d")
df$membership_expire_date <- parse_date_time(as.character(data_sample_v2$membership_expire_date), "%Y%m%d")
data_sample_v2 <- df


# Factor variables
data_sample_v2$is_churn <- as.factor(data_sample_v2$is_churn)

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data_sample_v2), replace = T, prob = c(0.7, 0.3))
train <- data_sample_v2[ind ==1,]
test <- data_sample_v2[ind ==2,]

#library(caret)
## Logistic Regression
#lrmodel <- glm(is_churn ~ + is_auto_renew + is_cancel + payment_plan_days + 
#                 plan_list_price + transaction_date + membership_expire_date + 
#                 payment_method_id + payment_plan_days, data = train, family = "binomial")

lrmodel <- glm(is_churn ~ + payment_method_id + payment_plan_days + 
                 plan_list_price + actual_amount_paid + is_auto_renew +
                 transaction_date + membership_expire_date + is_cancel,
                data = train, family = "binomial")


#lrmodel <- glm(is_churn ~., data=train, family = "binomial")
summary(lrmodel)


# Confustion matrix for training data
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train$is_churn)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Confustion matrix for test data
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
tab4 <- table(predicted = p4, Actual = test$is_churn)
tab4
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100

