train_v1_data = read.csv(file.choose(), header = T)

transaction_v1_data = read.csv(file.choose(), header = T)

train_transaction_v1 <- merge(train_v1_data, transaction_v1_data, by="msno")

ind <-sample(2,nrow(train_transaction_v1), replace = T, prob = c(0.0001, 0.9999))
data_sample <- train_transaction_v1[ind ==1,] ## taking 1% sample data

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data_sample), replace = T, prob = c(0.7, 0.3))
train <- data_sample[ind ==1,]
test <- data_sample[ind ==2,]


#library(caret)
## Logistic Regression
lrmodel <- glm(is_churn ~., data = train, family = "binomial")
summary(lrmodel)
