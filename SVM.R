tain_v2 <- read.csv(file.choose(),header = T)
transaction_v2 <- read.csv(file.choose(),header = T)
user_logs <- read.csv(file.choose(),header = T)
merged_v2_data<- merge(tain_v2, user_logs, by="msno")
100str(merged_v2_data)
#factor variables
#merged_v2_data$is_churn <- as.factor(merged_v2_data$is_churn)

#df <- data.frame(c(table(merged_v2_data$payment_plan_days)))

#partition data - train (80%), test(20%)
set.seed(511)
ind <- sample(2, nrow(merged_v2_data), replace = T, prob = c(0.1, 0.9))
#ind
data_sample <- merged_v2_data[ind == 1,]
data_sample <-merged_v2_data


#dividing the sample further
ind2 <- sample(2, nrow(data_sample), replace = T, prob = c(0.70, 0.30))
train <- data_sample[ind2==1,]
test <- data_sample[ind2==2,]



#Logistic Regression
lrmodel <- glm(is_churn ~ + payment_method_id + payment_plan_days + 
                 plan_list_price + actual_amount_paid + is_auto_renew +
                 transaction_date + membership_expire_date + is_cancel,
               data = train, family = "binomial")
summary(lrmodel)

##confustion matrix for training data
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
(tab3 <- table(predicted = p3, Actual = train$is_churn))
1 - sum(diag(tab3))/sum(tab3)

sum(diag(tab3))/sum(tab3)


##confustion matrix for test data
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
(tab4 <- table(predicted = p4, Actual = test$is_churn))
1 - sum(diag(tab4))/sum(tab4)
sum(diag(tab4))/sum(tab4)



data <- read.csv(file.choose(),header = T)
head(data)
data$NSP <- as.factor(data$NSP)
summary(data)

#data partition (70:30)
set.seed(511)
ind <- sample(2,nrow(data),replace=T,prob = c(0.7,0.3))
train <- data[ind ==1,]
test <- data[ind==2,]




##tree model
#tree model
#factor variables
#data$NSP <- as.factor(data$NSP)

#partition data - train (70%), test(30%)
set.seed(511)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
#ind
train <- data[ind == 1,]
test <- data[ind==2,]


#tree model
#install.packages("party")
#library(party)
tree <- tree(is_churn ~ + payment_method_id + payment_plan_days + 
                plan_list_price + actual_amount_paid +
                transaction_date + membership_expire_date + is_auto_renew  + is_cancel, train)
plot(tree, type = "simple")
plot(tree)
#prediction
p1 <- predict(tree, train, type = "class")

#confusion matrix - training data
(tab1 <- table(predicted = p1, Actual = train$is_churn))
#Miss classification rate
1 - sum(diag(tab1))/sum(tab1)
sum(diag(tab1))/sum(tab1)

#confusion matrix - test

p2 <- predict(tree, test, type = 'class')
#confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$is_churn))
1-sum(diag(tab2))/sum(tab2)
sum(diag(tab2))/sum(tab2)


###


## cross-validation to check where to stop pruning
cv_tree = cv.tree(tree, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size,
     cv_tree$dev,
     type = 'b')
## Pruning the tree
pruned_model = prune.misclass(tree, best = 7)
plot(pruned_model)
text(pruned_model, pretty = 0)

# Prediction
# Confusion matrix - test
p2 <- predict(pruned_model, test, type = 'class')
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$is_churn))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2)) * 100
#Accuracy
(sum(diag(tab2))/sum(tab2)) * 100



#neural network model
install.packages("neuralnet")
#library(neuralnet)
nmodel <- neuralnet(is_churn ~  + payment_method_id + payment_plan_days + 
                      plan_list_price + actual_amount_paid + is_auto_renew +
                      transaction_date + membership_expire_date + is_cancel ,data = train, hidden = 3)
plot(nmodel)


p <- nmodel$net.result[[1]]
head(p)
head(train)


#prediction train
pred <- compute(nmodel,train[,-1])

#confusion matrix - train data

p5 <- pred$net.result
p5 <- ifelse(p5>0.5,1,0)
(tab5 <- table(predicted=p5, Actual = train$admit))



#prediction test
pred <- compute(nmodel,test[,-1])

#confusion matrix - train data

p6 <- pred$net.result
p6 <- ifelse(p6>0.5,1,0)
(tab6 <- table(predicted=p6, Actual = test$admit))



# SVM
library(e1071)
model_svm <- svm(is_churn ~ + payment_method_id + payment_plan_days + 
                   plan_list_price + actual_amount_paid + is_auto_renew +
                   transaction_date + membership_expire_date + is_cancel , train)
summary(model_svm)

# Prediction
p1 <- predict(model_svm, train)
# Confusion matrix - training data
tab1 <- table(predicted = p1, Actual = train$is_churn)
tab1
# Miss classification error 
(1 - sum(diag(tab1))/sum(tab1))
# Accuracy
(sum(diag(tab1))/sum(tab1))

# Confusion matrix - test
p2 <- predict(model_svm, test)
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$is_churn))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2))
#Accuracy
(sum(diag(tab2))/sum(tab2))

