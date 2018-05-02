library(ROCR)

##################### ROC curve for logistic regression ######################

prob <- predict(lrmodel, newdata=test, type="response")
pred <- prediction(prob, test$is_churn)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc <- performance(perf, measure = "auc")
auc <- auc@y.values[[1]]
auc



############# ROC Curve for decision trees ###############3

prob1 <- predict(tree_model, newdata=test, type="prob")
pred1 <- prediction(prob1, test$is_churn)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
plot(perf1, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
