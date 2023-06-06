library(C50)
library(gmodels)
credit <- read.csv("C:/Users/hasna/Downloads/credit risk.csv")
str(credit)
summary(credit)
table(credit$checking_balance)
table(credit$savings_balance)
credit$default <- as.factor(credit$default)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)
#spliting the data into train and test set
set.seed(123)
train_sample <- sample(1000,700)
range(train_sample)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
head(credit_train)
head(credit_test)
prop.table(table(credit_train$default))
prop.table(table(credit_train$default))*100
prop.table(table(credit_test$default))
prop.table(table(credit_test$default))
prop.table(table(credit_test$default))*100
credit_train$default <- as.factor(credit_train$default)
#training a model
model <- C5.0(credit_train[-17], credit_train$default)
model
summary(model)
predict <- predict(model, credit_test)
head(predict)
accuracy <- CrossTable(credit_test$default, predict,
                          prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                          dnn = c('actual default', 'predict default'))
accuracy <- table(credit_test$default,predict)
accuracy
accuracy_result <- sum(diag(accuracy))/sum(accuracy)
accuracy_result
# boosting the accuracy of model
credit_boost <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost
summary(credit_boost)
credit_boost_pred <- predict(credit_boost,credit_test)
CrossTable(credit_test$default, credit_boost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))
matrix_dimensions <- list(c('yes','no'),c('yes','no'))
names(matrix_dimensions) <- c('actual','predict')
matrix_dimensions
error_cost <- matrix(c(0,1,4,0),nrow = 2, dimnames=matrix_dimensions)
error_cost
credit_cost <- C5.0(credit_train[-17],credit_train$default,costs=error_cost)
credit_cost_pred <- predict(credit_cost,credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))
