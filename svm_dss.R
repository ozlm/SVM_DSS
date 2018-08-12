#-----------------------SVM MODEL------------------
install.packages("e1071")
library("e1071") # This library is loaded for svm funtions.
set.seed(1815850)
accuracy_vals <- double() # keep accuracy values as a vector
precision_vals <- double()
recall_vals <- double()
# column names are defined for heading of the data table.
header_names = c("cclass", "persons", "lug_boot", "safety", "doors")
read_data <- read.table(file = "C:\\Users\\melis\\Desktop\\balance.txt", sep = ",", na.strings = "?", header = FALSE, col.names=header_names, stringsAsFactors = TRUE)
summary(read_data[!complete.cases(read_data), ])
input_data <- read_data[ !is.na (read_data$LW) & !is.na (read_data$LD) & !is.na (read_data$RW) & !is.na (read_data$RD),]
attach(input_data) # attached data
x <- subset(input_data, select=-cclass) # cclass is the target feature and retrieved from the dataset.
y <- cclass
svm_model <- svm(cclass ~ ., data=input_data) # svm model is working here.
summary(svm_model)
pred <- predict(svm_model, x) # prediction table is generated.
table(pred,y)
svm_tune <- tune(svm, cclass~ ., data=input_data, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
plot(svm_tune)
svm_model_after_tune <- svm(cclass~ ., data=input_data, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune) # svm tuning is applied here and summarized.
pred <- predict(svm_model_after_tune,x)
table(pred,y)
test_predict <- table(pred,y)
acc <- (sum(diag(test_predict)) / sum(test_predict)) # accuracy is calculated.
precision <- ((test_predict[1,1]/(test_predict[1,1]+test_predict[2,1]+test_predict[3,1]))+(test_predict[2,2]/(test_predict[1,2]+test_predict[2,2]+test_predict[3,2]))+(test_predict[3,3]/(test_predict[1,3]+test_predict[2,3]+test_predict[3,3]))) / 3
recall <- ((test_predict[1,1]/(test_predict[1,1]+test_predict[1,2]+test_predict[1,3]))+(test_predict[2,2]/(test_predict[2,2]+test_predict[2,1]+test_predict[2,3]))+(test_predict[3,3]/(test_predict[3,2]+test_predict[3,1]+test_predict[3,3]))) / 3
# svm is worked for linear kernel type and results are shown on the console.
svm_model_after_tune_2 <- svm(cclass ~ ., data=input_data, kernel="linear", cost=1, gamma=0.5)
summary(svm_model_after_tune_2)
pred_2 <- predict(svm_model_after_tune_2, x)
table(pred_2, y)
test_predict_2 <- table(pred_2,y)
accuracy_2 <- (sum(diag(test_predict_2)) / sum(test_predict_2))
precision_2 <- ((test_predict_2[1,1]/(test_predict_2[1,1]+test_predict_2[2,1]+test_predict_2[3,1]))+(test_predict_2[2,2]/(test_predict_2[1,2]+test_predict_2[2,2]+test_predict_2[3,2]))+(test_predict_2[3,3]/(test_predict_2[1,3]+test_predict_2[2,3]+test_predict_2[3,3]))) / 3
recall_2 <- ((test_predict_2[1,1]/(test_predict_2[1,1]+test_predict_2[1,2]+test_predict_2[1,3]))+(test_predict_2[2,2]/(test_predict_2[2,2]+test_predict_2[2,1]+test_predict_2[2,3]))+(test_predict_2[3,3]/(test_predict_2[3,2]+test_predict_2[3,1]+test_predict_2[3,3]))) / 3
for(index in 1:100) # performed for 100 iterations.(target feature is 'cclass')
{
sub_part <- sample(1:nrow(input_data), size=nrow(input_data)*0.7) # %70 of the samples #is used for training and rest of them for testing.
train <- input_data[sub_part,]
test <- input_data[-sub_part,]
model <- svm(cclass ~ ., data=train)
test_predict <- table(predict(model, input_data[-sub_part, ], type = "class"), input_data[-sub_part, "cclass"])
if(as.double(sum(diag(test_predict)) / sum(test_predict)) > max(accuracy_vals))
maxAccModel <- test_predict # to find max accuracy rate of the model.
accuracy_vals <- c(accuracy_vals, sum(diag(test_predict)) / sum(test_predict)) # accuracy is kept in the vector.
if(as.double(((test_predict[1,1]/(test_predict[1,1]+test_predict[2,1]+test_predict[3,1]))+(test_predict[2,2]/(test_predict[1,2]+test_predict[2,2]+test_predict[3,2]))+(test_predict[3,3]/(test_predict[1,3]+test_predict[2,3]+test_predict[3,3]))) / 3) > max(precision_vals))
maxPrecModel <- test_predict # to find max precision rate of the model.
precision_vals <- c(precision_vals, ((test_predict[1,1]/(test_predict[1,1]+test_predict[2,1]+test_predict[3,1]))+(test_predict[2,2]/(test_predict[1,2]+test_predict[2,2]+test_predict[3,2]))+(test_predict[3,3]/(test_predict[1,3]+test_predict[2,3]+test_predict[3,3]))) / 3) # #accuracy is kept in the vector
if((test_predict[1,1]+test_predict[1,2]+test_predict[1,3])==0)
a <- 1
else
a <- (test_predict[1,1]+test_predict[1,2]+test_predict[1,3])
if((test_predict[2,2]+test_predict[2,1]+test_predict[2,3])==0)
b <- 1
else
b <- (test_predict[2,2]+test_predict[2,1]+test_predict[2,3])
if((test_predict[3,2]+test_predict[3,1]+test_predict[3,3]) == 0)
c <- 1
else
c <- (test_predict[3,2]+test_predict[3,1]+test_predict[3,3])
if(!is.na(as.double(((test_predict[1,1]/a)+(test_predict[2,2]/b)+(test_predict[3,3]/c)) / 3) > max(recall_vals)))
maxRecallModel <- test_predict # to find max recall rate of the model.
recall_vals <- c(recall_vals, ((test_predict[1,1]/a)+(test_predict[2,2]/b)+(test_predict[3,3]/c)) / 3) # #accuracy is kept in the vector
}
# calculated average values of the calculated values
mean(accuracy_vals)
plot(accuracy_vals, type="l", ylab="Accuracy", xlab="Iterations", main="SVM Accuracy Rates for Balance dataset With Different Subsets of Data over 100 iterations", col = "blue")
plot(1-accuracy_vals, type="l", ylab="Error Rate", xlab="Iterations", main="SVM Error Rate for Balance With Different Subsets of Data")
mean(precision_vals)
plot(precision_vals, type="l", ylab="Precision", xlab="Iterations", main="SVM Precision Rates for Balance dataset With Different Subsets of Data over 100 iterations", col = "red")
mean(recall_vals)
plot(recall_vals, type="l", ylab="Recall", xlab="Iterations", main="SVM Recall Rates for Balance dataset With Different Subsets of Data over 100 iterations", col = "green")



#-----------------------DECISION TREE MODEL------------------

library(rpart) # Used rpart() library
set.seed(1815850)
header_names = c("cclass", "LW", "LD", "RW", "RD")
read_data <- read.table(file = "C:/Users/Ozlemm/Documents/balance-scale.txt", sep = ",", na.strings = "?", header = FALSE, col.names=header_names, stringsAsFactors = TRUE)
summary  (read_data[!complete.cases(read_data), ])
input_data <- read_data[!is.na (read_data$LW) & !is.na (read_data$LD) & !is.na (read_data$RW) & !is.na (read_data$RD), ]
attach(input_data)
x <- subset(input_data, select=-cclass)
y <- cclass
accuracy_vals <- double()  # keep accuracy values as a vector
precision_vals <- double()
recall_vals <- double()
prunned_acc <- double()
prunned_precision <- double()
prunned_recall <- double()
decision_tree <- rpart(cclass ~ .,data = input_data, method = "class")
plot(decision_tree, uniform=TRUE, compress=FALSE, main="Classification Tree for Balance Scale data set", margin=0.002)
text(decision_tree, use.n=TRUE, all=TRUE, cex=0.75, fancy=FALSE)
for(index in 1:100)  # performed for 100 iterations.(target feature is 'cclass')
{
  sub_part <- sample(1:nrow(input_data), size=nrow(input_data)*0.7) # %70 of the samples #is used for training and rest of them for testing.
  fit <- rpart(cclass ~ ., data = input_data, subset = sub_part, method = "class")
  test_predict <- table(predict(fit, input_data[-sub_part, ], type = "class"), input_data[-sub_part, "cclass"])
  if(as.double(sum(diag(test_predict)) / sum(test_predict)) > max(accuracy_vals))
    maxAccTree <- fit # to find max accuracy rate of the tree.
  accuracy_vals <- c(accuracy_vals, sum(diag(test_predict)) / sum(test_predict)) # #accuracy is kept in the vector
  if(as.double(((test_predict[1,1]/(test_predict[1,1]+test_predict[2,1]+test_predict[3,1]))
                +(test_predict[2,2]/(test_predict[1,2]+test_predict[2,2]+test_predict[3,2]))
                +(test_predict[3,3]/(test_predict[1,3]+test_predict[2,3]+test_predict[3,3]))) / 3) > max(precision_vals))
    maxPrecTree <- fit # to find max precision rate of the model.
  precision_vals <- c(precision_vals, ((test_predict[1,1]/(test_predict[1,1]+test_predict[2,1]+test_predict[3,1]))  +(test_predict[2,2]/(test_predict[1,2]+test_predict[2,2]+test_predict[3,2]))
                                       +(test_predict[3,3]/(test_predict[1,3]+test_predict[2,3]+test_predict[3,3]))) / 3) # #precison is kept in the vector
  if((test_predict[1,1]+test_predict[1,2]+test_predict[1,3])==0)
    a <- 1
  else
    a <- (test_predict[1,1]+test_predict[1,2]+test_predict[1,3])
  
  if((test_predict[2,2]+test_predict[2,1]+test_predict[2,3])==0)
    b <- 1
  else
    b <- (test_predict[2,2]+test_predict[2,1]+test_predict[2,3])
  
  if((test_predict[3,2]+test_predict[3,1]+test_predict[3,3]) == 0)
    c <- 1
  else
    c <- (test_predict[3,2]+test_predict[3,1]+test_predict[3,3])
  if(!is.na(as.double(((test_predict[1,1]/a)+(test_predict[2,2]/b)+(test_predict[3,3]/c)) / 3) > max(recall_vals)))
    maxRecallTree <- fit # to find max recall rate of the tree
  recall_vals <- c(recall_vals, ((test_predict[1,1]/a)+(test_predict[2,2]/b)+(test_predict[3,3]/c)) / 3) # #accuracy is kept in the vector
  bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
  tree.pruned <- prune(fit, cp = bestcp)  
  test_pruned_predict <- table(predict(tree.pruned, input_data[-sub_part, ], type = "vector"), input_data[-sub_part, "cclass"])
  prunned_acc <- c(prunned_acc, sum(diag(test_pruned_predict)) / sum(test_pruned_predict))
    # 
  # prunned_precision <- c(prunned_precision, ((test_pruned_predict[1,1]/(test_pruned_predict[1,1]+test_pruned_predict[2,1]+test_pruned_predict[3,1]))+(test_pruned_predict[2,2]/(test_pruned_predict[1,2]+test_pruned_predict[2,2]+test_pruned_predict[3,2]))+(test_pruned_predict[3,3]/(test_pruned_predict[1,3]+test_pruned_predict[2,3]+test_pruned_predict[3,3]))) / 3)
  # 
  # prunned_recall <- c(prunned_recall, ((test_pruned_predict[1,1]/(test_pruned_predict[1,1]+test_pruned_predict[1,2]+test_pruned_predict[1,3]))
  #                                      +(test_pruned_predict[2,2]/(test_pruned_predict[2,2]+test_pruned_predict[2,1]+test_pruned_predict[2,3]))
  #                                      +(test_pruned_predict[3,3]/(test_pruned_predict[3,2]+test_pruned_predict[3,1]+test_pruned_predict[3,3]))) / 3)  
}
mean(accuracy_vals)  # calculated average accuracy
# plotting accuracy changes over 100 iterations
plot(accuracy_vals, type="l", ylab="Accuracy", xlab="Iterations", main="DT Accuracy Rates for Balance Scale dataset With Different Subsets of Data over 100 iterations", col = "blue")
plot(1-accuracy_vals, type="l", ylab="Error Rate", xlab="Iterations", main="DT Error Rate for Balance Scale With Different Subsets of Data")

# plotted tree which has max accuracy over 100 iterations
plot(maxAccTree, uniform=TRUE, compress=FALSE, main="Max Accurate Decision Tree for Balance Scale data set", margin=0.08)
text(maxAccTree, use.n=TRUE, all=TRUE, cex=0.75, fancy=FALSE)

#plot prunned tree statistics
mean(prunned_acc)
plot(prunned_acc, type="l", ylab="AccuracyPruned(Balance Scale)", xlab="Iterations", main="DT Accuracy Rate for Balance Scale(prunned) With Different Subsets of Data")

# mean(prunned_precision)
# plot(prunned_precision, type="l", ylab="PrecisionPruned(Balance Scale)", xlab="Iterations", main="DT Precision Rate for Balance Scale (prunned) With Different Subsets of Data")
# 
# mean(prunned_recall)
# plot(prunned_recall, type="l", ylab="RecallPruned(Balance Scale)", xlab="Iterations", main="DT Recall Rate for Balance Scale (prunned) With Different Subsets of Data")

mean(precision_vals)  # calculated average accuracy
# plotting precision changes over 100 iterations
plot(precision_vals, type="l", ylab="Precision", xlab="Iterations", main="DT Precision Rates for Balance Scale dataset With Different Subsets of Data over 100 iterations", col = "blue")

# plotted tree which has max precision over 100 iterations
plot(maxPrecTree, uniform=TRUE, compress=FALSE, main="DT Precision Tree for Balance Scale data set", margin=0.08)
text(maxPrecTree, use.n=TRUE, all=TRUE, cex=0.75, fancy=FALSE)

mean(recall_vals)  # calculated average recall
# plotting recall changes over 100 iterations
plot(recall_vals, type="l", ylab="Recall", xlab="Iterations", main="DT Recall Rates for Balance Scale dataset With Different Subsets of Data over 100 iterations", col = "blue")

# plotted tree which has max recall over 100 iterations
plot(maxRecallTree, uniform=TRUE, compress=FALSE, main="DT Recall Tree for Balance Scale data set", margin=0.08)
text(maxRecallTree, use.n=TRUE, all=TRUE, cex=0.75, fancy=FALSE) 
