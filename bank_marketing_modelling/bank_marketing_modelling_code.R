library(tidyr)
library(caret)

setwd("C:/Users/vince/Desktop/task/1Old Tasks/ERG2050/bank-additional")
data <-read.csv("bank-additional-full.csv")

data <- separate(data, col = "age.job.marital.education.default.housing.loan.contact.month.day_of_week.duration.campaign.pdays.previous.poutcome.emp.var.rate.cons.price.idx.cons.conf.idx.euribor3m.nr.employed.y",
                 into =c("age","job","marital","education","default","housing","loan","contact","month","day_of_week","duration","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y"),
                 sep = ";")

data <- data[-grep("unknown", data$education), ]
data <- data[-grep("unknown", data$job), ]
data <- data[-grep("unknown", data$housing), ]
data <- data[-grep("unknown", data$marital), ]

drop <- c("default","loan","duration","pdays","poutcome")
data <- data[ ,!(names(data) %in% drop)]

data["education"][data["education"] == "basic.6y"] <- 6
data["education"][data["education"] == "basic.4y"] <- 4
data["education"][data["education"] == "basic.9y"] <- 9
data["education"][data["education"] == "university.degree"] <- 16
data["education"][data["education"] == "high.school"] <- 12
data["education"][data["education"] == "professional.course"] <- 17

data["job"][data["job"] == "unemployed"] <- 1
data["job"][data["job"] == "student"] <- 2
data["job"][data["job"] == "housemaid"] <- 3
data["job"][data["job"] == "services"] <- 4
data["job"][data["job"] == "blue-collar"] <- 5
data["job"][data["job"] == "self-employed"] <- 6
data["job"][data["job"] == "technician"] <- 7
data["job"][data["job"] == "admin."] <- 8
data["job"][data["job"] == "management"] <- 9
data["job"][data["job"] == "entrepreneur"] <- 10
data["job"][data["job"] == "retired"] <- 11

data["marital"][data["marital"] == "married"] <- 1
data["marital"][data["marital"] == "single"] <- -1
data["marital"][data["marital"] == "divorced"] <- 0

data["month"][data["month"] == "jan"] <- 1
data["month"][data["month"] == "feb"] <- 2
data["month"][data["month"] == "mar"] <- 3
data["month"][data["month"] == "apr"] <- 4
data["month"][data["month"] == "may"] <- 5
data["month"][data["month"] == "jun"] <- 6
data["month"][data["month"] == "jul"] <- 7
data["month"][data["month"] == "aug"] <- 8
data["month"][data["month"] == "sep"] <- 9
data["month"][data["month"] == "oct"] <- 10
data["month"][data["month"] == "nov"] <- 11
data["month"][data["month"] == "dec"] <- 12

data["day_of_week"][data["day_of_week"] == "mon"] <- 1
data["day_of_week"][data["day_of_week"] == "tue"] <- 2
data["day_of_week"][data["day_of_week"] == "wed"] <- 3
data["day_of_week"][data["day_of_week"] == "thu"] <- 4
data["day_of_week"][data["day_of_week"] == "fri"] <- 5

data["contact"][data["contact"] == "cellular"] <- 0
data["contact"][data["contact"] == "telephone"] <- 1

data["housing"][data["housing"] == "no"] <- 0
data["housing"][data["housing"] == "yes"] <- 1

data["y"][data["y"] == "no"] <- 0
data["y"][data["y"] == "yes"] <- 1

data$contact <- as.numeric(data$contact)
data$nr.employed <- as.numeric(data$nr.employed)
data$campaign <- as.numeric(data$campaign)
data$job <- as.numeric(data$job)
data$education <- as.numeric(data$education)

# Highly imbalanced data
yes <- which(data["y"] == 1)
no <- which(data["y"] == 0)
length(yes)
length(no)

# A combination of down-sampling and up-sampling
yes_upsample <- sample(yes, length(no)/2, replace = TRUE)
no_downsample <- sample(no, length(yes_upsample))
dataset <- data[c(yes_upsample, no_downsample),]

dataset$housing <- as.numeric(dataset$housing)
dataset$age <- as.numeric(dataset$age)
dataset$campaign <- as.numeric(dataset$campaign)
dataset$previous <- as.numeric(dataset$previous)
dataset$emp.var.rate <- as.numeric(dataset$emp.var.rate)
dataset$cons.price.idx <- as.numeric(dataset$cons.price.idx)
dataset$cons.conf.idx <- as.numeric(dataset$cons.conf.idx)
dataset$euribor3m <- as.numeric(dataset$euribor3m)
dataset$y <- as.factor(dataset$y)

length(which(dataset["y"] == 1))
length(which(dataset["y"] == 0))

set.seed(1)
data_index <- createDataPartition(dataset$y, p = 0.8, list = FALSE)
train_data <- dataset[data_index, ]
test_data <- dataset[-data_index, ]

train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

train_y <- train_data[, ncol(train_data)]
test_y <- test_data[, ncol(test_data)]

train_attributes <- train_data[, 1:(ncol(train_data)-1)]
test_attributes <- test_data[, 1:(ncol(test_data)-1)]

#TEST KNN
library(class)
set.seed(1)
x <- 1:20
acc_knn <- rep(0,20)
for (i in x){
  knn_pred <- knn(train_attributes, train_attributes, train_y, k = i)
  conf_matrix <- table(knn_pred, train_y)
  acc_knn[i] <- (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix)
}
plot <- ggplot() + geom_line(aes(x,acc_knn)) + scale_x_continuous(breaks =seq(0,50,by=1))
plot

#best knn
set.seed(1)
best_knn <- knn(train_data, test_data, train_y, k = 4)
table <- table(best_knn, test_y)
(accuracy_knn <- sum(diag(table))/sum(table))
print("precision")
(prec <- table[2,2]/sum(table[2,1],table[2,2]))
print("recall")
(recall <- table[2,2]/sum(table[1,2],table[2,2]))
print("F1")
(2*((prec*recall)/(prec+recall)))

#Logistic regression
library(boot)
log_model <- glm(y ~ ., data = train_data, family = binomial)
probabilities <- predict(log_model, test_data, type = "response")
predict <- rep(1, length(probabilities))
predict[probabilities < 0.5] <- 0
mean(predict == test_y)
table(test_y, predict)

#Log Regression with only *\beta0\
zero_model <- glm(y ~ 1, data = train_data, family = binomial)
summary(zero_model)

#Forward Log-regression
forward_model <- step(zero_model,
                      scope = list(lower = formula(zero_model), upper = formula(log_model)), direction="forward")
probabilities <- predict(forward_model, test_data, type = "response")
predict <- rep(1, length(probabilities))
predict[probabilities < 0.5] <- 0
mean(predict == test_y)
table(test_y, predict)

#Backward log-regression
backward_model <- step(log_model)
probabilities <- predict(backward_model, test_data, type = "response")
predict <- rep(1, length(probabilities))
predict[probabilities < 0.5] <- 0
mean(predict == test_y)
table(test_y, predict)

#Stepwise Log-regression
stepwise_model <- step(log_model, scope = list(upper = formula(zero_model), lower = formula(log_model)), direction = "both")
summary(stepwise_model)
probabilities <- predict(stepwise_model, test_data, type = "response")
predict <- rep(1, length(probabilities))
predict[probabilities < 0.5] <- 0
mean(predict == test_y)
table(test_y, predict)


#LDA
set.seed(1)
library(MASS)
library(dplyr)
lda_model <- lda(y ~., data = train_data)
lda_pred <- predict(lda_model, test_data)
lda_matrix <- table(lda_pred$class, test_y)
lda_matrix
(lda_matrix[1,1] + lda_matrix[2,2])/sum(lda_matrix)

#Naive Bayes
library(e1071)
naive_model <- naiveBayes(y ~ ., data = train_data)
naive_predict <- predict(naive_model, newdata = test_data)
mean(naive_predict == test_y)


#Tree
library(rpart)
library(rpart.plot)
set.seed(1)
tree_model <- rpart(formula = y ~., data = train_data, method = "class",
                    control = rpart.control(cp = 0))
rpart.plot(tree_model)
predict_tree_beforePrune <- rpart.predict(tree_model, test_data, type = "class")
table_mat_beforePrune <- table(test_data$y, predict_tree_beforePrune)
accuracy_Test_beforePrune <- sum(diag(table_mat_beforePrune)) / sum(table_mat_beforePrune)
cat("Accuracy before prune: ", accuracy_Test_beforePrune)

#Pruned 1
set.seed(1)
pruned_tree <- prune(tree_model, cp = tree_model$cptable
                     [which.min(tree_model$cptable[,"xerror"]),"CP"])
rpart.plot(pruned_tree)

predict_tree <- rpart.predict(pruned_tree, test_data, type = "class")
table_mat<- table(test_data$y, predict_tree)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
cat("Accuracy after prune: ", accuracy_Test)

#svm
library(e1071)
set.seed(1)
train_data <- train_data %>% mutate(y = as.factor(y))
test_data <- test_data %>% mutate(y = as.factor(y))

svm_model <- svm(y~., data = train_data, cost = 1,
                 kernel = "linear", scale= TRUE)
summary(svm_model)

pred <- predict(svm_model, test_data)
(table <- table(test_data$y, pred))
(table[1,1]+table[2,2])/sum(table)


svm_tune <- tune(svm, y~., data = train_data, kernel = "linear",
                 ranges = list(cost = 10^seq(-2,1, by=0.25)), scale= TRUE)
summary(svm_tune)

svm_after_tuned <- svm(y~., data = train_data, kernel = "linear", 
                       cost = svm_tune$best.parameters$cost)
pred <- predict(svm_after_tuned, test_data)
(table <- table(test_data$y, pred))
(table[1,1]+table[2,2])/sum(table)


svm_model_radial <- svm(y~., data = train_data, cost = 1,
                        kernel = "radial", scale= TRUE)
summary(svm_model_radial)
pred <- predict(svm_model_radial, test_data)
(table <- table(test_data$y, pred))
(table[1,1]+table[2,2])/sum(table)

svm_tune_radial <- tune(svm, y~., data = train_data, kernel = "radial",
                        ranges = list(cost = 10^seq(-2,1, by=0.25)), scale= TRUE,
                        gamma = c(1,2,3,4,5))
summary(svm_tune_radial)

svm_after_tuned_radial <- svm(y~., data = train_data, kernel = "radial", 
                              cost = svm_tune_radial$best.parameters$cost,
                              gamma = svm_tune_radial$best.parameters$gamma)
pred_radial<- predict(svm_after_tuned, test_data)
radial_matrix <- table(test_data$y, pred)
(radial_matrix[1,1]+radial_matrix[2,2])/sum(radial_matrix)

svm_model_polynomial <- svm(y~., data = train_data, cost = 1,
                            kernel = "polynomial", scale= TRUE)
summary(svm_model)

svm_tune_polynomial <- tune(svm, y~., data = train_data, kernel = "polynomial",
                 ranges = list(cost = 10^seq(-2,1, by=0.25)), scale= TRUE)
summary(svm_tune)

svm_after_tuned_polynomial <- svm(y~., data = train_data, kernel = "polynomial", 
                       cost = svm_tune$best.parameters$cost)
pred <- predict(svm_after_tuned, test_data)
(poly_matrix <- table(test_data$y, pred))
(poly_matrix[1,1]+poly_matrix[2,2])/sum(poly_matrix)