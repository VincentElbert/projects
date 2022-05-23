library(readr)

x <-read_csv("C:/Users/vince/Desktop/task/ERG2050/bank-additional/bank-additional-full.csv")

library(tidyr)
data <- separate(x, col = "age;job;marital;education;default;housing;loan;contact;month;day_of_week;duration;campaign;pdays;previous;poutcome;emp.var.rate;cons.price.idx;cons.conf.idx;euribor3m;nr.employed;y",
                 into =c("age","job","marital","education","default","housing","loan","contact","month","day_of_week","duration","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y"),
                 sep = ";")


s2<-lapply(data, gsub, pattern="\\\\", replacement = "")
s3 <- as.data.frame(s2)

s4<-lapply(s3, gsub, pattern="\"", replacement = "")
s5<- as.data.frame(s4)

s5 <- s5[-grep("unknown", s5$education), ]
s5 <- s5[-grep("unknown", s5$job), ]
s5 <- s5[-grep("unknown", s5$housing), ]
s5 <- s5[-grep("unknown", s5$marital), ]



#library(dplyr)
# baru <- s5 %>%
#   select(- c("marital","housing","default","loan","contact","duration","pdays","previous","poutcome","nr.employed","emp.var.rate"))

drop <- c("default","loan","duration","pdays","poutcome")
baru <- s5[ ,!(names(s5) %in% drop)]



baru["education"][baru["education"] == "basic.6y"] <- 6
baru["education"][baru["education"] == "basic.4y"] <- 4
baru["education"][baru["education"] == "basic.9y"] <- 9
baru["education"][baru["education"] == "university.degree"] <- 16
baru["education"][baru["education"] == "high.school"] <- 12
baru["education"][baru["education"] == "professional.course"] <- 17

baru["job"][baru["job"] == "unemployed"] <- 1
baru["job"][baru["job"] == "student"] <- 2
baru["job"][baru["job"] == "housemaid"] <- 3
baru["job"][baru["job"] == "services"] <- 4
baru["job"][baru["job"] == "blue-collar"] <- 5
baru["job"][baru["job"] == "self-employed"] <- 6
baru["job"][baru["job"] == "technician"] <- 7
baru["job"][baru["job"] == "admin."] <- 8
baru["job"][baru["job"] == "management"] <- 9
baru["job"][baru["job"] == "entrepreneur"] <- 10
baru["job"][baru["job"] == "retired"] <- 11

# baru["month"][baru["month"] == "jan"] <- 1
# baru["month"][baru["month"] == "feb"] <- 2
# baru["month"][baru["month"] == "mar"] <- 3
# baru["month"][baru["month"] == "apr"] <- 4
# baru["month"][baru["month"] == "may"] <- 5
# baru["month"][baru["month"] == "jun"] <- 6
# baru["month"][baru["month"] == "jul"] <- 7
# baru["month"][baru["month"] == "aug"] <- 8
# baru["month"][baru["month"] == "sep"] <- 9
# baru["month"][baru["month"] == "oct"] <- 10
# baru["month"][baru["month"] == "nov"] <- 11
# baru["month"][baru["month"] == "dec"] <- 12

baru$married <- as.numeric(baru["marital"]== "married")
baru$single <- as.numeric(baru["marital"]== "single")
baru$divorced <- as.numeric(baru["marital"]== "divorced")


baru$firstquarter <- as.numeric(baru["month"] == "jan" | baru["month"] == "feb" | baru["month"] == "mar")
baru$secondquarter <- as.numeric(baru["month"] == "apr" | baru["month"] == "may" | baru["month"] == "jun")
baru$thirdquarter <- as.numeric(baru["month"] == "jul" | baru["month"] == "aug" | baru["month"] == "sep")
baru$fourthquarter <- as.numeric(baru["month"] == "oct" | baru["month"] == "nov" | baru["month"] == "dec")

# baru["day_of_week"][baru["day_of_week"] == "mon"] <- 1
# baru["day_of_week"][baru["day_of_week"] == "tue"] <- 2
# baru["day_of_week"][baru["day_of_week"] == "wed"] <- 3
# baru["day_of_week"][baru["day_of_week"] == "thu"] <- 4
# baru["day_of_week"][baru["day_of_week"] == "fri"] <- 5

baru$monday <- as.numeric(baru["day_of_week"] == "mon")
baru$tuesday <- as.numeric(baru["day_of_week"] == "tue")
baru$wednesday <- as.numeric(baru["day_of_week"] == "wed")
baru$thursday <- as.numeric(baru["day_of_week"] == "thu")
baru$friday <- as.numeric(baru["day_of_week"] == "fri")

baru["y"][baru["y"] == "no"] <- 0
baru["y"][baru["y"] == "yes"] <- 1

baru["housing"][baru["housing"] == "no"] <- 0
baru["housing"][baru["housing"] == "yes"] <- 1

baru["contact"][baru["contact"] == "cellular"] <- 0
baru["contact"][baru["contact"] == "telephone"] <- 1


baru$housing <- as.numeric(baru$housing)
baru$contact <- as.numeric(baru$contact)
baru$emp.var.rate <- as.numeric(baru$emp.var.rate)
baru$nr.employed <- as.numeric(baru$nr.employed)
baru$previous <- as.numeric(baru$previous)
baru$campaign <- as.numeric(baru$campaign)
baru$age <- as.numeric(baru$age)
baru$job <- as.numeric(baru$job)
baru$education <- as.numeric(baru$education)
baru$campaign <- as.numeric(baru$campaign)
baru$cons.price.idx <- as.numeric(baru$cons.price.idx)
baru$cons.conf.idx <- as.numeric(baru$cons.conf.idx)
baru$euribor3m <- as.numeric(baru$euribor3m)
baru$y <- as.factor(baru$y)

colSums(is.na(baru))
baru <- na.omit(baru)
#baru$education[is.na(baru$education)] <- 0
colSums(is.na(baru))

baru <- baru[,-c(3,7,8)]


SMOTE <- function(form,data,
                  perc.over=200,k=5,
                  perc.under=200,
                  learner=NULL,...
)
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # minCl  the minority class label
  # per.over/100 is the number of new cases (smoted cases) generated
  #              for each rare case. If perc.over < 100 a single case
  #              is generated uniquely for a randomly selected perc.over
  #              of the rare cases
  # k is the number of neighbours to consider as the pool from where
  #   the new examples are generated
# perc.under/100 is the number of "normal" cases that are randomly
#                selected for each smoted case
# learner the learning system to use.
# ...  any learning parameters to pass to learner
{
  
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  minCl <- levels(data[,tgt])[which.min(table(data[,tgt]))]
  
  # get the cases of the minority class
  minExs <- which(data[,tgt] == minCl)
  
  # generate synthetic cases from these minExs
  if (tgt < ncol(data)) {
    cols <- 1:ncol(data)
    cols[c(tgt,ncol(data))] <- cols[c(ncol(data),tgt)]
    data <-  data[,cols]
  }
  newExs <- smote.exs(data[minExs,],ncol(data),perc.over,k)
  if (tgt < ncol(data)) {
    newExs <- newExs[,cols]
    data <- data[,cols]
  }
  
  # get the undersample of the "majority class" examples
  selMaj <- sample((1:NROW(data))[-minExs],
                   as.integer((perc.under/100)*nrow(newExs)),
                   replace=T)
  
  # the final data set (the undersample+the rare cases+the smoted exs)
  newdataset <- rbind(data[selMaj,],data[minExs,],newExs)
  
  # learn a model if required
  if (is.null(learner)) return(newdataset)
  else do.call(learner,list(form,newdataset,...))
}



# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
# L. Torgo, Feb 2010
# ---------------------------------------------------
smote.exs <- function(data,tgt,N,k)
  # INPUTS:
  # data are the rare cases (the minority "class" cases)
  # tgt is the name of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours to use for the generation
  # OUTPUTS:
  # The result of the function is a (N/100)*T set of generated
  # examples with rare values on the target
{
  nomatr <- c()
  T <- matrix(nrow=dim(data)[1],ncol=dim(data)[2]-1)
  for(col in seq.int(dim(T)[2]))
    if (class(data[,col]) %in% c('factor','character')) {
      T[,col] <- as.integer(data[,col])
      nomatr <- c(nomatr,col)
    } else T[,col] <- data[,col]
  
  if (N < 100) { # only a percentage of the T cases will be SMOTEd
    nT <- NROW(T)
    idx <- sample(1:nT,as.integer((N/100)*nT))
    T <- T[idx,]
    N <- 100
  }
  
  p <- dim(T)[2]
  nT <- dim(T)[1]
  
  ranges <- apply(T,2,max)-apply(T,2,min)
  
  nexs <-  as.integer(N/100) # this is the number of artificial exs generated
  # for each member of T
  new <- matrix(nrow=nexs*nT,ncol=p)    # the new cases
  
  for(i in 1:nT) {
    
    # the k NNs of case T[i,]
    xd <- scale(T,T[i,],ranges)
    for(a in nomatr) xd[,a] <- xd[,a]==0
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    kNNs <- order(dd)[2:(k+1)]
    
    for(n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k,1)
      
      ex <- vector(length=ncol(T))
      
      # the attribute values of the generated case
      difs <- T[kNNs[neig],]-T[i,]
      new[(i-1)*nexs+n,] <- T[i,]+runif(1)*difs
      for(a in nomatr)
        new[(i-1)*nexs+n,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
    }
  }
  newCases <- data.frame(new)
  for(a in nomatr)
    newCases[,a] <- factor(newCases[,a],levels=1:nlevels(data[,a]),labels=levels(data[,a]))
  
  newCases[,tgt] <- factor(rep(data[1,tgt],nrow(newCases)),levels=levels(data[,tgt]))
  colnames(newCases) <- colnames(data)
  newCases
}

baru <- SMOTE(form = y~.,data = baru, k = 5, perc.over = 100, perc.under = 200)

#assigning train and test data
library(caret)
set.seed(1)
data_index <- createDataPartition(baru$y, p = 0.8, list = FALSE)
train_data <- baru[data_index, ]
test_data <- baru[-data_index, ]
train_y <- baru[data_index, 10]
test_y <- baru[-data_index, 10]
# train_attributes <- baru[data_index, c(1:3,6:9,11:13,15:18)]
# test_attributes <- baru[-data_index, c(1:3,6:9,11:13,15:18)]
train_attributes <- baru[data_index, c(1:7,9:11,13:16)]
test_attributes <- baru[-data_index, c(1:7,9:11,13:16)]

#TEST KNN
library(class)
set.seed(1)
x <- 1:50
acc_knn <- rep(0,50)
for (i in x){
  knn_pred <- knn(train_data, train_data, train_y, k = i)
  conf_matrix <- table(knn_pred, train_y)
  acc_knn[i] <- (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix)
}
plot<- ggplot() + geom_line(aes(x,acc_knn))+scale_x_continuous(breaks =seq(0,50,by=1))
plot

#best knn
set.seed(1)
best_knn <- knn(train_data, test_data, train_y, k = 3)
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


#QDA
set.seed(1)
qda_model <- qda(y ~., data = train_data)
summary(qda_model)
qda_pred <- predict(qda_model, test_data)
qda_matrix <- table(qda_pred$class, test_y)
qda_matrix
(qda_matrix[1,1] + qda_matrix[2,2])/sum(qda_matrix)


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

svm_tune <- tune(svm, y~., data = train_data, kernel = "polynomial",
                 ranges = list(cost = 10^seq(-2,1, by=0.25)), scale= TRUE)
summary(svm_tune)

svm_after_tuned <- svm(y~., data = train_data, kernel = "polynomial", 
                       cost = svm_tune$best.parameters$cost)
pred <- predict(svm_after_tuned, test_data)
(poly_matrix <- table(test_data$y, pred))
(poly_matrix[1,1]+poly_matrix[2,2])/sum(poly_matrix)
