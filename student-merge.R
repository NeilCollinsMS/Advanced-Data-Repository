mathscores <- read.csv("~/MSBA/Advanced Data Analytics/Project/Student/mathscores.csv", stringsAsFactors = T)
portscores <- read.csv("~/MSBA/Advanced Data Analytics/Project/Student/portscores.csv", stringsAsFactors = T)

write.csv(mathscores, "~/MSBA/Advanced Data Analytics/Project/Student/mathscores.csv", row.names = FALSE)
write.csv(portscores, "~/MSBA/Advanced Data Analytics/Project/Student/portscores.csv", row.names = FALSE)

#Library objects
library(stringr)
library(corrplot)
library(MASS)
library(pROC)
library(tidyverse)
library(forcats)
library(dplyr)
library(caret)
library(glmnet)
library(psych)
library(DescTools)
library(cluster)
library(plyr)
library(repr)
library(caret)
library(ggplot2)
library(tree)
library(blorr)
library(xgboost)
library(class)
library(ranger)


#Not necessary if you read in the csvs above (mathscores, portscores)

# mathscores$Unsatisfactory <- ifelse(mathscores$G3 <= 5,1,0)
# portscores$Unsatisfactory <- ifelse(portscores$G3 <= 5,1,0)
# mathscores$schoolsup <- ifelse(mathscores$schoolsup == 'yes', 1,0)
# mathscores$famsup <- ifelse(mathscores$famsup == 'yes', 1,0)
# mathscores$paid <- ifelse(mathscores$paid == 'yes', 1,0)
# mathscores$activities <- ifelse(mathscores$activities == 'yes', 1,0)
# mathscores$nursery <- ifelse(mathscores$nursery == 'yes', 1,0)
# mathscores$higher <- ifelse(mathscores$higher == 'yes', 1,0)
# mathscores$internet <- ifelse(mathscores$internet == 'yes', 1,0)
# mathscores$romantic <- ifelse(mathscores$romantic == 'yes', 1,0)
# portscores$schoolsup <- ifelse(portscores$schoolsup == 'yes', 1,0)
# portscores$famsup <- ifelse(portscores$famsup == 'yes', 1,0)
# portscores$paid <- ifelse(portscores$paid == 'yes', 1,0)
# portscores$activities <- ifelse(portscores$activities == 'yes', 1,0)
# portscores$nursery <- ifelse(portscores$nursery == 'yes', 1,0)
# portscores$higher <- ifelse(portscores$higher == 'yes', 1,0)
# portscores$internet <- ifelse(portscores$internet == 'yes', 1,0)
# portscores$romantic <- ifelse(portscores$romantic == 'yes', 1,0)
# mathscores$Male <- ifelse(mathscores$sex == 'M',1,0)
# portscores$Male <- ifelse(portscores$G3 == 'M',1,0)



# mathscores$Ranking <- rep(NA, times = length(mathscores$G3))
# for(s in 1:length(mathscores$G3)){
#   if (mathscores$G3[s] <=5){
#     mathscores$Ranking[s] <- "Unsatisfactory"
#   } else if(mathscores$G3[s] <= 13) {
#     mathscores$Ranking[s] <- "Partial"
#   }else {
#     mathscores$Ranking[s] <- "Proficient"
#   }
# }
# 
# View(mathscores)
# 
# 
# portscores$Ranking <- rep(NA, times = length(portscores$G3))
# for(s in 1:length(portscores$G3)){
#   if (portscores$G3[s] <=5){
#     portscores$Ranking[s] <- "Unsatisfactory"
#   } else if(portscores$G3[s] <= 13) {
#     portscores$Ranking[s] <- "Partial"
#   }else {
#     portscores$Ranking[s] <- "Proficient"
#   }
# }
# 
# View(portscores)

# Check the transformations were correctly applied

str(portscores)
str(mathscores)


#Check the dimensions of the binary field "Unsatisfactory"

sum(mathscores$Unsatisfactory)
sum(portscores$Unsatisfactory)


#segment out the numeric variables to check for multicollinearity indicators

num_cols <- unlist(lapply(mathscores, is.numeric))
mathscoresNum <- mathscores[ , num_cols] 

portnum_cols <- unlist(lapply(portscores, is.numeric))
portscoresNum <- portscores[ ,portnum_cols]

corrplot(cor(portscoresNum), method = 'number')
corrplot(cor(mathscoresNum), method = 'number')

#Create Test and Train Datasets
set.seed(5415)
sample1 <- floor(0.70 * nrow(mathscores))
sample2 <- floor(0.70 * nrow(portscores))


train_div <- sample(seq_len(nrow(mathscores)), size = sample1)
train_div2 <- sample(seq_len(nrow(portscores)), size = sample2)

train <- mathscores[train_div, ]
test <- mathscores[-train_div, ]

train2 <- portscores[train_div2,]
test2 <- portscores[-train_div2,]

#Logistic Regression of Unsatisfactory

testglm <- glm(Unsatisfactory~.-(G3+G2+G1+Ranking), train, family = binomial)
summary(testglm)
stepAIC(testglm)
PseudoR2(testglm, which = 'McFaddenAdj')

mathglm1 <- glm(Unsatisfactory ~ sex + famsize + Pstatus + failures + paid + romantic + famrel + goout + absences, family = binomial, train)
summary(mathglm1)
PseudoR2(mathglm1)
PseudoR2(mathglm2, which = "McFaddenAdj") #check for additional variable penalties

mathglm2 <- glm(Unsatisfactory ~ sex + famsize +  failures + paid + romantic + famrel + goout + absences, family = binomial, train)
summary(mathglm2)
PseudoR2(mathglm2)
PseudoR2(mathglm2, which = "McFaddenAdj")

mathglm3 <- glm(Unsatisfactory~absences+failures, train, family = 'binomial')
summary(mathglm3)
PseudoR2(mathglm3)
PseudoR2(mathglm3, which = "McFaddenAdj")


#Trees to find the top variables for the logistic regression


math.tree <- tree(Unsatisfactory~.-(G3+G2+G1+Ranking), train)
plot(math.tree)
text(math.tree)

math.treev <- cv.tree(math.tree, ,prune.tree)
summary(math.treev)

mtreeplot <- cbind(math.treev$dev,math.treev$size)
plot(math.treev$size, math.treev$dev)

prune.math <- prune.tree(math.tree,,best = 3)
plot(prune.math)
text(prune.math)


port.tree <- tree(Unsatisfactory~.-(G3+G2+G1+Ranking), train2) #use top two coefficients to start logistic model
plot(port.tree)
text(port.tree)


#Descriptive statistics of the two datasets

summary(mathscores)
summary(portscores)


#Predict Unsatisfactory scores using a linear regression

test1 <- lm(Unsatisfactory~.,train)

test2 <- lm(Unsatisfactory~.,train2)

#Random Forest: Math Scores

set.seed(5415)
index = sample(1:nrow(mathscores), 0.70*nrow(mathscores)) 
train = mathscores[index,] # Create the training data 
test = mathscores[-index,] # Create the test data
mathrf <- ranger(Unsatisfactory~. -(G3+G2+G1), train, num.trees = 100)

summary(mathrf$forest)


eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on test data
predictions_test <- predict(mathrf, test)
eval_results(test$Unsatisfactory, predictions_test$predictions, test) #R2 = .826, RMSE = 0.139

# Prediction and evaluation on test data
predictions_train <- predict(mathrf, train)
eval_results(train$Unsatisfactory, predictions_train$predictions, train) #R2 = 0.942, RMSE = 0.076

#Random Forest: Math Scores

set.seed(5415)
index = sample(1:nrow(mathscores), 0.70*nrow(mathscores)) 
train = mathscores[index,] # Create the training data 
test = mathscores[-index,] # Create the test data
mathrf <- ranger(Unsatisfactory~. -(G3+G2+G1), train, num.trees = 100)

summary(mathrf$forest)


eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on test data
predictions_test <- predict(mathrf, test)
eval_results(test$Unsatisfactory, predictions_test$predictions, test) #R2 = .826, RMSE = 0.139

# Prediction and evaluation on test data
predictions_train <- predict(mathrf, train)
eval_results(train$Unsatisfactory, predictions_train$predictions, train) #R2 = 0.942, RMSE = 0.076


#Extreme Gradient Boosted Linear Regression for Math Scores (G3)
set.seed(5415)

num_cols <- unlist(lapply(mathscores, is.numeric))
mathnums <- mathscores[ , num_cols] 
mathnums$G2 <- NULL
mathnums$G1 <- NULL
mathnums$G3 <- NULL

index = sample(1:nrow(mathnums), 0.60*nrow(mathnums)) 
train = mathnums[index,] # Create the training data 
test = mathnums[-index,] # Create the test data


train_y = train[,'Unsatisfactory']
train_x = train[, names(train) !='Unsatisfactory']

test_y = test[,'Unsatisfactory']
test_x = test[, names(test) !='Unsatisfactory']

dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)

watchlist = list(train=dtrain, test=dtest)
  

#Logistic XGBOOST

math = xgb.train(data = dtrain, 
                max.depth = 4, 
                eta = 0.2, 
                nthread = 2, 
                nround = 1000, 
                watchlist = watchlist, 
                objective = "reg:logistic", 
                early_stopping_rounds = 50,
                print_every_n = 500)

port = xgb.train(data = dtrain, 
                 max.depth = 6, 
                 eta = 0.3, 
                 nthread = 2, 
                 nround = 1000, 
                 watchlist = watchlist, 
                 objective = "reg:logistic", 
                 early_stopping_rounds = 50,
                 print_every_n = 500)


# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(math, newdata = dtrain)
eval_results(train_y, predictions_train, dtrain)

# Prediction and evaluation on test data
predictions_test <- predict(math, newdata = dtest)
eval_results(test_y, predictions_test, dtest)

#Linear XGBOOST of Math Scores

set.seed(5415)

num_cols <- unlist(lapply(mathscores, is.numeric))
mathnums <- mathscores[ , num_cols] 
mathnums$G1 <- NULL
mathnums$Unsatisfactory <- NULL

index = sample(1:nrow(mathnums), 0.70*nrow(mathnums)) 
train = mathnums[index,] # Create the training data 
test = mathnums[-index,] # Create the test data


train_y = train[,'G3']
train_x = train[, names(train) !='G3']

test_y = test[,'G3']
test_x = test[, names(test) !='G3']

dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)

watchlist = list(train=dtrain, test=dtest)


mathxgb = xgb.train(data = dtrain, 
                max.depth = 6, 
                eta = 0.05, 
                nthread = 2, 
                nround = 1000, 
                watchlist = watchlist, 
                objective = "reg:linear", 
                early_stopping_rounds = 50,
                print_every_n = 500)

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
#predictions_train <- predict(bst, newdata = dtrain)
#eval_results(train_y, predictions_train, dtrain)

# Prediction and evaluation on test data
predictions_test <- predict(mathxgb, newdata = dtest)
eval_results(test_y, predictions_test, dtest) #RMSE = 1.48, R2 = 0.88


#Linear XGBOOST of Portuguese Scores

set.seed(5415)

num_cols <- unlist(lapply(portscores, is.numeric))
portnums <- portscores[ , num_cols] 
portnums$G1 <- NULL
portnums$Unsatisfactory <- NULL

index = sample(1:nrow(portnums), 0.70*nrow(portnums)) 
train = portnums[index,] # Create the training data 
test = portnums[-index,] # Create the test data


train_y = train[,'G3']
train_x = train[, names(train) !='G3']

test_y = test[,'G3']
test_x = test[, names(test) !='G3']

dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)

watchlist = list(train=dtrain, test=dtest)


portxgb = xgb.train(data = dtrain, 
                max.depth = 10, 
                eta = 0.01, 
                nthread = 8, 
                nround = 2000, 
                watchlist = watchlist, 
                objective = "reg:linear", 
                early_stopping_rounds = 50,
                print_every_n = 500)

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
#predictions_train <- predict(bst, newdata = dtrain)
#eval_results(train_y, predictions_train, dtrain)

# Prediction and evaluation on test data
predictions_test <- predict(portxgb, newdata = dtest)
eval_results(test_y, predictions_test, dtest) #RMSE = 1.62, R2 = 0.745, MAE = 0.976



