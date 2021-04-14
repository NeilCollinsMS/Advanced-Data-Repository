mathscores <- read.csv('student-mat.csv', sep=";", stringsAsFactors = T)

#convert some columns into factors that are ints
factorize <- c('Medu', 'Fedu', 'traveltime','studytime','famrel','freetime','goout','Dalc','Walc','health')

for (i in colnames(mathscores)){
  if (i %in% factorize){
    mathscores[,i] <- as.factor(mathscores[,i])
  }
}

str(mathscores)

library(caret)

#Lots of educational measuring is bucketed into groups based on proficiency.
mathscores$prof <- ifelse(mathscores$G3 < 6, 'Unsatisfactory',
                          ifelse(mathscores$G3 < 13, 'Partial', 'Proficient'))

mathscores$prof <- as.factor(mathscores$prof )

table(mathscores$prof)/nrow(mathscores)
#We can see that in our full dataset, 55% of kids are partially proficient, 33% are proficient and 12% are Unsatisfactory

set.seed(300)

#splitting data
train <- sample(nrow(mathscores), nrow(mathscores) * .6)
math.train <- mathscores[train, ]
math.test <- mathscores[-train, ]

#similar percentages in the test set for the buckets. We'll compare this to our predictions of G3 scores.
table(math.test$prof)/nrow(math.test)

#let's look at variable importance

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Train model using cubist method to find most important features.
# picking cubist method since we are looking at nearest neighbors
model <- train(G3~., data=mathscores, method='cubist', trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#we'll use the most important features until we can lower or rmse. From consultations with other teammates, G1 is important but increases our RMSE. Now need to look for the right number of K's
vk <- seq(1, 25, 1)

accuracy <- vk

for (i in 1:length(vk)) {
  knn <- knnreg(G3 ~ G2+absences+famrel+reason+schoolsup+age+nursery+health+Fedu+activities+Dalc+studytime+Fjob+failures, data = math.train, k = vk[i])
  accuracy[i] <- mean((math.test$G3 - round(predict(knn, newdata = math.test),0))^2)
}

#plot to find right number of k's, Looks like 7 k's is the best
plot(vk, accuracy, xlab = "k", ylab = "test accuracy", col = "blue")


knn <- knnreg(G3 ~ G2+absences+famrel+reason+schoolsup+age+nursery+health+Fedu+activities+Dalc+studytime+Fjob+failures, data = math.train, k =7)

#R2 of .898
preds = round(predict(knn, newdata = math.test),0)
rsq <- function (x, y) cor(x, y) ^ 2

rsq(math.test$G3,preds)

#RMSE for predictions on test set 1.544
sqrt(mean((math.test$G3 - round(predict(knn, newdata = math.test),0))^2))

preds_df = data.frame(preds = preds)
preds_df$prof <- ifelse(preds < 6, 'Unsatisfactory',
                          ifelse(preds < 13, 'Partial', 'Proficient'))

#Looks Like our model predicts more partially proficient scores and less proficient but its close.
table(preds_df$prof)/nrow(preds_df)

#Let's check portugese class

port <- read.csv('student-por.csv', sep=";", stringsAsFactors = T)

for (i in colnames(port)){
  if (i %in% factorize){
    port[,i] <- as.factor(port[,i])
  }
}

#Lets check port prof numbers
port$prof <- ifelse(port$G3 < 6, 'Unsatisfactory',
                          ifelse(port$G3 < 13, 'Partial', 'Proficient'))

port$prof <- as.factor(port$prof )

table(port$prof)/nrow(port)

#55% partial prof, 43% Prof, 3% Unsatisfactory

set.seed(300)

#Split the data
train <- sample(nrow(port), nrow(port) * .6)
port.train <- port[train, ]
port.test <- port[-train, ]


table(port.test$prof)/nrow(port.test)
#test data is 58% partial pro, 40% pro, 2% unsat

#Lets find the important features like with mathscores
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Train model using cubist method to find most important features.
model <- train(G3~., data=port, method='cubist', trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Let's find the right number of k's, this time G1 is important to leave in.
for (i in 1:length(vk)) {
  knn <-knnreg(G3 ~ G2+G1+absences+famrel+schoolsup+age+reason+nursery+Fedu+Dalc+health+activities+Pstatus+Medu, data = port.train, k = vk[i])
  accuracy[i] <- sqrt(mean((port.test$G3 - round(predict(knn, newdata = port.test),0))^2))
}

plot(vk, accuracy, xlab = "k", ylab = "test accuracy", col = "blue")
#Looks likee 8 k's is the optimal amount

knn_port <- knnreg(G3 ~ G2+G1+absences+famrel+schoolsup+age+reason+nursery+Fedu+Dalc+health+activities+Pstatus+Medu, data = port.train, k = 8)


#Rsquared of .866
preds = round(predict(knn_port, newdata = port.test),0)
rsq <- function (x, y) cor(x, y) ^ 2

rsq(port.test$G3,preds)


#1.123182 RMSE!
sqrt(mean((port.test$G3 - round(predict(knn_port, newdata = port.test),0))^2))

preds_df = data.frame(preds = preds)
preds_df$prof <- ifelse(preds < 6, 'Unsatisfactory',
                        ifelse(preds < 13, 'Partial', 'Proficient'))

#Looks Like our model predicts prof distribution pretty well! Test partial percentage is 59%, prof is right on at 40% and unsat is slightly off at 1%.
table(preds_df$prof)/nrow(preds_df)


#RMSE for math model on port
sqrt(mean((port$G3 - round(predict(knn, newdata = port),0))^2))

#RMSE for port model on math scores
sqrt(mean((mathscores$G3 - round(predict(knn_port, newdata = mathscores),0))^2))

###
#Cubist

#method = 'cubist'
#Type: Regression

#Tuning parameters:
  
  #committees (#Committees)
    #neighbors (#Instances)
      #Required packages: Cubist
      
      #A model-specific variable importance metric is available.
###