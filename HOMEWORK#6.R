#homework6.Decision tree

wine <- read.csv("Wine.csv")
wine <- wine[1:178,1:14]
wine<-droplevels(wine) # null값인 level 제거
str(wine)
View(wine)     


#Q1. 전체 데이터를 이용하여 의사결정나무모델을 구축하고 해석하시오.
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
table(wine$Type)
tree <- rpart(Type ~., data = wine, method = "class")
tree
rpart.plot(tree)

printcp(tree)    #cp : complexity parameter
plotcp(tree)

bestcp.all <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.all.pruned <- prune(tree, cp = bestcp.all)
tree.all.pruned
rpart.plot(tree.all.pruned)





#Q2. Training Data를 이용해 의사결정나무모델을 구축하고  Testing Data를 이용하여 분류정확도를 구하시오.

set.seed(15123)
split<-sample.split(wine$Type, SplitRatio = 0.7)
train<-subset(wine,split==TRUE)
test<-subset(wine,split==FALSE)
tree.train <- rpart(Type ~., data = train, method = "class")
tree.train
rpart.plot(tree.train)

#pruning
printcp(tree.train)
plotcp(tree.train)
bestcp.train <- tree.train$cptable[which.min(tree.train$cptable[,"xerror"]),"CP"]
tree.train.pruned <- prune(tree.train, cp = bestcp.train)
tree.train.pruned
rpart.plot(tree.train.pruned)


tree.predict <- predict(tree.train.pruned, test, type = "class")
tree.cm<- table(test$Type, tree.predict)
tree.cm

install.packages("gmodels")
library(gmodels)
CrossTable(x = test$Type , y = tree.predict , prop.chisq = FALSE)

A_to_A.acc <- tree.cm[1,1]/sum(tree.cm[1,])
B_to_B.acc <- tree.cm[2,2]/sum(tree.cm[2,])
C_to_C.acc <- tree.cm[3,3]/sum(tree.cm[3,])
accuracy <- (tree.cm[1,1]+tree.cm[2,2]+tree.cm[3,3])/sum(tree.cm[,])
for (i in 1){
  print(paste(c("A to A accuracy : ",A_to_A.acc),collapse=""))
  print(paste(c("B to B accuracy : ",B_to_B.acc),collapse=""))
  print(paste(c("C to C accuracy : ",C_to_C.acc),collapse=""))
  print(paste(c("accuracy : ",accuracy),collapse=""))
}



#Q3.다른 종류의 비용함수를 사용함에 따라 결과가 어떻게 달라지는지 보이시오.

#----gini----
tree.train.gini <- rpart(Type ~., data = train, method = "class", parms = list(split = "gini"))
tree.train.gini
rpart.plot(tree.train.gini)

tree.predict.gini <- predict(tree.train.gini, test, type = "class")
tree.gini.cm<- table(test$Type, tree.predict.gini)
tree.gini.cm
CrossTable(x = test$Type , y = tree.predict.gini , prop.chisq = FALSE)

A_to_A.acc <- tree.gini.cm[1,1]/sum(tree.gini.cm[1,])
B_to_B.acc <- tree.gini.cm[2,2]/sum(tree.gini.cm[2,])
C_to_C.acc <- tree.gini.cm[3,3]/sum(tree.gini.cm[3,])
accuracy <- (tree.gini.cm[1,1]+tree.gini.cm[2,2]+tree.gini.cm[3,3])/sum(tree.gini.cm[,])
for (i in 1){
  print("Use gini index")
  print(paste(c("A to A accuracy : ",A_to_A.acc),collapse=""))
  print(paste(c("B to B accuracy : ",B_to_B.acc),collapse=""))
  print(paste(c("C to C accuracy : ",C_to_C.acc),collapse=""))
  print(paste(c("accuracy : ",accuracy),collapse=""))
}


#----entropy----
tree.train.information <- rpart(Type ~., data = train, method = "class", parms = list(split = "information"))
tree.train.information
rpart.plot(tree.train.information)

tree.predict.information <- predict(tree.train.information, test, type = "class")
tree.information.cm<- table(test$Type, tree.predict.information)
tree.information.cm
CrossTable(x = test$Type , y = tree.predict.information , prop.chisq = FALSE)

A_to_A.acc <- tree.information.cm[1,1]/sum(tree.information.cm[1,])
B_to_B.acc <- tree.information.cm[2,2]/sum(tree.information.cm[2,])
C_to_C.acc <- tree.information.cm[3,3]/sum(tree.information.cm[3,])
accuracy <- (tree.information.cm[1,1]+tree.information.cm[2,2]+tree.information.cm[3,3])/sum(tree.information.cm[,])
for (i in 1){
  print("Use entropy")
  print(paste(c("A to A accuracy : ",A_to_A.acc),collapse=""))
  print(paste(c("B to B accuracy : ",B_to_B.acc),collapse=""))
  print(paste(c("C to C accuracy : ",C_to_C.acc),collapse=""))
  print(paste(c("accuracy : ",accuracy),collapse=""))
}




