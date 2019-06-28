trainset <- read.csv("train-playoff.csv")
View(trainset)
trainset_x <-trainset[,-27] 
trainset_y <-trainset[,27]

install.packages("dplyr")
library(dplyr)
testset <- read.csv("test-playoff.csv")
test <- filter(testset, testset$result != 'na')
test$result <- droplevels(test$result)
View(test)


library(caTools)
set.seed(123)
split <- sample.split(trainset$Output, SplitRatio = 0.7)
trn <- subset(trainset,split==TRUE)
val <- subset(trainset,split==FALSE)

trn_x <- trn[,-27]
trn_y <- trn[,27]
val_x <- val[,-27]
val_y <- val[,27]
test_x <- test[,-27]
test_y <- test[,27]

#KNN algorithm(classification) packages&library
install.packages("class")
library(class)

#find the optimal K value (using training / validation dataset)
i=1                          
k.optm=1                     
for (i in 1:499){ 
  knn.mod <-  knn(train=trn_x, test=val_x, cl=trn_y, k=i)
  k.optm[i] <- sum(val_y == knn.mod)/NROW(val_y)
  k=i  
  cat(k,'=',k.optm[i],'\n')  
}

plot(k.optm, type="b", xlab="K- Value",ylab="accuracy of validation set")

# k=99 ---> maximum accuracy of 0.7420382  ----> best k!!!

knn.optimal <-  knn(train=trainset_x, test=test_x, cl=trainset_y, k=99)
accuracy <- sum(test_y == knn.optimal)/NROW(test_y)
cat('when K=99, accuracy:#correctly predict/#total data  =',accuracy,'\n')

test <- cbind(test,knn.optimal)

test_Y_PREDICT <- test[,27:28]
colnames(test_Y_PREDICT) <- c("Y" , "predicted Y")
write.csv(test_Y_PREDICT,'test_Y_PREDICT.csv')



# Neural Network------------------------------------------------------------------
#NEURAL NETWORK MODEL
summary(trainset)
str(trainset)

#정규화 & 데이터프레임으로
trainset_scaled<-trainset
trainset_scaled[,-27]<-scale(trainset_scaled[,-27],T,T)
trainset_scaled
df_NCAA <-as.data.frame(trainset_scaled)

test_scaled<-test
test_scaled[,-27]<-scale(test_scaled[,-27],T,T)
df_test <-as.data.frame(test_scaled)

#Neural Network Model 학습
install.packages("neuralnet")
library(neuralnet)
set.seed(12342)
nn <- neuralnet(
  Output~.,
  data=df_NCAA, hidden=c(10,10), threshold=0.01,
  act.fct = "logistic")
nn$result.matrix #가중치 값, error, step수
plot(nn)
nn$net.result[[1]] #예측확률

#공분산과 예측확률
col<-colnames(nn$covariate)
out <- cbind(nn$covariate,nn$net.result[[1]])
head(out)


#일반화 가중치 보기(그 변수에 따른 예측변화 있는가, 0이면 영향x)
par(mfrow=c(2,2))
for (i in 1:26){
  gwplot(nn,selected.covariate=col[i], min=-10, max=10)}

#install.packages("NeuralNetTools")
install.packages("NeuralNetTools")
library(NeuralNetTools)
garson(nn) #각 변수의 중요도->대체적으로 비중 낮다

#예측
Predict=compute(nn,df_test[,-27])
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
colnames(pred) <-c("predicted Y") 

Y_Yhat_mat <- cbind(test, pred)
Y_Yhat_mat <- Y_Yhat_mat[,27:28]
Y_Yhat_mat
write.csv(Y_Yhat_mat,"Y_Yhat_mat.csv")

install.packages("gmodels")
library(gmodels)
CrossTable(x = test$result , y = as.factor(pred) , prop.chisq = FALSE) #58.73% 
#돌릴때마다 다른 결과, hidden layer조건 바꿔도 예측 성능 비슷한걸(60%) 보면 모델 기반 외에
#다른 외부 요인들의 영향이 크다는것 암시.

#model

nn.bp2 <- neuralnet(
  Output~., stepmax=1e6, threshold=0.01,#수렴 안될때는 stepmax, threshold값 크게
  data=df_NCAA, hidden=c(5,5), algorithm="backprop",learningrate=0.1,
  linear.output=FALSE)
nn.bp2$result.matrix
plot(nn.bp2)

out <- cbind(nn.bp2$covariate,nn.bp2$net.result[[1]])
head(out)

col<-colnames(nn.bp2$covariate)

Predict=compute(nn.bp2,df_test[,-27])
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)

CrossTable(x = test$result , y = as.factor(pred) , prop.chisq = FALSE) #74.6%


#nnet package
install.packages("nnet")

library(nnet)
str(df_test)
set.seed(12342)
model.nnet <- nnet(Output ~ ., data = df_NCAA, size =10,decay = 0.0005)
summary(model.nnet)

#그리기
install.packages("devtools")
library(devtools)
#source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
par(mfrow=c(1,1))
plot.nnet(model.nnet)

#예측
prob<-predict(model.nnet, df_test[,-27])
pred <- ifelse(prob>0.5, 1, 0)

library("caret")
confusionMatrix(as.factor(pred),df_test$result) #69.84%


#출처:https://woosa7.github.io/R-%EC%9D%B8%EA%B3%B5%EC%8B%A0%EA%B2%BD%EB%A7%9D-NeuralNetwork/



