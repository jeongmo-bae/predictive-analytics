#homework4.Ridge/LASSO
data<-read.csv("BostonHousing.csv")
View(data)


#Q1
install.packages("glmnet")
library(glmnet)
library('caTools')

df_beta <- data.frame()
for(i in 1:30){
split<-sample.split(data$MEDV, SplitRatio = 0.8)
train<-subset(data,split==TRUE)
test<-subset(data,split==FALSE)

x <- model.matrix(MEDV~., train)[,-1]
y <- train$MEDV
cv.lasso <- cv.glmnet(x, y, alpha = 1)
model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min)
print(coef(model))
df_beta[i,1]<-model$beta[1,]
df_beta[i,2]<-model$beta[2,]
df_beta[i,3]<-model$beta[3,]
df_beta[i,4]<-model$beta[4,]
df_beta[i,5]<-model$beta[5,]
df_beta[i,6]<-model$beta[6,]
df_beta[i,7]<-model$beta[7,]
df_beta[i,8]<-model$beta[8,]
df_beta[i,9]<-model$beta[9,]
df_beta[i,10]<-model$beta[10,]
df_beta[i,11]<-model$beta[11,]
  
}
colnames(df_beta) <- colnames(x)
rownames(df_beta) <- paste("train", c(1:30))

boxplot(df_beta)
dev.off()



#Q2
install.packages('corrplot')
library(corrplot)
cor_data<-cor(data[,-12])
corrplot(cor_data,method='color',addCoef.col='black')
data_indep<-data[,-c(2,3,4,6,7,8)]
data_dep<-data[,-c(1,2,5,9,10,11)]
data_dep<-data_dep[1:200,]

cor_data_indep<-cor(data_indep[,-6])
corrplot(cor_data_indep,method='color',addCoef.col='black')

cor_data_dep<-cor(data_dep[,-6])
corrplot(cor_data_dep,method='color',addCoef.col='black')



#independent variables
df_beta_indep<-data.frame()
for( i in 1:100){
split<-sample.split(data_indep$MEDV, SplitRatio = 0.8)
train_indep<-subset(data_indep,split==TRUE)
test_indep<-subset(data_indep,split==FALSE)
  
x_indep <- model.matrix(MEDV~., train_indep)[,-1]
y_indep <- train_indep$MEDV
cv.lasso_indep <- cv.glmnet(x_indep, y_indep, alpha = 1)
model_indep <- glmnet(x_indep, y_indep, alpha = 1, lambda = cv.lasso_indep$lambda.min)
print(coef(model_indep))
df_beta_indep[i,1]<-model_indep$beta[1,]
df_beta_indep[i,2]<-model_indep$beta[2,]
df_beta_indep[i,3]<-model_indep$beta[3,]
df_beta_indep[i,4]<-model_indep$beta[4,]
df_beta_indep[i,5]<-model_indep$beta[5,]
}
colnames(df_beta_indep) <- colnames(x_indep)
rownames(df_beta_indep) <- paste("train2-", c(1:100))  

boxplot(df_beta_indep)
dev.off()

#dependent variables
df_beta_dep<-data.frame()
for(i in 1:100){
  split<-sample.split(data_dep$MEDV, SplitRatio = 0.8)
  train_dep<-subset(data_dep,split==TRUE)
  test_dep<-subset(data_dep,split==FALSE)

  x_dep <- model.matrix(MEDV~., train_dep)[,-1]
  y_dep <- train_dep$MEDV
  cv.lasso_dep <- cv.glmnet(x_dep, y_dep, alpha = 1)
  model_dep <- glmnet(x_dep, y_dep, alpha = 1, lambda = cv.lasso_dep$lambda.min)
  print(coef(model_dep))
  
  
  df_beta_dep[i,1]<-model_dep$beta[1,]
  df_beta_dep[i,2]<-model_dep$beta[2,]
  df_beta_dep[i,3]<-model_dep$beta[3,]
  df_beta_dep[i,4]<-model_dep$beta[4,]
  df_beta_dep[i,5]<-model_dep$beta[5,]
}
colnames(df_beta_indep) <- colnames(x_dep)
rownames(df_beta_indep) <- paste("train3-", c(1:100))


boxplot(df_beta_dep)
dev.off()
