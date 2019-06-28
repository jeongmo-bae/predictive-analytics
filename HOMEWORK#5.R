#homework5.K-nearest neighbor
wine <- read.csv("Wine.csv")
wine<-wine[c(1:178),c(1:14)]





###Wine data(classification)###


##Q1

View(wine)
head(wine,10)
tail(wine,10)
dim(wine)
nrow(wine)
ncol(wine)
names(wine)
str(wine)
summary(wine)
attach(wine)

#input data nomalization
normalization <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wine_n <- cbind(Type, as.data.frame (lapply (wine[,2:14], normalization)))
View(wine_n)

#split data
set.seed(1000)
train.idx<-sample(1:nrow(wine_n),size=nrow(wine_n)*3/5,replace = F)
train.idx

wine.train <- wine_n[train.idx,-1]
wine.test <- wine_n[-train.idx,-1]
trainlabels <- wine_n[train.idx,1]
testlabels <- wine_n[-train.idx,1]
trainlabels
testlabels

#KNN algorithm(classification) packages&library
install.packages("class")
library(class)

#KNN algorithm(classification) 
model1 <- knn(train = wine.train , cl = trainlabels , test = wine.test , k=1 )
model1

model2 <- knn(train = wine.train , cl = trainlabels , test = wine.test , k=7 )
model2

model3 <- knn(train = wine.train , cl = trainlabels , test = wine.test , k=20)
model3

model4 <- knn(train = wine.train , cl = trainlabels , test = wine.test , k=21)
model4

model5 <- knn(train = wine.train , cl = trainlabels , test = wine.test , k=23)
model5




#KNN algorithm(classification) evaluation
install.packages("gmodels")
library(gmodels)
CrossTable(x = testlabels , y = model1 , prop.chisq = FALSE)       
CrossTable(x = testlabels , y = model2 , prop.chisq = FALSE)       
CrossTable(x = testlabels , y = model3 , prop.chisq = FALSE)       
CrossTable(x = testlabels , y = model4 , prop.chisq = FALSE)       
CrossTable(x = testlabels , y = model5 , prop.chisq = FALSE)      


#knn algorithm(classification) plot
install.packages("caret")
library(caret)
i=1                          
k.optm=1                     
for (i in 1:106){ 
  knn.mod <-  knn(train=wine.train, test=wine.test, cl=trainlabels, k=i)
  k.optm[i] <- (NROW(testlabels)-sum(testlabels == knn.mod))/NROW(testlabels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Misclassification Error")




##Q2
install.packages("knnGarden")
library(knnGarden)
model.manhattan <- knnVCN(TrnX = wine.train , OrigTrnG = trainlabels , ShowObs = F , TstX =  wine.test , K=20 , method = "manhattan" , p = 1 )
model.manhattan <- model.manhattan$TstXIBelong
model.manhattan
CrossTable(x = testlabels , y = model.manhattan , prop.chisq = FALSE)


model.euclidean <- knnVCN(TrnX = wine.train , OrigTrnG = trainlabels , ShowObs = F , TstX =  wine.test , K=20, method = "euclidean" , p = 2 )
model.euclidean <- model.euclidean$TstXIBelong
model.euclidean
CrossTable(x = testlabels , y = model.euclidean , prop.chisq = FALSE)


model.supuremum <- knnVCN(TrnX = wine.train , OrigTrnG = trainlabels , ShowObs = F , TstX =  wine.test , K=20 , method = "maximum"  )
model.supuremum <- model.supuremum$TstXIBelong
model.supuremum
CrossTable(x = testlabels , y = model.supuremum , prop.chisq = FALSE)



##Q3
install.packages("factoextra")
library(factoextra)
res.pca <- prcomp(wine[,-1], scale = TRUE)
c1 <- res.pca$rotation[,1]
c2 <- res.pca$rotation[,2]
df_c <- as.matrix(cbind(c1,c2))
df_c
wine_n_mat<-as.matrix(wine_n[,-1])
wine_n_mat
co_mat<-wine_n_mat%*%df_c
co_df <- as.data.frame(co_mat)
co_df <- cbind(Type,co_df)
co_df
attach(co_df)

set.seed(1000)
idx<-sample(1:nrow(co_df),size=nrow(co_df)*3/5,replace = F)
idx

trn <- co_df[idx,2:3]
tst <- co_df[-idx,2:3]
trnlabels <- co_df[idx,1]
tstlabels <- co_df[-idx,1]
trnlabels
tstlabels

require(class)
cls <- trnlabels
train <-trn
test <- expand.grid(x=seq(min(train[,1]-1), max(train[,1]+1),
                          by=0.1),
                    y=seq(min(train[,2]-1), max(train[,2]+1), 
                          by=0.1))

classif <- knn(train, test, cl=cls, k = 10, prob=TRUE)
prob <- attr(classif, "prob")


require(dplyr)

dataf <- bind_rows(mutate(test,
                          prob=prob,
                          cls="A",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="B",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="C",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)))

ggplot(dataf) +
  geom_point(aes(x=x, y=y, col=cls, size=prob),
             data = mutate(test, cls=classif)) + 
  scale_size(range=c(0.8, 2)) +
  geom_contour(aes(x=x, y=y, z=prob_cls, group=cls, color=cls),
               bins=2,
               data=dataf) +
  geom_point(aes(x=x, y=y, col=cls),
             size=3,
             data=data.frame(x=train[,1], y=train[,2], cls)) +
  geom_point(aes(x=x, y=y),
             size=3, shape=1,
             data=data.frame(x=train[,1], y=train[,2], cls)) 


#-------------------------------------------------------------------------------------#


###corolla data(prediction)###
corolla <- read.csv("ToyotaCorolla_Simple.csv")
corolla <-corolla[,c(1:10)]

##Q1
corolla <- corolla[,-1]
View(corolla)
head(corolla,10)
tail(corolla,10)
dim(corolla)
nrow(corolla)
ncol(corolla)
names(corolla)
str(corolla)
summary(corolla)
corolla$Model <- as.numeric(corolla$Model)
attach(corolla)


normalization <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
corolla_n <- cbind(Price..Y., as.data.frame(lapply (corolla[,1:8], normalization)))
View(corolla_n)


set.seed(1000)
train_idx<-sample(1:nrow(corolla_n),size=nrow(corolla_n)*2/3,replace = F)
train_idx

corolla.trn <- corolla_n[train_idx,-1]
corolla.tst <- corolla_n[-train_idx,-1]
corolla.trnlabels <- corolla_n[train_idx,1]
corolla.tstlabels <- corolla_n[-train_idx,1]
corolla.trnlabels
corolla.tstlabels



library(caret)
i=1                          
RMSE=1                     
for (i in 1:20){ 
  knn_mod <-  knnregTrain(train=corolla.trn, test=corolla.tst, corolla.trnlabels, k=i)
  knn_mod
  sum=0
  for (j in 1:479){
    sum <- sum + (corolla.tstlabels[j] - knn_mod[j])^2
  }
  RMSE[i] <- sqrt(sum/length(corolla.tstlabels))
  k=i
  cat('#k','=',k,':', 'RMSE=',RMSE[i],'\n')
}

plot(RMSE, type="b", xlab="K-Value",ylab="RMSE")





##Q2
install.packages("KernelKnn")
library(KernelKnn)

Perf_Table <- matrix(0, nrow = 1, ncol = 4)
colnames(Perf_Table) <- c("euclidean", "manhattan", "mahalanobis", "pearson correlation")
Perf_Table

# Euclidean
preds_TEST = KernelKnn(corolla.trn, TEST_data = corolla.tst, corolla.trnlabels, k = 7 , 
                       method = 'euclidean', weights_function = NULL, regression = T,
                       Levels = unique(y))
sum=0
for (i in 1:479){
  sum= sum+(corolla.tstlabels[i]-preds_TEST[i])^2}
sum=sqrt(sum/length(corolla.tstlabels))
Perf_Table[,1]<-sum

# Manhattan
preds_TEST = KernelKnn(corolla.trn, TEST_data = corolla.tst, corolla.trnlabels, k = 7 , 
                       method = 'manhattan', weights_function = NULL, regression = T,
                       Levels = unique(y))
sum=0
for (i in 1:479){
  sum= sum+(corolla.tstlabels[i]-preds_TEST[i])^2}
sum=sqrt(sum/length(corolla.tstlabels))
Perf_Table[,2]<-sum



# Mahalanobis
preds_TEST = KernelKnn(corolla.trn, TEST_data = corolla.tst, corolla.trnlabels, k = 7 , 
                       method = 'mahalanobis', weights_function = NULL, regression = T,
                       Levels = unique(y))
sum=0
for (i in 1:479){
  sum= sum+(corolla.tstlabels[i]-preds_TEST[i])^2}
sum=sqrt(sum/length(corolla.tstlabels))
Perf_Table[,3]<-sum
Perf_Table

# Pearson_Correlation
preds_TEST = KernelKnn(corolla.trn, TEST_data = corolla.tst, corolla.trnlabels, k = 7 , 
                       method = 'pearson_correlation', weights_function = NULL, regression = T,
                       Levels = unique(y))
sum=0
for (i in 1:479){
  sum= sum+(corolla.tstlabels[i]-preds_TEST[i])^2}
sum=sqrt(sum/length(corolla.tstlabels))
Perf_Table[,4]<-sum
Perf_Table






