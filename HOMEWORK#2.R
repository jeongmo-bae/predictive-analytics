par(mfrow=c(2,2))
par(mfrow=c(1,1))

#data불러오기
setwd("C:/Users/Bae jeong mo/Desktop/2014170849/2019/4-1/예측애널리틱스/과제2")
data_bike<-read.csv("hour.csv")
View(data_bike)

#data 파악하기(기초)
dim(data_bike)
str(data_bike)
summary(data_bike)

#data handling
dfb=data_bike[,-c(1,2,15,16)]
dfb$season = factor(dfb$season, levels=c(1:4), labels=c('spring','summer','fall','winter'))
dfb$yr = factor(dfb$yr, levels=c(0,1), labels=c('2011Y','2012Y'))
dfb$mnth = factor(dfb$mnth, levels=c(1:12), labels=c('1M','2M','3M','4M','5M','6M','7M','8M','9M',
                                                                 '10M','11M','12M'))
dfb$hr = factor(dfb$hr, levels=c(0:23), labels=c('0h','1h','2h','3h','4h','5h','6h','7h','8h','9h','10h',
                                                             '11h','12h','13h','14h','15h','16h','17h','18h','19h',
                                                             '20h','21h','22h','23h'))
dfb$holiday = factor(dfb$holiday, levels=c(0,1), labels=c('공휴일X','공휴일O'))
dfb$weekday = factor(dfb$weekday, levels=c(0:6), labels=c('SUN','MON','TUE','WED','THR','FRI','SAT'))
dfb$workingday = factor(dfb$workingday, levels=c(0,1), labels=c('쉼','안쉼'))
dfb$weathersit = factor(dfb$weathersit, levels=c(1:4), labels=c('clear','mist','light snow','heavy rain'))
str(dfb)

#attach를 쓰면 컬럼명으로 해당컬럼 호출가능
install.packages("psych")
library(psych)
describe(dfb)
attach(dfb)       




#data 파악하기 (관계)
library(ggplot2)

#<1>X vs Y scatter plot 
ggplot(data=dfb,aes(x=season,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=yr,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=mnth,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=hr,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=holiday,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=weekday,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=workingday,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=weathersit,y=cnt))+geom_point()

ggplot(data=dfb,aes(x=temp,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=atemp,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=hum,y=cnt))+geom_point()
ggplot(data=dfb,aes(x=windspeed,y=cnt))+geom_point()


#<2>continueos data correlation
dfb_con<-dfb[,9:12]
con_cor<-cor(dfb_con)
con_cor
install.packages('corrplot')
library(corrplot)
corrplot(con_cor,method='color',addCoef.col='black')


#<3>training/test set 나누기
library('caTools')
set.seed(123)
split<-sample.split(dfb$cnt, SplitRatio = 0.7)
dfb_train<-subset(dfb,split==TRUE)
dfb_test<-subset(dfb,split==FALSE)


#linear regression model
lm_dfb <- lm(cnt~.,data = dfb_train)         #lm(y~x1+x2+....)인데 . 하면 y 빼고 데이터 전부로 돌리겠다는 의미
summary(lm_dfb)

#residual plot
plot(lm_dfb)

#log transform
lm_dfb_log<-lm(log(cnt)~.,data=dfb_train)
summary(lm_dfb_log)
plot(lm_dfb_log)

#stepwise Model selection
library(MASS)
lm_dfb_logAIC<-stepAIC(lm_dfb_log,direction='both')
summary(lm_dfb_logAIC)

#multi-collinearity
install.packages('car')
library(car)
vif(lm_dfb_logAIC)   #mnth 랑 atemp 빼자... vif 값이 높으면서 모델에서의 pvalue도 높아서!

#iteration1
dfb_train_M<-dfb_train[,-c(1,7)]
lm_dfb_log2<- lm(log(cnt)~.,data = dfb_train_M)
lm_dfb_log2AIC<-stepAIC(lm_dfb_log2,direction='both')
vif(lm_dfb_log2AIC)

#iteration2
dfb_train_M2<-dfb_train_M[,-7]
lm_dfb_log3<-lm(log(cnt)~.,data=dfb_train_M2)
lm_dfb_log3AIC<-stepAIC(lm_dfb_log3,direction='both')
vif(lm_dfb_log3AIC)

#conclusion
summary(lm_dfb_log3AIC)
plot(lm_dfb_log3AIC)



#각 회귀계수의 95% CI
confint(lm_dfb_log3AIC)

#각 회귀계수의 기울기=0 여부 검정

#예측진단
pre<-predict(lm_dfb_log3AIC,newdata=dfb_test,interval='predict')
pre<-as.data.frame(pre)
pre<-cbind(pre,log(dfb_test$cnt))
tf<-F
pre<-cbind(pre,tf)
pre$tf[pre$'log(dfb_test$cnt)'>=pre$lwr & pre$'log(dfb_test$cnt)'<=pre$upr]<-T
sum(pre$tf=="TRUE")/dim(pre)[1]


#ANOVA test
anova(lm_dfb_log3AIC)

