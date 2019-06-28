loan_data<-read.csv("loan_data.csv")
View(loan_data)

#data 파악하기
dim(loan_data)
summary(loan_data)

#data 자료형 변형 
loan_data$COMB_COMM<-as.factor(loan_data$COMB_COMM)
loan_data$TARGET<-as.factor(loan_data$TARGET)

str(loan_data)
attach(loan_data)



#training/test set나누기
library(caTools)
set.seed(100)
split <- sample.split(loan_data$TARGET,SplitRatio=0.7)
loan_train <- subset(loan_data, split == TRUE)
loan_test = subset(loan_data, split == FALSE)



#logistic regression model 구축
modLog = glm(TARGET~. , data=loan_train, family="binomial")
summary(modLog)

#iteration
modLog2 = glm(TARGET ~ TOT_LOAN + LOAN_BNK + LOAN_CPT + CRDT_CNT + GUARN_CNT + INCOME + LATE_RATE_1Y 
              + CANCEL_CNT_1Y + MOBILE_PRICE + SUSP_DAY + LATE_TEL + COMB_COMM + PAY_METHOD , 
              data=loan_train, family="binomial")
summary(modLog2)

#prediction
loan_test$predicted.risk = predict(modLog2, newdata=loan_test, type="response")
mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >= 0.5))
mat_loan


#cut-off value 조절
mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >=0.3))
mat_loan

#accuracy
num.one=mat_loan[2,1]+mat_loan[2,2]   #test set에서 실제 1의 개수 
num.zero=mat_loan[1,1]+mat_loan[1,2]  #test set에서 실제 0의 개수 


overallacc=(mat_loan[1,1]+mat_loan[2,2])/(num.one+num.zero)
overallacc
11776/13016

#실제1을 1로 분류된 것의 비율 
sensitivity=mat_loan[2,2]/num.one
sensitivity

#실제0을 0으로 분류한 것의 비율 
specificity=mat_loan[1,1]/num.zero
specificity


