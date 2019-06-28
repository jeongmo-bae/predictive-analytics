loan_data<-read.csv("loan_data.csv")
View(loan_data)

#data �ľ��ϱ�
dim(loan_data)
summary(loan_data)

#data �ڷ��� ���� 
loan_data$COMB_COMM<-as.factor(loan_data$COMB_COMM)
loan_data$TARGET<-as.factor(loan_data$TARGET)

str(loan_data)
attach(loan_data)



#training/test set������
library(caTools)
set.seed(100)
split <- sample.split(loan_data$TARGET,SplitRatio=0.7)
loan_train <- subset(loan_data, split == TRUE)
loan_test = subset(loan_data, split == FALSE)



#logistic regression model ����
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


#cut-off value ����
mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >=0.3))
mat_loan

#accuracy
num.one=mat_loan[2,1]+mat_loan[2,2]   #test set���� ���� 1�� ���� 
num.zero=mat_loan[1,1]+mat_loan[1,2]  #test set���� ���� 0�� ���� 


overallacc=(mat_loan[1,1]+mat_loan[2,2])/(num.one+num.zero)
overallacc
11776/13016

#����1�� 1�� �з��� ���� ���� 
sensitivity=mat_loan[2,2]/num.one
sensitivity

#����0�� 0���� �з��� ���� ���� 
specificity=mat_loan[1,1]/num.zero
specificity

