##loan prediction 

train= read.csv("train_loan_predict_analytics_vidya.csv",header = T)
test = read.csv("test_loan_predict_analytics_vidya.csv", header = T)
 
#analysing variables

hist(train$ApplicantIncome , col = "red" )
boxplot(train$ApplicantIncome , col = "green")
boxplot(train$ApplicantIncome~train$Education)
hist(train$LoanAmount)
boxplot(train$LoanAmount~train$Education)
barplot(table(train$Loan_Status , train$Gender) , col = train$Loan_Status)
legend("topleft",c("N","Y"),fill =train$Loan_Status  , cex = 0.5, pch = 20)
str(train)

# imputing missing values with mice package using cart method as this is use for prediction of all types of variables
train[train==""]= NA
install.packages("mice")
library(mice)
tempData <- mice(train,m=1,meth="cart" ,maxit=5,seed=500)

par(mfrow=c(7,7))
densityplot(tempData)
train.r = complete(tempData)

summary(train.r)

#Randon forest

d= sample(2,nrow(train.r) , replace = T , prob = c(0.7,0.3))
train1 = train.r[d==1,-1]
test1= train.r[d==2,-1]


library(randomForest)

rf = randomForest(Loan_Status~Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Property_Area,data = train1 , importance = T , proximity = T, ntree = 300 , na.action = na.roughfix)
pred = predict(rf, newdata = test1)
table( pred ,test1$Loan_Status)
mean(pred==test1$Loan_Status)

##test data

summary(test)

test[test==""]=NA
temp_test = mice(test, m=1,method = "cart",maxit = 5 , seed = 400)
test.r = complete(temp_test)
test.r$Loan_status = NA
pred1 = predict(rf , newdata = test.r)
test.r$Loan_status = pred1


#creating  final result data frame 

answer = data.frame(Loan_ID = test$Loan_ID , Loan_status = test.r$Loan_status)
write.csv(answer , file = "answer.csv" , row.names = F)
