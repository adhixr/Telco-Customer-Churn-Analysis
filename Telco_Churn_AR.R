#TELCO CHURN PROJECT_ ADHITHYAN R

library(readxl)
setwd("C:/AR Files/USF/Projects/Churn")
d <-read_excel("TelcoChurn.xlsx", sheet="Data")
str(d)
d2 <- d
str(d2)


#plot(churn ~ tenure, data=d2)
#dropping columns that are not required for analysis of any of the three predictors
d2$customerID <- NULL
d2$Partner <- NULL
d2$Dependents <- NULL
d2$TechSupport <- NULL
d2$PaperlessBilling <- NULL
d2$TotalCharges <- NULL

colnames(d2) <- tolower(colnames(d2))
str(d2)

#we need to factorize and relevel the character type columns

d2$gender <-factor(d2$gender)
d2$gender <-relevel(d2$gender,"Female")

d2$contract <-factor(d2$contract)
d2$contract <-relevel(d2$contract,"Month-to-month")

d2$paymentmethod <-factor(d2$paymentmethod)
d2$paymentmethod <-relevel(d2$paymentmethod,"Mailed check")

d2$internetservice <-factor(d2$internetservice)
d2$internetservice <-relevel(d2$internetservice,"DSL")

d2$churn <-factor(d2$churn)
d2$churn <-relevel(d2$churn,"No")

#convert "yes and no" type columns to 0's and 1's for our analysis- more useful 

d2$phoneservice <- ifelse(d2$phoneservice=="Yes",1,0)
d2$multiplelines <- ifelse(d2$multiplelines=="Yes",1,0)   
d2$onlinesecurity <- ifelse(d2$onlinesecurity=="Yes",1,0)
d2$onlinebackup <- ifelse(d2$onlinebackup=="Yes",1,0)
d2$deviceprotection <- ifelse(d2$deviceprotection=="Yes",1,0)
d2$streamingtv <- ifelse(d2$streamingtv=="Yes",1,0)
d2$streamingmovies <- ifelse(d2$streamingmovies=="Yes",1,0)
d2$seniorcitizen <- ifelse(d2$seniorcitizen=="Yes",1,0)

#we need to create three data partitions for phone users, internet users, and users of both services

#Reference - https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

#phone - 1526 customers
library(tidyverse)
phonecust <- d2
phonecust2= (phonecust %>% filter(phoneservice=='1', internetservice=="No"))
View(phonecust2)
str(phonecust2)

#internet - 682 customers

internetcust <- d2
internetcust2= (internetcust %>% filter(phoneservice=='0', internetservice=="DSL" | internetservice=="Fiber optic"))
View(internetcust2)
str(internetcust2)


#both services - 4835 customers

bothcust <- d2
bothcust2= (bothcust %>% filter(phoneservice=='1', internetservice=="DSL" | internetservice=="Fiber optic"))
View(bothcust2)
str(bothcust2)

#overall view of how many customers churned in these datasets

table(phonecust2$churn) #113 yes /1413 no
table(internetcust2$churn) #170 yes/ 512 no
table(bothcust2$churn) #1526 yes / 3249 no

#check null rows
colSums(is.na(phonecust2))
colSums(is.na(internetcust2))
colSums(is.na(bothcust2))
# no null rows, clear coast


#EDA and Vizzes to understand data a little better
d3 <- d2


d3$churn <- ifelse(d2$churn=="Yes",1,0)

str(d3)

library(ggplot2)
ggplot(d3, aes(x=tenure, fill=churn)) +
  geom_density(alpha = 0.3) +
  ggtitle("Tenure vs Churn")
geom_smooth(color="red")

#Performance Analytics 
d3 <- d3[,-c(1,6,12,13)]
d3 <- d3[,-c(9,10)]
str(d3)
library(corrplot)
m <- cbind(d3)
cor(m)
corrplot(cor(m), method="number")                
pairs(d3)                                        
library("PerformanceAnalytics")
chart.Correlation(m)

ggplot(d2, aes(x=monthlycharges, fill=churn)) +
  geom_density(alpha = 0.6) +
  ggtitle("Monthly Charges vs Churn")
geom_smooth(color="red")

ggplot(d2, aes(x=tenure, fill=churn)) +
  geom_density(alpha = 0.6) +
  ggtitle("Tenure vs Churn")
geom_smooth(color="red")


#train test split 75-25 for the three Y variables using class code from prof

set.seed(1024)
trainIndex <- sample(1:nrow(phonecust2), size=round(0.75*nrow(phonecust2)), replace=FALSE)
trainphone <- phonecust2[trainIndex,]
testphone  <- phonecust2[-trainIndex,]


set.seed(1024)
trainIndex2 <- sample(1:nrow(internetcust2), size=round(0.75*nrow(internetcust2)), replace=FALSE)
train_internet <- internetcust2[trainIndex2,]
test_internet  <- internetcust2[-trainIndex2,]


set.seed(1024)
trainIndex3 <- sample(1:nrow(bothcust2), size=round(0.75*nrow(bothcust2)), replace=FALSE)
trainboth <- bothcust2[trainIndex3,]
testboth  <- bothcust2[-trainIndex3,]


str(phonecust2) 
str(internetcust2)
str(bothcust2)
str(trainphone)#1144 rows
str(testphone) #382 rows
str(train_internet) #512 rows
str(test_internet)#170 rows
str(trainboth)#3626 rows
str(testboth) #1209 rows

colnames(trainphone)

#logit Models on churn for the 3 Y Variables

phone <- glm(churn ~ gender+ seniorcitizen+tenure+multiplelines+ contract+paymentmethod+
               monthlycharges, family=binomial (link="logit"), data=trainphone)

net <-glm(churn ~ gender+ tenure+onlinesecurity +onlinebackup+deviceprotection+ streamingtv+
            streamingmovies+contract+paymentmethod+ monthlycharges, family=binomial (link="logit"), data=train_internet)

both <-glm(churn ~ gender+ tenure+onlinesecurity +onlinebackup+deviceprotection+ streamingtv+
             streamingmovies+internetservice+contract+paymentmethod+ monthlycharges, family=binomial (link="logit"), data=trainboth)


library(stargazer)
stargazer(phone,net,both, type="text", single.row=TRUE)

#model metrics 

str(testphone)

testphone$churn <- ifelse(testphone$churn=="Yes",1,0)
test_internet$churn <- ifelse(test_internet$churn=="Yes",1,0)
testboth$churn <- ifelse(testboth$churn=="Yes",1,0)

#model1 

library(caret)
predicted<- predict(phone,testphone,type="response")
predictedz <-ifelse(predicted>0.4,1,0)
table(testphone$churn,predictedz)

library(MLmetrics)
recall_phone = Recall(testphone$churn, predictedz, positive = NULL)
print(recall_phone)
precision_phone = Precision(testphone$churn, predictedz, positive = NULL)
print(precision_phone)
f1_phone = F1_Score(predictedz,testphone$churn)
print(f1_phone)
auc_phone = AUC(predictedz,testphone$churn)
print(auc_phone)


print(recall_phone)
print(precision_phone)
print(f1_phone)
print(auc_phone)
print(Accuracy(predictedz,testphone$churn))
table(testphone$churn,predictedz)

#model 2 
library(caret)
predicted<- predict(net,test_internet,type="response")
predictedzz <-ifelse(predicted>0.5,1,0)
table(test_internet$churn,predictedzz)

library(MLmetrics)
recall_net = Recall(test_internet$churn, predictedzz, positive = NULL)
print(recall_net)
precision_net = Precision(test_internet$churn, predictedzz, positive = NULL)
print(precision_net)
f1_net = F1_Score(predictedzz,test_internet$churn)
print(f1_net)
auc_net = AUC(predictedzz,test_internet$churn)
print(auc_net)

print(recall_net)
print(precision_net)
print(f1_net)
print(auc_net)
print(Accuracy(predictedzz,test_internet$churn))
table(test_internet$churn,predictedzz)


#model 3 

library(caret)
predicted<- predict(both,testboth,type="response")
predictedzzz <-ifelse(predicted>0.48,1,0)
table(testboth$churn,predictedzzz)

library(MLmetrics)
recall_both = Recall(testboth$churn, predictedzzz, positive = NULL)
print(recall_both)
precision_both = Precision(testboth$churn, predictedzzz, positive = NULL)
print(precision_both)
f1_both = F1_Score(predictedzzz,testboth$churn)
print(f1_both)
auc_both = AUC(predictedzzz,testboth$churn)
print(auc_both)


print(recall_both)
print(precision_both)
print(f1_both)
print(auc_both)
print(Accuracy(predictedzzz,testboth$churn))
table(testboth$churn,predictedzzz)


