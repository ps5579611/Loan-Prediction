 df <- read.csv("train_u6lujuX_CVtuZ9i.csv")
table(df$Loan_Status)

df1 <- read.csv("test_Y3wMUE5_7gLdaTN.csv")
df1$Loan_Status <- "Y"

df1$Loan_Status <- "Y"

com <- rbind(df, df1)

com <- as.data.frame(com)

any(is.na(com$ApplicantIncome))
any(is.na(com$CoapplicantIncome))
any(is.na(com$LoanAmount))
any(is.na(com$Loan_Amount_Term))
any(is.na(com$Credit_History))
any(is.na(com$Dependents))
any(is.na(com$Married))
any(is.na(com$Self_Employed))
any(is.na(com$Gender))
any(is.na(com$CoapplicantIncome))

library(ggplot2)
library(Amelia)
missmap(com,main = 'Post cleaning of data',col= c('yellow','black'),legend = FALSE)

### Credit history,Loan Amount and Amount term has NA values####################################



ggplot(com,aes(Loan_Status,LoanAmount))+geom_boxplot(aes(group=Loan_Status,fill=factor(Loan_Status)))



#ggplot(com,aes(com$CoapplicantIncome,com$LoanAmount))+geom_point()
library(ggplot2)
ggplot(com,aes(ApplicantIncome))+geom_histogram(fill='blue',color='black')
#### Most of the Applicant income is between 2000-6000 #######################################
ggplot(com,aes(Gender,ApplicantIncome))+geom_boxplot(aes(fill=factor(Gender)))

ggplot(com,aes(LoanAmount))+geom_histogram(fill='blue',color='black')
####LOan Amount is distributed in the range of 60-200k#############
x1 <- ggplot(com,aes(Credit_History))+geom_bar(col='black',fill='blue')
ggplotly(x1)
############## Loan status on the basis of Gender..Male has most number of approval with few missing values###########
ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant")

ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant")

print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
#print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))

print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))

##difficult to see any patterns, most of the loans are for 360 months
print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))

#it's easiest to get a loan if the property is semi urban and hardest if it is rural
print(ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))

#doesn't look like there's much difference
print(ggplot(com, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))

#this seems to make a difference

print(ggplot(com, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))

#the third quartile of the refused loans is higher
print(ggplot(com, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))

print(ggplot(com,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married))

library(ggplot2)
#Applicants with higher than 20000 income have been truncated from the plot
print(ggplot(data=com[com$ApplicantIncome<20000,],aes(ApplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))
print(ggplot(data=com[com$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))


prop.table(table(com$Credit_History))

table(com$Married)
##############################Loan Amount#############################################################

###Applicant Income and Loan amount is in linear fashion
### Most of the data ranges from 0-10000 so splitted in range to find Loan Amount for that income range



x <- ggplot(com,aes(ApplicantIncome,LoanAmount))+geom_point(fill='blue')
library(plotly)
ggplotly(x)

d1 <-com[(com$ApplicantIncome>0 & com$ApplicantIncome<2000),]
m1<-mean(d1$LoanAmount,na.rm = TRUE)
round(m1)

d2 <-com[(com$ApplicantIncome>=2000 & com$ApplicantIncome<4000),]
m2<-mean(d2$LoanAmount,na.rm = TRUE)
round(m2)

d3 <-com[(com$ApplicantIncome>=4000 & com$ApplicantIncome<6000),]
m3<-mean(d3$LoanAmount,na.rm = TRUE)
round(m3)

d4 <-com[(com$ApplicantIncome>=6000 & com$ApplicantIncome<8000),]
m4<-mean(d4$LoanAmount,na.rm = TRUE)
round(m4)

d5 <-com[(com$ApplicantIncome>=8000 & com$ApplicantIncome<10000),]
m5<-mean(d5$LoanAmount,na.rm = TRUE)
round(m5)

d6 <-com[(com$ApplicantIncome>=10000),]
m6<-mean(d6$LoanAmount,na.rm = TRUE)
round(m6)

### Impute loan amount based on Applicant Inccome
impute_Loan <- function(LoanAmount,App_income){
  out <- LoanAmount
  for(i in 1:length(LoanAmount)){
    if(is.na(LoanAmount[i])){
      
      if(App_income[i]>0 & App_income[i]<2000){
        out[i] <- round(m1)
      }else if (App_income[i]>=2000 & App_income[i]<4000){
        out[i] <- round(m2)
      }else if (App_income[i]>=4000 & App_income[i]<6000){
        out[i] <- round(m3)
      }else if (App_income[i]>=6000 & App_income[i]<8000){
        out[i] <- round(m4)
      }else if (App_income[i]>=8000 & App_income[i]<10000){
        out[i] <- round(m5)
      }else if (App_income[i]>=10000 ){
        out[i] <- round(m6)
      }
      
    }else {
      out[i] <- LoanAmount[i]
    }
  }
  return(out)
  
}
new_m <- impute_Loan(com$LoanAmount,com$ApplicantIncome)
com$LoanAmount <- new_m 

#############################Loan Amount Term#################################################################
ggplot(com,aes(com$Credit_History,com$Loan_Amount_Term))+geom_point()
table(com$Loan_Amount_Term)
###MOst of the values are 360
ggplot(com, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant")
### Function to find mode of the value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

x1 <- Mode(com$Loan_Amount_Term)

com$Loan_Amount_Term[is.na(com$Loan_Amount_Term)]<-x1
any(is.na(com$Loan_Amount_Term))



######################### Credit History###########################################################
any(is.na(com$Credit_History))
table(com$Credit_History)



ggplot(com,aes(com$Credit_History,com$ApplicantIncome))+geom_boxplot(aes(factor(com$Credit_History)))+scale_y_continuous(limits = c(2400,4200))

x2<- subset(com$ApplicantIncome,com$Credit_History==1)
x2 <-mean(x2,trim = 0.2)
x2
x3<- subset(com$ApplicantIncome,com$Credit_History==0)
x3<- mean(x3,trim = 0.2)
x3

x <- (x2+x3)/2
### Impute credit History based on Applicant Income by selecting the range
impute_credit <- function(credit_his,App_income){
  out <- credit_his
  for(i in 1:length(credit_his)){
    if(is.na(credit_his[i])){
      
      if(App_income[i]<=x){
        out[i] <- 0
      }else if (App_income[i]>x){
        out[i] <- 1
      }
      
      
    }else {
      out[i] <- credit_his[i]
    }
  }
  return(out)
  
}
new_m <- impute_credit(com$Credit_History,com$ApplicantIncome)
com$Credit_History <- new_m 

table(com$Credit_History)

##############################Gender#################################
ggplot(com,aes(com$Gender,com$ApplicantIncome))+geom_boxplot(aes(factor(com$Gender)))+scale_y_continuous(limits = c(2400,4200))
x <- subset(com$ApplicantIncome,com$Gender=='Male')
x <- mean(x,trim=0.2)

y <- subset(com$ApplicantIncome,com$Gender=='Female')
y <- mean(y,trim = 0.2)

any(is.na(com$Gender))

impute_gender <- function(gender,App_income){
  out <- gender
  for(i in 1:length(gender)){
    if(gender[i]==""){
      if(App_income[i]<y){
        out[i] <- 'Female'
      }else if (App_income[i]>=x){
        out[i] <- 'Male'
      }else{
        out[i]<-'Male'
      }
      
      
    }else {
      out[i] <- gender[i]
    }
  }
  return(out)
  
}
com$Gender <- impute_gender(com$Gender,com$ApplicantIncome)
table(com$Gender)

##############marriage#################################################

#### Impute MArriage based on Location

m <- subset(com$ApplicantIncome,com$Married=="Yes")
mean(m,trim = 0.2)

m1 <- subset(com$ApplicantIncome,com$Married=="No")
m1 <-mean(m1,trim = 0.2)

impute_marriage <- function(marriage,App_Income){
  for (i in 1:length(marriage)){
    
    if (marriage[i]==''){
      
      if (App_Income[i] >=m1) {
        marriage[i] <- 'Yes'
      }else{
        marriage[i] <- 'No'
      }
      
      
    }
    
  }
  return(marriage)
}


com$Married <- impute_marriage(com$Married,com$ApplicantIncome)
com$Married
table(com$Married)

#############################Dependent ##################################################

z0<- subset(com$ApplicantIncome,(com$Married=='Yes'& com$Dependents==0))
mean(z0,trim = 0.2)
min(z0)
z1 <- subset(com$ApplicantIncome,(com$Married=='Yes'& com$Dependents==1))
mean(z1,trim = 0.2)

z2 <- subset(com$ApplicantIncome,(com$Married=='Yes'& com$Dependents==2))
mean(z2,trim = 0.2)

z3 <- subset(com$ApplicantIncome,(com$Married=='Yes'& com$Dependents=='3+'))
mean(z3,trim = 0.2)



a0<- subset(com$ApplicantIncome,(com$Married=='No'& com$Dependents==0))
mean(a0,trim = 0.2)

a1 <- subset(com$ApplicantIncome,(com$Married=='No'& com$Dependents==1))
mean(a1,trim = 0.2)

a2 <- subset(com$ApplicantIncome,(com$Married=='No'& com$Dependents==2))
mean(a2,trim = 0.2)

a3 <- subset(com$ApplicantIncome,(com$Married=='No'& com$Dependents=='3+'))
mean(a3,trim = 0.2)

impute_dependent <- function(Married,App_income,Dependent){
  out <- Dependent
  for(i in 1:length(Dependent)){
    if(Dependent[i]==""){
      
      if(Married[i]=='Yes' & App_income[i]<3900){
        out[i] <- 0
      }else if (Married[i]=='Yes' & (App_income[i]>=3900&App_income[i]<4200)){
        out[i] <- 1
      }else if (Married[i]=='Yes' & (App_income[i]>=4200&App_income[i]<4500)){
        out[i] <- 2
      }else if (Married[i]=='Yes' & App_income[i]>=4200){
        out[i] <- '3+'
      }else if (Married[i]=='No' & (App_income[i]<=3600)){
        out[i] <- 0
      }else if (Married[i]=='No' & (App_income[i]>=3600&App_income[i]<3900)){
        out[i] <- 2
      }else if (Married[i]=='Yes' & (App_income[i]>=3900&App_income[i]<4200)){
        out[i] <- '3+'
      }else if (Married[i]=='Yes' & App_income[i]>=4200){
        out[i] <- 1
      }else{
        out[i] <- 0
      }
    }else {
      out[i] <- Dependent[i]
    }
  }
  return(out)
  
}


com$Dependents <- impute_dependent(com$Married,com$ApplicantIncome,com$Dependents)
table(com$Dependents)

class(com$Dependents)


##############################Employee Status###########################################


impute_empstatus <- function(emp_status){
  for (i in 1:length(emp_status)){
    if(emp_status[i]==""){
      emp_status[i] <- "No"
    }else{
      emp_status[i] <- emp_status[i]
    }
    
  }
  return(emp_status)
}
com$Self_Employed <- impute_empstatus(com$Self_Employed)
table(com$Self_Employed)

any(is.na(com$Self_Employed))

missmap(com,main = 'Main',col= c('yellow','black'),legend = FALSE)


train <- com[1:614,]
test <- com[615:981,-13]

# train[ , 7:11] <- scale(train[ , 7:11])
# test[ , 7:11] <- scale(test[ , 7:11])

log.model<- glm(formula = Loan_Status ~ .,family = binomial(link = 'logit'),data = train[,-1])
summary(log.model)


df1$Loan_Status = predict(log.model, type="response",newdata = test)
fitted.results <- ifelse(df1$Loan_Status > 0.5,1,0)

print(fitted.results)

x<- data.frame(test$Loan_ID,fitted.results)
colnames(x) <- c("Loan_ID","Loan_Status")
x$Loan_Status<-recode(x$Loan_Status,"1='Y'; else='N'")


write.csv(x,"C:/Users/patel/Desktop/project/sample_submission.csv",row.names = F)
###################################K-nn############################################
library(randomForest)
y_pred <- randomForest(x=train[,c(-1,-13)],y=train$Loan_Status,ntree = 10)

fitted.results <- predict(y_pred,newdata = test[-1])


x<- data.frame(test$Loan_ID,fitted.results)
colnames(x) <- c("Loan_ID","Loan_Status")
x$Loan_Status<-recode(x$Loan_Status,"1='Y'; else='N'")















