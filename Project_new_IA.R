 df <- read.csv("train_u6lujuX_CVtuZ9i.csv")
 df1 <- read.csv("test_Y3wMUE5_7gLdaTN.csv")
 
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
 missmap(com,main = 'Main',col= c('yellow','black'),legend = FALSE)
 
 
 ggplot(com,aes(com$Loan_Status,com$LoanAmount))+geom_boxplot(aes(group=com$Loan_Status,fill=factor(com$Loan_Status)))
 
 ggplot(com,aes(com$ApplicantIncome,com$LoanAmount))+geom_point()
 
 #ggplot(com,aes(com$CoapplicantIncome,com$LoanAmount))+geom_point()
 

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
 
 
com
ggplot(com,aes(com$Credit_History,com$Loan_Amount_Term))+geom_point()
table(com$Loan_Amount_Term)
com$Loan_Amount_Term[is.na(com$Loan_Amount_Term)]<-360
com$Loan_Amount_Term
com
df <- com

any(is.na(com$Credit_History))
table(com$Credit_History)



ggplot(com,aes(com$Credit_History,com$ApplicantIncome))+geom_boxplot(aes(factor(com$Credit_History)))+scale_y_continuous(limits = c(2400,4200))


log.model <- glm(com$Credit_History~com$ApplicantIncome+com$CoapplicantIncome+com$Education+com$Self_Employed+com$Married,family = binomial(link = 'logit'),data = com)
summary(log.model)






impute_credit <- function(credit_his,App_income){
  out <- credit_his
  for(i in 1:length(credit_his)){
    if(is.na(credit_his[i])){
      
      if(App_income[i]<3050){
        out[i] <- 0
      }else if (App_income[i]>3050){
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



impute_gender <- function(gender){
  for (i in 1:length(gender)){
    if(gender[i]==""){
      gender[i] <- "Male"
    }else{
      gender[i] <- gender[i]
    }
    
  }
  return(gender)
}
com$Gender <- impute_gender(com$Gender)


##############marriage#######

impute_status <- function(status){
  for (i in 1:length(status)){
    if(status[i]==""){
      status[i] <- "Yes"
    }else{
      status[i] <- status[i]
    }
    
  }
  return(status)
}
com$Married <- impute_status(com$Married)




impute_marriage <- function(a,loc){
  for (i in 1:length(a)){
    
    if (is.na(a[i])){
      
      if (loc[i] == "Urban"){
        a[i] <- 'Yes'
      }else{
        a[i] <- 'No'
      }
    }  
    
  }
  return(a)
}

com$Married <- impute_marriage(com$Married,com$Property_Area)



impute_dependent <- function(b){
  for (i in 1:length(b)){
    if (b[i] == ""){
      b[i] <- 0
    }else{
      b[i] <- b[i]
    }
    
  }
  return(b)
}

com$Dependents <- impute_dependent(com$Dependents)

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

df <- com


























