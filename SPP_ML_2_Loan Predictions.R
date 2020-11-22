#Spp Machine Learning in R - Project 
#Predicting whether a loan application is SuCCESS OR NOT
#--------------------------------------------------------

loan.df <- read.csv("D:\\001_Data\\Not Completed\\1R -Loan Prediction Dataset\\Loan Prediction Dataset.csv")
loan.df <- loan.df[-1] #Dropping the loanID column 


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#1) EXPLORATORY DATA ANALYSIS
#-------------------------
head(loan.df)
tail(loan.df)
summary(loan.df) #LoanAmount,Loan_Amount_Term,Credit_History has null values
dim(loan.df)
t(t(names(loan.df)))

#1.1)Checking unique values of every 'character' columns
unique(loan.df$Gender)
unique(loan.df$Married)
unique(loan.df$Dependents)
unique(loan.df$Education)
unique(loan.df$Self_Employed)
unique(loan.df$Property_Area)
unique(loan.df$Loan_Status)

#1.2)Manipulating the data
loan.df$Dependents <- as.numeric(as.character(loan.df$Dependents))
loan.df[is.na(loan.df$Dependents), 3] <- 3

loan.df$Gender <- ifelse(loan.df$Gender == 'Male', 1, 0)
loan.df$Married <- ifelse(loan.df$Married == 'Yes', 1, 0)
loan.df$Education <- ifelse(loan.df$Education == 'Graduate', 1, 0)
loan.df$Self_Employed <- ifelse(loan.df$Self_Employed == 'Yes', 1, 0)
loan.df$Loan_Status <- ifelse(loan.df$Loan_Status == "Y", 1, 0)

loan.df[loan.df$Property_Area == 'Rural', 11] <- 0
loan.df[loan.df$Property_Area == 'Semiurban', 11] <- 1
loan.df[loan.df$Property_Area == 'Urban', 11] <- 2
loan.df$Property_Area <- as.numeric(as.character(loan.df$Property_Area))

#Confirming the changed values
unique(loan.df$Gender)
unique(loan.df$Married)
unique(loan.df$Dependents)
unique(loan.df$Education)
unique(loan.df$Self_Employed)
unique(loan.df$Property_Area)
unique(loan.df$Loan_Status)

library(Amelia)
missmap(obj = loan.df, legend = FALSE, main = "Missing values heatmap", 
        col = c("yellow", "black"))

#Filling null values with the median value of that columns
fill.loan.Amt <- median(loan.df$LoanAmount, na.rm = T)
loan.df[is.na(loan.df$LoanAmount),8] <- fill.loan.Amt

fill.loan.Amt.Term <- median(loan.df$Loan_Amount_Term, na.rm = T)
loan.df[is.na(loan.df$Loan_Amount_Term),9] <- fill.loan.Amt.Term 

fill.credit.history <- median(loan.df$Credit_History, na.rm = T)
loan.df[is.na(loan.df$Credit_History),10] <- fill.credit.history

#Checking the heatmap again
missmap(obj = loan.df, legend = FALSE, main = "Missing values heatmap", 
        col = c("yellow", "black")) #NULL Missing values :)

#1.3)Data Visulaization
#----------------------
library(ggplot2)

applicant.income <- ggplot(loan.df, aes(x=ApplicantIncome))
plot.1 <- applicant.income + geom_histogram(color='red', fill='pink') 
plot.1 + ggtitle("Main Applicants Income Distribution") + xlab("Applicant's Income")+ 
  ylab("Count")

co.applicant.income <- ggplot(loan.df, aes(x=CoapplicantIncome))
plot.2 <- co.applicant.income + geom_histogram(color='red', fill='pink') 
plot.2 + ggtitle("Co-Applicants Income Distribution") + xlab("Co-Applicant's Income")+ 
  ylab("Count")

education.count <- ggplot(loan.df, aes(x=Education))
plot.3 <- education.count + geom_bar(color='red', fill='pink')
plot.3 + ggtitle("Count of Grads and Non-Grads") + xlab("Graduation Status")+ 
  ylab("Count") + scale_x_discrete(limits=c(0,1))

prop.area.count <- ggplot(loan.df, aes(x=Property_Area))
plot.4 <- prop.area.count + geom_bar(color='red', fill='pink')
plot.4 + ggtitle("Count of Property Area") + xlab("Property Area")+ 
  ylab("Count") 

loan.status.count <- ggplot(loan.df, aes(x=Loan_Status))
plot.5 <- prop.area.count + geom_bar(color='red', fill='pink')
plot.5 + ggtitle("Count of Loan Status") + xlab("Loan Status")+ 
  ylab("Count") 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#2) Building Logistic Regression Model
loan.train.rows <- sample(rownames(loan.df), 
                           dim(loan.df)[1]*0.7)
loan.train.df <- loan.df[loan.train.rows, ]

loan.validate.rows <- sample(setdiff(rownames(loan.df), 
                                      loan.train.rows))
loan.validate.df <- loan.df[loan.validate.rows, ]

logistic.loan <- glm(Loan_Status ~ ., data = loan.train.df, family = binomial("logit"))

summary(logistic.loan)

loan.approval.predictions <- predict(logistic.loan, loan.validate.df)             
prediction.results <- ifelse(loan.approval.predictions > 0.5, 1, 0)
misClassificationRate <- mean(prediction.results != loan.validate.df$Loan_Status)
print((1 - misClassificationRate) * 100)
table(loan.validate.df$Loan_Status, loan.approval.predictions>0.5)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
