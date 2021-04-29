#Spp Machine Learning in R - Project 5
#Classification of Telecom user's churn
##------------------------------------------------------------------------------

#Importing the data set
#-----------------------

telecom_df <- read.csv("D:\\001_Data\\Completed\\R Projects\\Project 5\\telecom_users.csv")

head(telecom_df)
tail(telecom_df)
summary(telecom_df)
dim(telecom_df)

sum(is.na(telecom_df))

#Filling NaN Values
mean_totalCharges <- mean(telecom_df$TotalCharges, na.rm = TRUE)
print(mean_totalCharges)
telecom_df$TotalCharges[is.na(telecom_df$TotalCharges)] <- mean_totalCharges

summary(telecom_df$TotalCharges)
sum(is.na(telecom_df))


#Data Visualization
#------------------

library(ggplot2)

#Count of Churn
##------------------------------------------------------------------------------
plot1 <- ggplot(telecom_df) + geom_bar(aes(x = Churn), color="red",
                                        fill="pink") 

plot1 + ggtitle("Count of the Outcome Variable") + xlab("Churn Outcome") +
  ylab("Count of the Outcome") 


#Count of Gender based on Churn 
##------------------------------------------------------------------------------
plot2 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=gender, fill=Churn))

plot2 + ggtitle("Count of the Gender based on Churn") + xlab("Gender") +
  ylab("Count of the Gender")

#Count of Senior Citizens based on Churn 
##------------------------------------------------------------------------------
plot3 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=SeniorCitizen, fill=Churn))

plot3 + ggtitle("Count of the SeniorCitizen based on Churn") + xlab("SeniorCitizen (1: YES, 0:NO)") +
  ylab("Count of the SeniorCitizen")

#Count of Customer with hasPartner based on Churn 
##------------------------------------------------------------------------------
plot4 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=Partner, fill=Churn))

plot4 + ggtitle("Count of the hasPartner based on Churn") + xlab("Partner") +
  ylab("Count of the Partner")

#Count of Customer with hasDependent based on Churn 
##------------------------------------------------------------------------------
plot5 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=Dependents, fill=Churn))

plot5 + ggtitle("Count of the hasDependent based on Churn") + xlab("Dependent") +
  ylab("Count of the Dependent")

#Distribution of tenure 
##------------------------------------------------------------------------------
plot6 <- ggplot(telecom_df) + geom_histogram(aes(x = tenure), color="red",
                                              fill="pink", bins = 70)
plot6 + ggtitle("Distribution of Tenure") + xlab("Tenure") +
  ylab("Count of the Tenure")

#Count of Customer with hasPhoneService based on Churn 
##------------------------------------------------------------------------------
plot7 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=PhoneService, fill=Churn))

plot7 + ggtitle("Count of the hasPhoneService based on Churn") + xlab("PhoneService") +
  ylab("Count of the PhoneService")

#Count of Customer with hasMultipleLines based on Churn 
##------------------------------------------------------------------------------
plot8 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=MultipleLines, fill=Churn))

plot8 + ggtitle("Count of the hasMultipleLines based on Churn") + xlab("MultipleLines") +
  ylab("Count of the MultipleLines")

#Count of Customer with hasInternetService based on Churn 
##------------------------------------------------------------------------------
plot9 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=InternetService, fill=Churn))

plot9 + ggtitle("Count of the hasInternetService based on Churn") + xlab("InternetService") +
  ylab("Count of the InternetService")

#Count of Customer with hasOnlineSecurity based on Churn 
##------------------------------------------------------------------------------
plot10 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=OnlineSecurity, fill=Churn))

plot10 + ggtitle("Count of the hasOnlineSecurity based on Churn") + xlab("OnlineSecurity") +
  ylab("Count of the OnlineSecurity")

#Count of Customer with hasOnlineBackup based on Churn 
##------------------------------------------------------------------------------
plot11 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=OnlineBackup, fill=Churn))

plot11 + ggtitle("Count of the hasOnlineBackup based on Churn") + xlab("OnlineBackup") +
  ylab("Count of the OnlineBackup")

#Count of Customer with hasDeviceProtection based on Churn 
##------------------------------------------------------------------------------
plot12 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=DeviceProtection, fill=Churn))

plot12 + ggtitle("Count of the hasDeviceProtection based on Churn") + xlab("DeviceProtection") +
  ylab("Count of the DeviceProtection")

#Count of Customer with hasTechSupport based on Churn 
##------------------------------------------------------------------------------
plot13 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=TechSupport, fill=Churn))

plot13 + ggtitle("Count of the hasTechSupport based on Churn") + xlab("TechSupport") +
  ylab("Count of the TechSupport")

#Count of Customer with hasStreamingTV based on Churn 
##------------------------------------------------------------------------------
plot14 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=StreamingTV, fill=Churn))

plot14 + ggtitle("Count of the hasStreamingTV based on Churn") + xlab("StreamingTV") +
  ylab("Count of the StreamingTV")

#Count of Customer with hasStreamingMovies based on Churn 
##------------------------------------------------------------------------------
plot15 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=StreamingMovies, fill=Churn))

plot15 + ggtitle("Count of the hasStreamingMovies based on Churn") + xlab("StreamingMovies") +
  ylab("Count of the StreamingMovies")

#Count of Contract
##------------------------------------------------------------------------------
plot16 <- ggplot(telecom_df) + geom_bar(aes(x = Contract), color="red",
                                       fill="pink") 

plot16 + ggtitle("Count of the Contracts") + xlab("Contracts") +
  ylab("Count of the Contracts") 

#Count of Contract based on Churn 
##------------------------------------------------------------------------------
plot17 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=Contract, fill=Churn))

plot17 + ggtitle("Count of the Contracts based on Churn") + xlab("Contracts") +
  ylab("Count of the Contracts")

#Count of Customers with hasPaperlessBilling based on Churn 
##------------------------------------------------------------------------------
plot18 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=PaperlessBilling, fill=Churn))

plot18 + ggtitle("Count of the hasPaperlessBilling based on Churn") + xlab("PaperlessBilling") +
  ylab("Count of the PaperlessBilling")

#Count of PaymentMethod based on Churn 
##------------------------------------------------------------------------------
plot19 <- ggplot(telecom_df) +
  geom_histogram(stat = "count",  aes(x=PaymentMethod, fill=Churn))

plot19 + ggtitle("Count of PaymentMethod based on Churn") + xlab("PaymentMethod") +
  ylab("Count of the PaymentMethod")

#Distribution of tenure 
##------------------------------------------------------------------------------
plot20 <- ggplot(telecom_df) + geom_histogram(aes(x = MonthlyCharges), color="red",
                                             fill="pink", bins = 70)
plot20 + ggtitle("Distribution of MonthlyCharges") + xlab("MonthlyCharges") +
  ylab("Count of the MonthlyCharges")

#Distribution of TotalCharges 
##------------------------------------------------------------------------------
plot21 <- ggplot(telecom_df) + geom_histogram(aes(x = TotalCharges), color="red",
                                              fill="pink", bins = 70)
plot21 + ggtitle("Distribution of TotalCharges") + xlab("TotalCharges") +
  ylab("Count of the TotalCharges")

#Distribution of tenure 
##------------------------------------------------------------------------------
plot22 <- ggplot(telecom_df) + geom_histogram(aes(x = MonthlyCharges), color="red",
                                              fill= Churn, bins = 70)
plot22 + ggtitle("Distribution of MonthlyCharges based on Churn") + xlab("MonthlyCharges") +
  ylab("Count of the MonthlyCharges")

#Distribution of TotalCharges 
##------------------------------------------------------------------------------
plot23 <- ggplot(telecom_df) + geom_histogram(aes(x = TotalCharges), color="red",
                                              fill= Churn, bins = 70)

plot23 + ggtitle("Distribution of TotalCharges based on Churn") + xlab("TotalCharges") +
  ylab("Count of the TotalCharges")

##------------------------------------------------------------------------------

library(dplyr) ## Used to split/ filter the data set

#Separating the data set into two subsets
#Yes = Churn
#No = Retention

yesChurn <- filter(telecom_df, Churn == "Yes")
noChurn <- filter(telecom_df, Churn == "No")

print(dim(yesChurn))
print(dim(noChurn))

# Comparing Churn based on Monthly Charges.
par(mfrow=c(1,2))

boxplot(yesChurn$MonthlyCharges, border = 'red', col='pink',
        main="Churn & Monthly Charges", ylab='Monthly Charges')

boxplot(noChurn$MonthlyCharges, border = 'red', col='pink',
        main="Retention & Monthly Charges", ylab='Monthly Charges')

par(mfrow=c(1,1))    

# Comparing Churn based on Total Charges.
par(mfrow=c(1,2))

boxplot(yesChurn$TotalCharges, border = 'red', col='pink',
        main="Churn & Total Charges", ylab='Total Charges')

boxplot(noChurn$TotalCharges, border = 'red', col='pink',
        main="Retention & Total Charges", ylab='Total Charges')

par(mfrow=c(1,1))   


## Data Pre-Processing
##--------------------

#Converting Categorical Vars to Numerical Vars using ifelse function
##------------------------------------------------------------------------------

telecom_df$NumGender <- ifelse(telecom_df$gender == "Male", 1, 0)
telecom_df$NumPartner <- ifelse(telecom_df$Partner == "Yes", 1, 0)
telecom_df$NumDependent <- ifelse(telecom_df$Dependents == "Yes", 1, 0)
telecom_df$NumPhoneService <- ifelse(telecom_df$PhoneService == "Yes", 1, 0)
telecom_df$NumPaperlessBilling <- ifelse(telecom_df$PaperlessBilling == "Yes", 1, 0)
telecom_df$NumChurn <- ifelse(telecom_df$Churn == "Yes", 1, 0)

#Converting Categorical Vars to Numerical Vars using dummies package
##------------------------------------------------------------------------------

install.packages("dummies")
library(dummies)

t(t(names(telecom_df)))

dummy_vars <- telecom_df[c("MultipleLines",   
                           "InternetService", 
                           "OnlineSecurity",  
                           "OnlineBackup",    
                           "DeviceProtection",
                           "TechSupport",     "StreamingTV",     
                           "StreamingMovies", "Contract",  "PaymentMethod")]

head(dummy_vars)

dummies_telecom <- dummy.data.frame(dummy_vars, sep="-")
print(dummies_telecom)

# Merging Dataframes
##------------------------------------------------------------------------------

final_telecom_df <- dummies_telecom

final_telecom_df$NumGender <- telecom_df$NumGender
final_telecom_df$NumPartner <- telecom_df$NumPartner
final_telecom_df$NumDependent <- telecom_df$NumDependent
final_telecom_df$NumPhoneService <- telecom_df$NumPhoneService
final_telecom_df$NumPaperlessBilling <- telecom_df$NumPaperlessBilling
final_telecom_df$NumChurn <- telecom_df$NumChurn
final_telecom_df$SeniorCitizens <- telecom_df$SeniorCitizen
final_telecom_df$MCharge <- telecom_df$MonthlyCharges
final_telecom_df$TCharge <- telecom_df$TotalCharges

##PARTITIONING THE DATA SET
##-------------------------
train.rows <- sample(rownames(final_telecom_df), dim(final_telecom_df)[1]*0.6)
train.df <- final_telecom_df[train.rows, ]

validate.rows <- sample(setdiff(rownames(final_telecom_df), train.rows))
validate.df <- final_telecom_df[validate.rows, ]

print(dim(train.df))
print(dim(validate.df))

##BUILDING THE ML MODEL
##---------------------

library(rpart)

dtree <- rpart(NumChurn ~ ., method = 'class', data=train.df)

plot(dtree, uniform = TRUE, 
     main = "Telecom Churn - Decision  
                 Tree Classifier")

text(dtree, use.n = TRUE, cex = .7)

churn.predictions = predict(dtree, validate.df, type = 'class')


##MODEL EVALUATION
##---------------------

# Confusion Matrix

print("Confusion Matrix of the Decision Tree Model: ")
print("============================================")
table(validate.df$NumChurn, churn.predictions)

cm <- table(validate.df$NumChurn, churn.predictions)

# Accuracy

dtree_accuracy <- sum(diag(cm)) / sum(cm)
print(paste('Accuracy of Decision Tree Model: ', dtree_accuracy))
