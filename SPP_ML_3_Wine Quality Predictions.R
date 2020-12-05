#Spp Machine Learning in R - Project 3
#Predicting the quality of the wine
#---------------------------------------------------------

##IMPORTING THE DATASET
##--------------------``
wine.df <- read.csv("D:\\001_Data\\Not Completed\\Wine Quality\\winequality-red.csv")

#CHECKING THE DATASET
##-------------------
head(wine.df)
tail(wine.df)
summary(wine.df)
dim(wine.df)

sum(is.na(wine.df))

##DATA VISUALIZATION
##------------------
library(ggplot2)

plt.1 <- ggplot(data = wine.df) + geom_histogram(aes(x=quality), color="darkblue", fill="lightblue") 
plt.1 + theme_classic() + ggtitle("Histogram of wine quality") + xlab("Quality of the wine") + ylab("Count")

plt.2 <- ggplot(data = wine.df) + geom_histogram(aes(x=alcohol), color="darkblue", fill="lightblue") 
plt.2 + theme_classic() + ggtitle("Histogram of alcohol content") + xlab("Alcohol Content") + ylab("Count")

plt.3 <- ggplot(data = wine.df) + geom_histogram(aes(x=pH), color="darkblue", fill="lightblue") 
plt.3 + theme_classic() + ggtitle("Histogram of pH content") + xlab("pH Content") + ylab("Count")

plt.4 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=fixed.acidity), color="red")
plt.4 + theme_classic() + ggtitle("Quality of the wine & Fixed Acidity") + 
  xlab("Quality of the wine") + ylab("Fixed Acidity")

plt.5 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=volatile.acidity), color="lightblue")
plt.5 + theme_classic() + ggtitle("Quality of the wine & Volatile Acidity") + 
  xlab("Quality of the wine") + ylab("Volatile Acidity")

plt.6 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=residual.sugar), color="darkblue")
plt.6 + theme_classic() + ggtitle("Quality of the wine & Residual Sugar") + 
  xlab("Quality of the wine") + ylab("Residual Sugar")

plt.7 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=chlorides), color="darkblue")
plt.7 + theme_classic() + ggtitle("Quality of the wine & Chlorides") + 
  xlab("Quality of the wine") + ylab("Chlorides")

plt.8 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=free.sulfur.dioxide), color="yellow")
plt.8 + theme_classic() + ggtitle("Quality of the wine & Free SO2") + 
  xlab("Quality of the wine") + ylab("Free SO2")

plt.9 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=total.sulfur.dioxide), color="yellow")
plt.9 + theme_classic() + ggtitle("Quality of the wine & Total SO2") + 
  xlab("Quality of the wine") + ylab("Total SO2")

plt.10 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=density), color="blue")
plt.10 + theme_classic() + ggtitle("Quality of the wine & Density") + 
  xlab("Quality of the wine") + ylab("Density")

plt.11 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=pH), color="blue")
plt.11 + theme_classic() + ggtitle("Quality of the wine & pH") + 
  xlab("Quality of the wine") + ylab("pH")

plt.12 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=sulphates), color="blue")
plt.12 + theme_classic() + ggtitle("Quality of the wine & sulphates") + 
  xlab("Quality of the wine") + ylab("sulphates")

plt.11 <- ggplot(data=wine.df) + geom_point(aes(x=quality, y=alcohol), color="blue")
plt.11 + theme_classic() + ggtitle("Quality of the wine & Alcohol Content") + 
  xlab("Quality of the wine") + ylab("Alcohol Content")

##PARTITIONING THE DATA SET
##-------------------------
train.rows <- sample(rownames(wine.df), dim(wine.df)[1]*0.6)
train.df <- wine.df[train.rows, ]

validate.rows <- sample(setdiff(rownames(wine.df), train.rows))
validate.df <- wine.df[validate.rows, ]

##BIULDING A DECISION TREE REGRESSOR MODEL
##-----------------------------------------
library(rpart)

dTree.model <- rpart(quality ~ ., method = "anova", data = wine.df)

plot(dTree.model, uniform = TRUE, 
     main = "Wine Quality - Decision  
                 Tree Classifier") 
text(dTree.model, use.n = TRUE, cex = .7)

wine.predictions = predict(dTree.model, validate.df)

library(forecast)
accuracy(round(wine.predictions), validate.df$quality)

all.residuals <- validate.df$quality[1:20] - round(wine.predictions[1:20])
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
data.frame("Pred" = round(wine.predictions[1:20]), "Actual" = validate.df$quality[1:20])

summary(dTree.model)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
      