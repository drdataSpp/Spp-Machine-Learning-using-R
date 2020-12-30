#-------------------------------------------------------------------------------
#IMPORTING THE DATASET AND CHECKING IT
#-------------------------------------------------------------------------------

candy.df <- read.csv("D:\\001_Data\\Not Completed\\candy-data.csv")

head(candy.df)
tail(candy.df)
dim(candy.df)
summary(candy.df)

#dropping competitor name column as it's not required.
candy.df <- candy.df[-c(1)]

#Checking the dimension again to check the dropped column
dim(candy.df)

#Checking for any null(NA) values
sum(is.na(candy.df))

#-------------------------------------------------------------------------------
#DATA VISUALIZATION
#-------------------------------------------------------------------------------

library(ggplot2)
library(corrplot)

boxplot(winpercent ~ chocolate, data = candy.df, col = "pink", border = "red",
        main= "HOW CHOCOLATE FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ fruity, data = candy.df, col = "pink", border = "red",
        main= "HOW FRUITY FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ caramel, data = candy.df, col = "pink", border = "red",
        main= "HOW CARAMEL FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ peanutyalmondy, data = candy.df, col = "pink", border = "red",
        main= "HOW PEANUT/ALMOND FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ nougat, data = candy.df, col = "pink", border = "red",
        main= "HOW NOUGAT FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ crispedricewafer, data = candy.df, col = "pink", border = "red",
        main= "HOW WAFER FLAVOR INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ hard, data = candy.df, col = "pink", border = "red",
        main= "HOW HARD CHOCOLATES INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ bar, data = candy.df, col = "pink", border = "red",
        main= "HOW BAR CHOCOLATE INFLUENCES WIN PERCENTAGE")

boxplot(winpercent ~ pluribus, data = candy.df, col = "pink", border = "red",
        main= "HOW PLURIBUS FLAVOR INFLUENCES WIN PERCENTAGE")

ggplot(data = candy.df) + geom_point(aes(x=sugarpercent, y=winpercent), col="blue") + 
  ggtitle("SUGAR Vs. Win %") + xlab("Sugar %") + ylab("Win %")
 
ggplot(data = candy.df) + geom_point(aes(x=pricepercent, y=winpercent), col="blue") + 
  ggtitle("PRICE Vs. Win %") + xlab("PRICE %") + ylab("Win %")

corrplot(cor(candy.df))

#-------------------------------------------------------------------------------
#PARTITIONING THE DATASET
#-------------------------------------------------------------------------------

candy.train.rows <- sample(rownames(candy.df), dim(candy.df)[1]*0.7)
candy.train.df <- candy.df[candy.train.rows, ]

candy.validate.rows <- sample(setdiff(rownames(candy.df), candy.train.rows))
candy.validate.df <- candy.df[candy.validate.rows, ]

#-------------------------------------------------------------------------------
#BUILDING MACHINE LEARNING MODELS
#-------------------------------------------------------------------------------

#1. Random Forest Regression
library(randomForest)

rfr <- randomForest(winpercent ~., data = candy.train.df, ntree=1000)

rfr.preds <- predict(rfr, candy.validate.df)

#2. Linear Regression

candy.train.df.scale <- data.frame(scale(candy.train.df))
candy.validate.df.scale <- data.frame(scale(candy.validate.df))

regressor <- lm(winpercent ~., data = candy.train.df.scale)

regressor.preds <- predict(regressor, candy.validate.df.scale)

#-------------------------------------------------------------------------------
#CHECKING THE PERFORMANCE OF RANDOM FOREST AND LINEAR REGRESSION
#-------------------------------------------------------------------------------
options(scipen = 999, digits = 4)

accuracy(rfr.preds, candy.validate.df$winpercent)
accuracy(regressor.preds, candy.validate.df.scale$winpercent)
