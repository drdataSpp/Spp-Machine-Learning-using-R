#Spp Machine Learning in R - Project 6
#Prediction of Clothing Sizes.
##------------------------------------------------------------------------------

#Importing the data set
#-----------------------

cloth_df <- read.csv("D:\\001_Data\\Completed\\R Projects\\Project 6\\cloth_size_preds.csv")

head(cloth_df)
tail(cloth_df)
summary(cloth_df)
dim(cloth_df)

sum(is.na(cloth_df))

#Filling NaN Values
mean_age <- mean(cloth_df$age, na.rm = TRUE)
print(mean_age)
cloth_df$age[is.na(cloth_df$age)] <- mean_age

mean_height <- mean(cloth_df$height, na.rm = TRUE)
print(mean_height)
cloth_df$height[is.na(cloth_df$height)] <- mean_height

summary(cloth_df$height)
sum(is.na(cloth_df))


#Data Visualization
#------------------

library(ggplot2)

#Count of Output Labels
##------------------------------------------------------------------------------
plot1 <- ggplot(cloth_df) + geom_bar(aes(x = size), color="red",
                                       fill="pink") 

plot1 + ggtitle("Count of the Outcome Labels") + xlab("Size") +
  ylab("Count of the Output Labels") 


#Count of age
##------------------------------------------------------------------------------
plot2 <- ggplot(cloth_df) +
  geom_histogram(stat = "count",  aes(x=age), color="red",
                 fill="pink") 

plot2 + ggtitle("Count of the Age") + xlab("Age") +
  ylab("Count of the Age")


#Count of weight
##------------------------------------------------------------------------------
plot3 <- ggplot(cloth_df) +
  geom_histogram(stat = "count",  aes(x=weight), color="red",
                 fill="pink") 

plot3 + ggtitle("Count of the Weight") + xlab("Weight") +
  ylab("Count of the Weight")


#Count of Height
##------------------------------------------------------------------------------
plot3 <- ggplot(cloth_df) +
  geom_histogram(stat = "count",  aes(x=height), color="red",
                 fill="pink") 

plot3 + ggtitle("Count of the height") + xlab("height") +
  ylab("Count of the height")


##PARTITIONING THE DATA SET
##-------------------------
train.rows <- sample(rownames(cloth_df), dim(cloth_df)[1]*0.6)
train.df <- cloth_df[train.rows, ]

validate.rows <- sample(setdiff(rownames(cloth_df), train.rows))
validate.df <- cloth_df[validate.rows, ]

print(dim(train.df))
print(dim(validate.df))

##BUILDING THE ML MODEL
##---------------------

## RANDOM FOREST MODEL
##--------------------

library(randomForest)
rf_classifier = randomForest(size ~ ., data= train.df, 
                             ntree=100, mtry=2, importance=TRUE)

print(rf_classifier)
varImpPlot(rf_classifier)

rf.predictions <- predict(rf_classifier, validate.df) 
print(rf.predictions)
table(validate.df$size, rf.predictions)

## DECISION TREE MODEL
##--------------------

library(rpart)

dtree <- rpart(size ~ ., method = 'class', data=train.df)

plot(dtree, uniform = TRUE, 
     main = "Size Prediction - Decision  
                 Tree Classifier")

text(dtree, use.n = TRUE, cex = .7)

dtree.predictions = predict(dtree, validate.df, type = 'class')
table(validate.df$size, dtree.predictions)

##MODEL EVALUATION
##---------------------

# Confusion Matrix

print("Confusion Matrix of the Random Forest Model: ")
print("============================================")
table(validate.df$size, rf.predictions)

cm <- table(validate.df$size, rf.predictions)

# Accuracy

rf_accuracy <- sum(diag(cm)) / sum(cm)
print(paste('Accuracy of Random Forest Model: ', rf_accuracy))
