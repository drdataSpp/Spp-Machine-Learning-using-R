# Spp Machine Learning in R - Project 5
# Predicting whether a product has reached in time.
# Predicting task: Binary Classification
##------------------------------------------------------------------------------

## Importing the data set
#-----------------------

delivery_df <- read.csv("D:\\001_Data\\Completed\\R Projects\\Project 5\\product-delivery.csv")

head(delivery_df)
tail(delivery_df)
dim(delivery_df)
summary(delivery_df)
sum(is.na(delivery_df))


## Pre-Processing Data
#---------------------

library(dummies)

t(t(names(delivery_df)))

dummy_vars <- delivery_df[c("Warehouse_block","Mode_of_Shipment",
                            "Product_importance","Gender" )]

dummy_df <- dummy.data.frame(dummy_vars, sep="_")
head(dummy_df)

final_list <- c(delivery_df, dummy_df)

final_df <- data.frame(final_list)
t(t(names(final_df)))

final_delivery_df <- final_df[c(4:7, 10:25)]

summary(final_delivery_df)


## Partitioning the dataset
#--------------------------
train.rows <- sample(rownames(final_delivery_df), dim(final_delivery_df)[1]*0.6)
train.df <- final_delivery_df[train.rows, ]

validate.rows <- sample(setdiff(rownames(final_delivery_df), train.rows))
validate.df <- final_delivery_df[validate.rows, ]

print(dim(train.df))
print(dim(validate.df))

train.df.scale <- scale(train.df[c(1:6, 8:19)], scale = T)
train.df.2 <- data.frame(train.df.scale)
train.df.2$y <- train.df$Reached.on.Time_Y.N
  
t(t(names(train.df.2)))

validate.df.scale <- scale(validate.df[c(1:6, 8:19)], scale = T)
validate.df.2 <- data.frame(validate.df.scale)
validate.df.2$y <- validate.df$Reached.on.Time_Y.N

t(t(names(validate.df.2)))

## Building ML Models
#-----------------------------


# MODEL 1
#--------

delivery.log.model <- glm(y ~ ., data = train.df.2,
                           family = binomial("logit"))

print(delivery.log.model)


delivery.predictions <- predict(delivery.log.model, validate.df.2) 

prediction.results <- ifelse(delivery.predictions > 0.5, 1, 0)

misClassificationRate <- mean(prediction.results != validate.df.2$y)

print((1 - misClassificationRate) * 100)



# MODEL 2
#--------

library(rpart)

delivery.dtree.model <- rpart(Reached.on.Time_Y.N ~ ., method = 'class',
                              data = train.df)

print(delivery.dtree.model)


delivery.predictions.2 <- predict(delivery.dtree.model, validate.df) 

prediction.results.2 <- ifelse(delivery.predictions.2 > 0.5, 1, 0)

misClassificationRate.dtree <- mean(prediction.results.2 != validate.df$Reached.on.Time_Y.N)

print((1 - misClassificationRate.dtree) * 100)

