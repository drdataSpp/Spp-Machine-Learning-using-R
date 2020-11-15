#Spp Machine Learning in R - Project 1
#Predicting Airfare On New Routes
#---------------------------------------------

airfares.df <- read.csv("Airfares.csv")
head(airfares.df)
summary(airfares.df)

#removing the first 4 variables, as it's not important for the model 
airfares.df <- airfares.df[, c(5:18)]
head(airfares.df)
t(t(names(airfares.df)))

#Creating correlation matrix and scatter plot between FARE & Numerical predictors.
corr.airfares.df <- airfares.df[, c(1,2,5,6,7,8,9,12,13,14)]
corr.airfares <- cor(corr.airfares.df)

#Creating scatter plots to understand the relationship between the features and
#label.
library(gplots)
heatmap.2(corr.airfares, Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(corr.airfares,2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(7,7))

library(ggplot2)
ggplot(data = corr.airfares.df) + geom_point(aes(x=COUPON, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. COUPON") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=NEW, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. NEW") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=HI, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. HI") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=S_INCOME, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. S_INCOME") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=E_INCOME, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. E_INCOME") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=S_POP, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. S_POP") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=E_POP, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. E_POP") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=DISTANCE, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. DISTANCE") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = corr.airfares.df) + geom_point(aes(x=PAX, y=FARE), 
                                             color='darkred', size=2) + 
  labs(title = "FARE Vs. PAX") + 
  theme(plot.title = element_text(hjust = 0.5))

#Converting categorical variables to dummy/binary & creating partition sets.
airfares.df$VACATION <- ifelse(airfares.df$VACATION == 'Yes', 1, 0)
airfares.df$SW <- ifelse(airfares.df$SW == 'Yes', 1, 0)
airfares.df$SLOT <- ifelse(airfares.df$SLOT == 'Controlled', 1, 0)
airfares.df$GATE <- ifelse(airfares.df$VACATION == 'Constrained', 1, 0)

head(airfares.df)

#Partitioning the data set.
airfare.train.rows <- sample(rownames(airfares.df), dim(airfares.df)[1]*0.6)
airfares.train.df <- airfares.df[airfare.train.rows, ]

airfare.validate.rows <- sample(setdiff(rownames(airfares.df), airfare.train.rows))
airfare.validate.df <- airfares.df[airfare.validate.rows, ]

#Building a Regression model with all 13 predictors/features
airfares.reg <- lm(FARE ~., data = airfares.train.df)
options(scipen = 999, digits = 4)
summary(airfares.reg)

#Using exhaustive search to reduce the no.of predictors
library(leaps)
best.airfares.lm.model <- regsubsets(FARE ~ ., data = airfares.train.df, method = "exhaustive")
new.model.summary <- summary(best.airfares.lm.model)

new.model.summary$adjr2
new.model.summary$rsq
new.model.summary$cp
new.model.summary$which

#Building a new model with reduce features.
adjreg.airfares.model <- lm(FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP
                            + SLOT + DISTANCE + PAX, data = airfares.train.df)
summary(adjreg.airfares.model)
summary(airfares.reg)

#Comparing predictive accuracy of the 2 different models.
library(forecast)
lm.with.13.variables <- predict(airfares.reg, airfare.validate.df)
lm.with.9.variables <- predict(adjreg.airfares.model, airfare.validate.df)
options(scipen=999, digits = 4)

accuracy(lm.with.13.variables, airfare.validate.df$FARE)
accuracy(lm.with.9.variables, airfare.validate.df$FARE)

#Checking the new model with custom values.
custom.values1 <- data.frame("VACATION" = 0, "SW" = 0,
                             "HI" = 4442.141, "E_INCOME" = 27664,
                             "S_POP" = 4557004,"E_POP" = 3195503, "SLOT" = 0,
                             "DISTANCE"=1976, "PAX"=12782)

adjreg.model.prediction <- predict(adjreg.airfares.model, custom.values1) 
adjreg.model.prediction

#If SW = 1 
custom.values2 <- data.frame("VACATION" = 0, "SW" = 1,
                             "HI" = 4442.141, "E_INCOME" = 27664,
                             "S_POP" = 4557004,"E_POP" = 3195503, "SLOT" = 0,
                             "DISTANCE"=1976, "PAX"=12782)

adjreg.model.prediction2 <- predict(adjreg.airfares.model, custom.values2) 
adjreg.model.prediction2

#Building a LM model before flights began to operate on a new route.
lm.model.before.airport <- regsubsets(FARE ~ VACATION + SW + 
                                        S_INCOME + E_INCOME + 
                                        S_POP + E_POP
                                      + SLOT + GATE + DISTANCE 
                                      , data = airfares.train.df, method = "exhaustive")
no.airport.model.summary <- summary(lm.model.before.airport)

no.airport.model.summary$adjr2
no.airport.model.summary$cp
no.airport.model.summary$rsq
no.airport.model.summary$which

no.airport.airfares.model <- lm(FARE ~ VACATION + SW + E_INCOME 
                                + S_POP + E_POP + SLOT + DISTANCE, 
                                data = airfares.train.df)
summary(adjreg.airfares.model)
summary(no.airport.airfares.model)

#Trying custom values on the above 
custom.values3 <- data.frame("VACATION" = 0, "SW" = 0,
                             "E_INCOME" = 27664, "S_POP" = 4557004,
                             "E_POP" = 3195503, "SLOT" = 0,
                             "DISTANCE"=1976)

no.airpot.predictions <- predict(no.airport.airfares.model, custom.values3) 
no.airpot.predictions

summary(adjreg.airfares.model)
summary(no.airport.airfares.model)

lm.model.before.airpot <- predict(no.airport.airfares.model, airfare.validate.df)

accuracy(lm.model.before.airpot, airfare.validate.df$FARE)
accuracy(lm.with.9.variables, airfare.validate.df$FARE)
##------------------------------------------------------------------------------
