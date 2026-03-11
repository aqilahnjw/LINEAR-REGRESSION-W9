#----------------------- MULTIPLE LINEAR REGRESSION ----------------------------

#built in data
data(mtcars)
head(mtcars)
str(mtcars)

#model the MLR
model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
#investigate the properties of the model
summary(model)

#split data into train and test sets
data.train<- mtcars[1:22,]
data.test<- mtcars[23:32,]
#modelling
relation <-lm(mpg ~ hp +wt+cyl, data = data.train)
summary(relation)
# Prediction
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2)) 


# performance measurement
mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")


# or

actuals_preds <- data.frame(cbind(actuals=data.test$mpg,predicted=result))
View(actuals_preds )

correlation_accuracy <- cor(actuals_preds)

mape <- mean(abs(actuals_preds$actuals -actuals_preds$predicted)/actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")

