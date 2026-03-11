#----------------------CLASS ACTIVITY 1----------------------------

# Dataset 
experience <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
salary     <- c(2500, 2700, 3000, 3400, 3900, 4400, 5000, 5600, 6200, 6900)

df <- data.frame(Experience_Years = experience, Monthly_Salary = salary)

# 70% Train / 30% Test Split
set.seed(42)
train_idx  <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_data <- df[ train_idx, ]
test_data  <- df[-train_idx, ]

# Fit Linear Regression Model 
model <- lm(Monthly_Salary ~ Experience_Years, data = train_data)
summary(model)

# Predictions on Test Set 
test_data$Predicted <- predict(model, newdata = test_data)
print(test_data)


# Plot
plot(train_data$Experience_Years, train_data$Monthly_Salary,
     col  = "steelblue", pch = 16, cex = 1.5,
     xlim = c(0, 11), ylim = c(2000, 8000),
     xlab = "Experience Years",
     ylab = "Monthly Salary (RM)",
     main = "Simple Linear Regression: Monthly Salary vs Experience Years")

# Test points
points(test_data$Experience_Years, test_data$Monthly_Salary,
       col = "tomato", pch = 17, cex = 1.5)

# Regression line
abline(model, col = "darkgreen", lwd = 2)

# Legend
legend("topleft",
       legend = c("Train data", "Test data", "Regression line"),
       col    = c("steelblue", "tomato", "darkgreen"),
       pch    = c(16, 17, NA),
       lty    = c(NA, NA, 1),
       lwd    = c(NA, NA, 2),
       bty    = "n")




#----------------------CLASS ACTIVITY 2----------------------------

# Dataset
df <- data.frame(
  Ozone   = c(11, 11, 11, 12, 12, 13, 13, 13, 13, 14),
  Solar.R = c(290, 44, 320, 149, 120, 137, 112, 27, 238, 274),
  Wind    = c(9.2, 9.7, 16.6, 12.6, 11.5, 10.3, 11.5, 10.3, 12.6, 10.9),
  Temp    = c(66, 62, 73, 74, 73, 76, 71, 76, 64, 68)
)

# Split 70% train, 30% test
set.seed(42)
train_idx  <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_data <- df[train_idx, ]
test_data  <- df[-train_idx, ]

# Build model
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = train_data)
summary(model)

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Compare actual vs predicted
data.frame(Actual = test_data$Ozone, Predicted = predictions)
