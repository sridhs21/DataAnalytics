##########################################
### SVM Regression on NY House Data   ###
##########################################

library(e1071)
library(Metrics)
library(readr)

ny_house <- read_csv("C:/Users/swaro/OneDrive/Desktop/Classes/Data Analytics/DataAnalytics/Lab 6/NY-House-Dataset.csv")

house_data <- ny_house[, c("PRICE", "PROPERTYSQFT")]
house_data <- na.omit(house_data)
house_data <- house_data[house_data$PRICE > 0 & house_data$PROPERTYSQFT > 0, ]

set.seed(123)
train_idx <- sample(1:nrow(house_data), 0.7 * nrow(house_data))
train_data <- house_data[train_idx, ]
test_data <- house_data[-train_idx, ]

# SVM Linear
tune_linear <- tune.svm(PRICE ~ PROPERTYSQFT, data = train_data, 
                        kernel = "linear",
                        cost = c(0.1, 1, 10, 100, 1000))

svm_linear <- svm(PRICE ~ PROPERTYSQFT, data = train_data, 
                  kernel = "linear",
                  cost = tune_linear$best.parameters$cost)

pred_linear <- predict(svm_linear, test_data)

mae_linear <- mae(test_data$PRICE, pred_linear)
mse_linear <- mse(test_data$PRICE, pred_linear)
rmse_linear <- rmse(test_data$PRICE, pred_linear)

# SVM RBF
tune_rbf <- tune.svm(PRICE ~ PROPERTYSQFT, data = train_data, 
                     kernel = "radial",
                     cost = c(1, 10, 100, 1000),
                     gamma = c(0.001, 0.01, 0.1, 1))

svm_rbf <- svm(PRICE ~ PROPERTYSQFT, data = train_data, 
               kernel = "radial",
               cost = tune_rbf$best.parameters$cost,
               gamma = tune_rbf$best.parameters$gamma)

pred_rbf <- predict(svm_rbf, test_data)

mae_rbf <- mae(test_data$PRICE, pred_rbf)
mse_rbf <- mse(test_data$PRICE, pred_rbf)
rmse_rbf <- rmse(test_data$PRICE, pred_rbf)

# Linear Regression
lm_model <- lm(PRICE ~ PROPERTYSQFT, data = train_data)
pred_lm <- predict(lm_model, test_data)

mae_lm <- mae(test_data$PRICE, pred_lm)
mse_lm <- mse(test_data$PRICE, pred_lm)
rmse_lm <- rmse(test_data$PRICE, pred_lm)

# Results
comparison <- data.frame(
  Model = c("SVM Linear", "SVM RBF", "Linear Regression"),
  MAE = c(mae_linear, mae_rbf, mae_lm),
  MSE = c(mse_linear, mse_rbf, mse_lm),
  RMSE = c(rmse_linear, rmse_rbf, rmse_lm)
)

print(comparison)

par(mfrow = c(1, 3))
plot(test_data$PRICE, pred_linear, main = "SVM Linear", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red", lwd = 2)
plot(test_data$PRICE, pred_rbf, main = "SVM RBF", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red", lwd = 2)
plot(test_data$PRICE, pred_lm, main = "Linear Regression", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red", lwd = 2)

