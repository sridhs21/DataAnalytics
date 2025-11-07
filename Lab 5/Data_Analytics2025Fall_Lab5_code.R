##########################################
### SVM Classification on Wine Data   ###
##########################################

#runonce
#install.packages("e1071")

library(e1071)
library(randomForest)
library(readr)

wine <- read_csv("C:/Users/swaro/OneDrive/Desktop/Classes/Data Analytics/DataAnalytics/Lab 5/wine.data", col_names = FALSE)

#set column names
names(wine) <- c("Type","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_Phenols","Proanthocyanins","Color_Intensity","Hue","Od280_od315_of_diluted_wines","Proline")

#convert Type to factor
wine$Type <- as.factor(wine$Type)

#select subset of variables (based on PCA importance)
selected_vars <- c("Flavanoids", "Total_phenols", "Proline", "Color_Intensity", "Alcohol")
wine_subset <- wine[, c("Type", selected_vars)]

#train/test split
set.seed(123)
train_idx <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_data <- wine_subset[train_idx, ]
test_data <- wine_subset[-train_idx, ]

#SVM with Linear Kernel
tune_linear <- tune.svm(Type ~ ., data = train_data, kernel = "linear",
                        cost = c(0.01, 0.1, 1, 10, 100))
svm_linear <- svm(Type ~ ., data = train_data, kernel = "linear",
                  cost = tune_linear$best.parameters$cost)
pred_linear <- predict(svm_linear, test_data)
conf_linear <- table(Predicted = pred_linear, Actual = test_data$Type)

#SVM with RBF Kernel
tune_rbf <- tune.svm(Type ~ ., data = train_data, kernel = "radial",
                     cost = c(0.1, 1, 10, 100),
                     gamma = c(0.01, 0.1, 0.5, 1))
svm_rbf <- svm(Type ~ ., data = train_data, kernel = "radial",
               cost = tune_rbf$best.parameters$cost,
               gamma = tune_rbf$best.parameters$gamma)
pred_rbf <- predict(svm_rbf, test_data)
conf_rbf <- table(Predicted = pred_rbf, Actual = test_data$Type)

#Random Forest
rf_model <- randomForest(Type ~ ., data = train_data, ntree = 500)
pred_rf <- predict(rf_model, test_data)
conf_rf <- table(Predicted = pred_rf, Actual = test_data$Type)

#calc metrics
calculate_metrics <- function(conf_matrix) {
  n <- nrow(conf_matrix)
  precision <- recall <- f1 <- numeric(n)
  
  for (i in 1:n) {
    tp <- conf_matrix[i, i]
    fp <- sum(conf_matrix[i, ]) - tp
    fn <- sum(conf_matrix[, i]) - tp
    
    precision[i] <- tp / (tp + fp)
    recall[i] <- tp / (tp + fn)
    f1[i] <- 2 * precision[i] * recall[i] / (precision[i] + recall[i])
  }
  
  return(data.frame(Class = 1:n, Precision = precision, Recall = recall, F1 = f1))
}

#output/results
print("Linear SVM - Best C:")
print(tune_linear$best.parameters$cost)
print("Confusion Matrix:")
print(conf_linear)
print("Metrics:")
print(calculate_metrics(conf_linear))

print("\nRBF SVM - Best C and gamma:")
print(tune_rbf$best.parameters)
print("Confusion Matrix:")
print(conf_rbf)
print("Metrics:")
print(calculate_metrics(conf_rbf))

print("\nRandom Forest:")
print("Confusion Matrix:")
print(conf_rf)
print("Metrics:")
print(calculate_metrics(conf_rf))

