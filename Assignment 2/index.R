# EPI 2024 Statistical Analysis - Complete Script

# Load required libraries
library(class)

# Load the data
epi_data <- read.csv("epi_results_2024_pop_gdp_v2.csv", stringsAsFactors = FALSE)

# ============================================================================
# 1. VARIABLE DISTRIBUTIONS
# ============================================================================

# 1.1 Create two regional subsets
region1 <- epi_data[epi_data$region == "Global West", ]
region2 <- epi_data[epi_data$region == "Sub-Saharan Africa", ]

# Choose variable: AIR.new
r1_values <- region1$AIR.new[!is.na(region1$AIR.new)]
r2_values <- region2$AIR.new[!is.na(region2$AIR.new)]

# Boxplot - Region 1 (Global West)
boxplot(r1_values, 
        main = "Boxplot: Air Quality - Global West",
        ylab = "Air Quality Score (AIR.new)",
        col = "lightblue",
        border = "navy")

# Boxplot - Region 2 (Sub-Saharan Africa)
boxplot(r2_values, 
        main = "Boxplot: Air Quality - Sub-Saharan Africa",
        ylab = "Air Quality Score (AIR.new)",
        col = "lightcoral",
        border = "darkred")

# Histogram with density overlay - Region 1
hist(r1_values, 
     breaks = 15,
     main = "Histogram: Air Quality - Global West",
     xlab = "Air Quality Score (AIR.new)",
     ylab = "Frequency",
     col = "skyblue",
     border = "white",
     prob = FALSE)
d1 <- density(r1_values)
d1$y <- d1$y * length(r1_values) * diff(hist(r1_values, plot = FALSE, breaks = 15)$breaks[1:2])
lines(d1, col = "darkblue", lwd = 2)

# Histogram with density overlay - Region 2
hist(r2_values, 
     breaks = 15,
     main = "Histogram: Air Quality - Sub-Saharan Africa",
     xlab = "Air Quality Score (AIR.new)",
     ylab = "Frequency",
     col = "salmon",
     border = "white",
     prob = FALSE)
d2 <- density(r2_values)
d2$y <- d2$y * length(r2_values) * diff(hist(r2_values, plot = FALSE, breaks = 15)$breaks[1:2])
lines(d2, col = "darkred", lwd = 2)

# 1.2 Q-Q Plot between two regions
qqplot(r1_values, r2_values,
       main = "Q-Q Plot: Global West vs Sub-Saharan Africa",
       xlab = "Global West Quantiles",
       ylab = "Sub-Saharan Africa Quantiles",
       col = "blue",
       pch = 16)
abline(0, 1, col = "red", lwd = 2)

# ============================================================================
# 2. LINEAR MODELS
# ============================================================================

# 2.1 Full Dataset - Choose AIR.new as response variable

# Prepare full dataset
model_data_full <- epi_data[!is.na(epi_data$AIR.new) & 
                              !is.na(epi_data$population) & 
                              !is.na(epi_data$gdp) & 
                              epi_data$population > 0 & 
                              epi_data$gdp > 0, ]
model_data_full$log_population <- log(model_data_full$population)
model_data_full$log_gdp <- log(model_data_full$gdp)

# FULL DATASET - Model 1: AIR.new ~ log(Population)
model1_full <- lm(AIR.new ~ log_population, data = model_data_full)
summary(model1_full)

# Plot predictor vs response
plot(model_data_full$log_population, model_data_full$AIR.new,
     main = "Full Dataset Model 1: AIR.new ~ log(Population)",
     xlab = "log(Population)",
     ylab = "Air Quality (AIR.new)",
     pch = 16,
     col = rgb(0.4, 0.4, 0.8, 0.6))
abline(model1_full, col = "red", lwd = 2)

# Plot residuals
plot(fitted(model1_full), residuals(model1_full),
     main = "Residual Plot: Full Dataset Model 1",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0.4, 0.4, 0.8, 0.6))
abline(h = 0, col = "red", lwd = 2)

# FULL DATASET - Model 2: AIR.new ~ log(GDP)
model2_full <- lm(AIR.new ~ log_gdp, data = model_data_full)
summary(model2_full)

# Plot predictor vs response
plot(model_data_full$log_gdp, model_data_full$AIR.new,
     main = "Full Dataset Model 2: AIR.new ~ log(GDP)",
     xlab = "log(GDP)",
     ylab = "Air Quality (AIR.new)",
     pch = 16,
     col = rgb(0.1, 0.7, 0.5, 0.6))
abline(model2_full, col = "red", lwd = 2)

# Plot residuals
plot(fitted(model2_full), residuals(model2_full),
     main = "Residual Plot: Full Dataset Model 2",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0.1, 0.7, 0.5, 0.6))
abline(h = 0, col = "red", lwd = 2)

# 2.2 Regional Subset (Global West) - Repeat both models

# Prepare regional subset data
model_data_region <- epi_data[epi_data$region == "Global West" &
                                !is.na(epi_data$AIR.new) & 
                                !is.na(epi_data$population) & 
                                !is.na(epi_data$gdp) & 
                                epi_data$population > 0 & 
                                epi_data$gdp > 0, ]
model_data_region$log_population <- log(model_data_region$population)
model_data_region$log_gdp <- log(model_data_region$gdp)

# REGIONAL SUBSET - Model 1: AIR.new ~ log(Population)
model1_region <- lm(AIR.new ~ log_population, data = model_data_region)
summary(model1_region)

# Plot predictor vs response
plot(model_data_region$log_population, model_data_region$AIR.new,
     main = "Regional Model 1: AIR.new ~ log(Population)",
     xlab = "log(Population)",
     ylab = "Air Quality (AIR.new)",
     pch = 16,
     col = rgb(0.5, 0.3, 0.8, 0.6))
abline(model1_region, col = "red", lwd = 2)

# Plot residuals
plot(fitted(model1_region), residuals(model1_region),
     main = "Residual Plot: Regional Model 1",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0.5, 0.3, 0.8, 0.6))
abline(h = 0, col = "red", lwd = 2)

# REGIONAL SUBSET - Model 2: AIR.new ~ log(GDP)
model2_region <- lm(AIR.new ~ log_gdp, data = model_data_region)
summary(model2_region)

# Plot predictor vs response
plot(model_data_region$log_gdp, model_data_region$AIR.new,
     main = "Regional Model 2: AIR.new ~ log(GDP)",
     xlab = "log(GDP)",
     ylab = "Air Quality (AIR.new)",
     pch = 16,
     col = rgb(0.9, 0.6, 0.1, 0.6))
abline(model2_region, col = "red", lwd = 2)

# Plot residuals
plot(fitted(model2_region), residuals(model2_region),
     main = "Residual Plot: Regional Model 2",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0.9, 0.6, 0.1, 0.6))
abline(h = 0, col = "red", lwd = 2)

# ============================================================================
# 3. kNN CLASSIFICATION
# ============================================================================

# 3.1 kNN Model 1: Use EPI.new, AIR.new, HLT.new as inputs

knn_data1 <- epi_data[, c("region", "EPI.new", "AIR.new", "HLT.new")]
knn_data1 <- knn_data1[complete.cases(knn_data1), ]

knn_features1 <- knn_data1[, c("EPI.new", "AIR.new", "HLT.new")]
knn_labels1 <- knn_data1$region

# Normalize features
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
knn_features1_norm <- as.data.frame(lapply(knn_features1, normalize))

# Perform leave-one-out cross-validation with k=5
k_value <- 5
predictions1 <- character(nrow(knn_features1_norm))

for (i in 1:nrow(knn_features1_norm)) {
  train_data <- knn_features1_norm[-i, ]
  test_data <- knn_features1_norm[i, ]
  train_labels <- knn_labels1[-i]
  
  pred <- knn(train = train_data, 
              test = test_data, 
              cl = train_labels, 
              k = k_value)
  predictions1[i] <- as.character(pred)
}

# Calculate accuracy
accuracy1 <- sum(predictions1 == knn_labels1) / length(knn_labels1)

# Confusion matrix
confusion_matrix1 <- table(Actual = knn_labels1, Predicted = predictions1)
print(confusion_matrix1)
print(paste("Accuracy:", round(accuracy1 * 100, 2), "%"))

# 3.2 kNN Model 2: Use BDH.new, H2O.new, PCC.new as inputs

knn_data2 <- epi_data[, c("region", "BDH.new", "H2O.new", "PCC.new")]
knn_data2 <- knn_data2[complete.cases(knn_data2), ]

knn_features2 <- knn_data2[, c("BDH.new", "H2O.new", "PCC.new")]
knn_labels2 <- knn_data2$region

knn_features2_norm <- as.data.frame(lapply(knn_features2, normalize))

# Perform leave-one-out cross-validation with k=5
predictions2 <- character(nrow(knn_features2_norm))

for (i in 1:nrow(knn_features2_norm)) {
  train_data <- knn_features2_norm[-i, ]
  test_data <- knn_features2_norm[i, ]
  train_labels <- knn_labels2[-i]
  
  pred <- knn(train = train_data, 
              test = test_data, 
              cl = train_labels, 
              k = k_value)
  predictions2[i] <- as.character(pred)
}

# Calculate accuracy
accuracy2 <- sum(predictions2 == knn_labels2) / length(knn_labels2)

# Confusion matrix
confusion_matrix2 <- table(Actual = knn_labels2, Predicted = predictions2)
print(confusion_matrix2)
print(paste("Accuracy:", round(accuracy2 * 100, 2), "%"))


