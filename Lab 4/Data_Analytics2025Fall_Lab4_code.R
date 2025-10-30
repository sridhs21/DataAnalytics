##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/swaro/OneDrive/Desktop/Classes/Data Analytics/DataAnalytics/Lab 4/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###
#compute the PCs and plot the dataset using the 1st and 2nd PCs
wine_pca <- princomp(wine[,-1], cor = TRUE)
summary(wine_pca)

#plot using 1st and 2nd PCs
scores <- wine_pca$scores
plot(scores[,1], scores[,2], col = c("red", "yellow", "blue")[wine$Type], pch = 19, xlab = "PC1", ylab = "PC2", main = "Wine Dataset: PC1 vs PC2")
legend("topright", legend = levels(wine$Type), col = c("red", "yellow", "blue"), pch = 19)

#identify the variables that contribute the most to the 1st PC and 2nd PCs
loadings <- wine_pca$loadings[,1:2]
print("Loadings for PC1 and PC2:")
print(loadings)

#train a classifier model (kNN) to predict wine type using all variables
set.seed(123)
train_idx <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_data <- wine[train_idx, ]
test_data <- wine[-train_idx, ]

#kNN on original variables
train_features <- train_data[, -1]
test_features <- test_data[, -1]
train_labels <- train_data$Type
test_labels <- test_data$Type

#scale the features
train_scaled <- scale(train_features)
test_scaled <- scale(test_features, center = attr(train_scaled, "scaled:center"), scale = attr(train_scaled, "scaled:scale"))

#train kNN (using k=5 as default)
knn_pred_original <- knn(train = train_scaled, test = test_scaled, cl = train_labels, k = 5)

#conf matrix
conf_matrix_original <- table(Predicted = knn_pred_original, Actual = test_labels)
print("Confusion Matrix (Original Vars):")
print(conf_matrix_original)

# now train a classifier model using data projected onto the first 2 PCs
#get scores for first 2 PCs
all_scores <- wine_pca$scores[, 1:2]
train_pc <- all_scores[train_idx, ]
test_pc <- all_scores[-train_idx, ]

#train kNN on PCs
knn_pred_pc <- knn(train = train_pc, test = test_pc, cl = train_labels, k = 5)

#conf matrix
conf_matrix_pc <- table(Predicted = knn_pred_pc, Actual = test_labels)
print("Confusion Matrix (First 2 PCs):")
print(conf_matrix_pc)

# compare both classification models using contingency tables and precision/recall/F1 metrics
calculate_metrics <- function(conf_matrix) {
  n_classes <- nrow(conf_matrix)
  precision <- numeric(n_classes)
  recall <- numeric(n_classes)
  f1 <- numeric(n_classes)
  
  for (i in 1:n_classes) {
    tp <- conf_matrix[i, i]
    fp <- sum(conf_matrix[i, ]) - tp
    fn <- sum(conf_matrix[, i]) - tp
    
    precision[i] <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall[i] <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1[i] <- ifelse(precision[i] + recall[i] == 0, 0, 
                    2 * precision[i] * recall[i] / (precision[i] + recall[i]))
  }
  
  return(data.frame(Class = 1:n_classes, Precision = precision, 
                    Recall = recall, F1 = f1))
}

print("Metrics for Original Variables:")
print(calculate_metrics(conf_matrix_original))
print("Metrics for First 2 PCs:")
print(calculate_metrics(conf_matrix_pc))
