####################################
##### Abalone Data Preparation #####
####################################

# read dataset
abalone.data <- read.csv("C:/Users/swaro/OneDrive/Desktop/Classes/Data Analytics/DataAnalytics/Lab 3/abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"

#exercise 1: kNN Models - Abalone 
set.seed(123)
s.train <- sample(nrow(abalone.data), nrow(abalone.data)*0.7)
abalone.train <- abalone.data[s.train,]
abalone.test <- abalone.data[-s.train,]

knn.model1 <- knn(abalone.train[,2:8], abalone.test[,2:8], abalone.train$age.group, k=3)

#confusion matrix for Model 1
cat("\nModel 1: All Features\n")
print(table(knn.model1, abalone.test$age.group, dnn=list('predicted','actual')))

#calc accuracy for Model 1
accuracy1 <- sum(knn.model1 == abalone.test$age.group) / length(abalone.test$age.group)
cat(paste("\nModel 1 Accuracy:", round(accuracy1, 4), "\n"))


#model 2 - Subset of features (length, diameter, whole_weight, shell_weight)
knn.model2 <- knn(abalone.train[,c(2,3,5,8)], abalone.test[,c(2,3,5,8)], abalone.train$age.group, k=3)

cat("\nmodel 2 - Selected Features (length, diameter, whole_weight, shell_weight)\n")
print(table(knn.model2, abalone.test$age.group, dnn=list('predicted','actual')))

accuracy2 <- sum(knn.model2 == abalone.test$age.group) / length(abalone.test$age.group)
cat(paste("\nModel 2 Accuracy:", round(accuracy2, 4), "\n"))


#find the optimal k for better performing model...
cat("\nFinding Optimal k\n")
if(accuracy1 >= accuracy2) {
  cat("Model 1 performs better. Finding optimal k...\n")
  best.features.train <- abalone.train[,2:8]
  best.features.test <- abalone.test[,2:8]
  best.cols <- 2:8
} else {
  cat("Model 2 performs better. Finding optimal k...\n")
  best.features.train <- abalone.train[,c(2,3,5,8)]
  best.features.test <- abalone.test[,c(2,3,5,8)]
  best.cols <- c(2,3,5,8)
}

#test different k values
k.values <- seq(1, 21, by=2)  # test odd numbers from 1 to 21
accuracies <- numeric(length(k.values))

for(i in 1:length(k.values)) {
  knn.pred <- knn(best.features.train, best.features.test,abalone.train$age.group, k=k.values[i])
  accuracies[i] <- sum(knn.pred == abalone.test$age.group) / length(abalone.test$age.group)
}

#find optimal k
optimal.k <- k.values[which.max(accuracies)]
max.accuracy <- max(accuracies)

cat(paste("\nOptimal k:", optimal.k, "\n"))
cat(paste("Maximum Accuracy:", round(max.accuracy, 4), "\n"))


#exercise 2: K-Means and PAM - Abalone

library(cluster)
library(factoextra)

abalone.cluster.data <- abalone.data[, best.cols]

abalone.scaled <- scale(abalone.cluster.data)


## K-Means Clustering ##
cat("\n\nK-Means Clustering\n")

#find optimal K using Elbow Method and Silhouette Method
#and test K values from 2 to 10
k.range <- 2:10

#silhouette method for K-Means
sil.kmeans <- numeric(length(k.range))
for(i in 1:length(k.range)) {
  kmeans.model <- kmeans(abalone.scaled, centers=k.range[i], nstart=25)
  sil <- silhouette(kmeans.model$cluster, dist(abalone.scaled))
  sil.kmeans[i] <- mean(sil[,3])
}

#find optimal K for K-Means
optimal.k.kmeans <- k.range[which.max(sil.kmeans)]
cat(paste("\nOptimal K for K-Means (Silhouette):", optimal.k.kmeans, "\n"))
cat(paste("Average Silhouette Width:", round(max(sil.kmeans), 4), "\n"))

#final K-Means model with optimal K
kmeans.final <- kmeans(abalone.scaled, centers=optimal.k.kmeans, nstart=25)

#silhouette plot for optimal K-Means
sil.kmeans.final <- silhouette(kmeans.final$cluster, dist(abalone.scaled))
plot(sil.kmeans.final, col=1:optimal.k.kmeans, border=NA, main=paste("K-Means Silhouette Plot (K =", optimal.k.kmeans, ")"))


##PAM Clustering
cat("\n\nPAM Clustering\n")

#silhouette method for PAM
sil.pam <- numeric(length(k.range))
for(i in 1:length(k.range)) {
  pam.model <- pam(abalone.scaled, k=k.range[i])
  sil.pam[i] <- pam.model$silinfo$avg.width
}

#find optimal K for PAM
optimal.k.pam <- k.range[which.max(sil.pam)]
cat(paste("\nOptimal K for PAM (Silhouette):", optimal.k.pam, "\n"))
cat(paste("Average Silhouette Width:", round(max(sil.pam), 4), "\n"))

#final PAM model with optimal K
pam.final <- pam(abalone.scaled, k=optimal.k.pam)

#silhouette plot for optimal PAM
plot(pam.final, which.plots=2, main=paste("PAM Silhouette Plot (K =", optimal.k.pam, ")"))