# This routine is a work through of 
# https://www.datacamp.com/community/tutorials/machine-learning-in-r

# Load in built-in iris dataset
iris

# Load relevant libraries
library(class)
library(ggvis)

# Make some plots of quantities
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill=~Species) %>% layer_points()
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species) %>% layer_points()

# Calculate correlation matrices for different species
cor(iris[iris$Species == "setosa", 1:4])
cor(iris[iris$Species == "versicolor", 1:4])
cor(iris[iris$Species == "virginica", 1:4])

# Summary of data to see if normalisation is required
summary(iris)

# Normalisation function
normalise <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalise data 
iris_norm <- as.data.frame(lapply(iris[1:4], normalise))

# Summary of normalised data
summary(iris_norm)

# Training and test sets
set.seed(1234)
ind <- sample(2, nrow(iris_norm), replace=TRUE, prob=c(0.67, 0.33))
iris_train <- iris[ind==1, 1:4]
iris_test <- iris[ind==2, 1:4]
iris_train_labels <- iris[ind==1,5]
iris_test_labels <- iris[ind==2,5]

# Build the KNN model
iris_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=3)

# Merge observed classifications and predictions
iris_test_labels <- data.frame(iris_test_labels)
merge <- data.frame(iris_test_labels, iris_pred)
names(merge) <- c("Observed", "Predicted")
merge
  
  
