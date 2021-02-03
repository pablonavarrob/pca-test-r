setwd("/Users/pablo/Desktop/Herts Master/Machine Learning /Exercises/Lab 3")
library(matrixStats)
library(ggplot2)
library(ggfortify)
library(datasets)
library(dplyr)

# Run PCA step by step
# - Create simple data set for experimentation purposes
d <- expand.grid(x=1:10, y=1:10) # Basically a meshgrid in Python
d$z <- 2 * d$x


# First step is to center and scale the data set: calculate mean 
# and std for each feature, in this case the x and y coordinates
mu = apply(d, 2, mean)
std = apply(d, 2, sd)

preprocess_dataset <- function(data, mu_matrix, sigma_matrix){
  # Make copy of data array to overwrite
  # It is variable according to the dimensions of the data
  centered_data = data
  for (i in 1:dim(data)[2]) {
    centered_column = (data[i] - mu_matrix[i])/sigma_matrix[i]
    # Append the newer columns
    centered_data[i] = centered_column
  }
  return(centered_data)
}

d_centered = preprocess_dataset(d, mu, std)

# Next step is to compute covariance matrix, it is symmetric, 
# so it's fine to compute one triangle and flip it. 
# How the fuck do they get to the covariance matrix?
covariance_matrix = cov(d_centered)

# Calculate eigenvectors
eigenvectors <- eigen(covariance_matrix)
eigenvalues <- eigenvectors$values # The eigenvalues are the
# variance, square of the standard devaiation
matrix_eigenvalues <- diag(dim(d)[2])*(1/sqrt(eigenvalues))

# Compute final matrix
pcad_matrix = data.matrix(d_centered) %*% eigenvectors$vectors

# Run PCA with prcomp: basically does everythingm
# scale true centers (0 mean) and scales de variance to unit
pca_prcomp <- prcomp(d, scale=TRUE)

# Use the iris data set and perform PCA on it
data("iris")
iris <- select(iris,  Sepal.Length, Sepal.Width,
               Petal.Length, Petal.Width, Species)
# Remove last column for species
cols <- as.numeric(iris$Species)
pca_iris <- prcomp(iris[,-5], scale=TRUE)

pdf(file="/Users/pablo/Desktop/Herts Master/Machine Learning /Exercises/Lab 3/pca.pdf")
pairs(pcad_matrix, labels = c("PC 1", "PC 2", "PC 3"))
dev.off()


