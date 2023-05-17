# AssingmentBigData

# QUESTION 1:
library(lpSolve)

# Set up the transportation problem

# Define the production and warehouse constraints
production_constraints <- c(100, 150, 200)  # Example production constraints
warehouse_constraints <- c(80, 120, 100)  # Example warehouse constraints

# Define the transportation costs
transportation_costs <- matrix(c(
  10, 8, 7,
  9, 12, 10,
  11, 7, 6
), nrow = length(production_constraints), ncol = length(warehouse_constraints), byrow = TRUE)

# Define the objective function (to minimize transportation cost)
objective_function <- as.vector(transportation_costs)

# Define the constraint matrix
constraint_matrix <- rbind(
  production_constraints,
  warehouse_constraints
)

# Define the constraint directions (<=)
constraint_directions <- c("<=", "<=")

# Define the right-hand side of the constraints
constraint_rhs <- c(sum(production_constraints), sum(warehouse_constraints))

# Set up the linear programming problem
transportation_lp <- lp("min", objective_function, constraint_matrix, constraint_directions, constraint_rhs)

# Solve the linear programming problem
solution <- transportation_lp$solution

# Print the solution (transportation schedule)
print(solution)





# QUESTION 2:

library(cluster)
library(ggplot2)

# Load the dataset
data <- read.csv("Mall Customers.csv")

# Select relevant columns for clustering
df <- data[, c("Gender", "Age", "Annual Income", "Spending Score")]

# Data preprocessing
df$Gender <- as.numeric(factor(df$Gender, levels = c("Male", "Female")))
df <- na.omit(df)

# Perform K-means clustering on gender and age
k <- 5  # Number of clusters
km <- kmeans(df[, c("Gender", "Age")], centers = k)
df$Cluster <- km$cluster

# Visualize the gender and age distributions
ggplot(df, aes(x = Age, y = Gender, color = factor(Cluster))) +
  geom_point() +
  labs(x = "Age", y = "Gender", color = "Cluster") +
  ggtitle("K-means Clustering: Gender and Age Distributions")

# Examine annual earnings and spending patterns within each cluster
cluster_stats <- aggregate(df[, c("Annual Income", "Spending Score")], by = list(df$Cluster), FUN = mean)
names(cluster_stats) <- c("Cluster", "Avg. Annual Income", "Avg. Spending Score")
print(cluster_stats)


# QUESTION 3:
library(ggplot2)
library(caret)
library(dplyr)

# Load the dataset
data <- read.csv("winequality-red.csv")

# Data Exploration
head(data)  # Print the first few rows of the dataset
summary(data)  # Summary statistics of the dataset

# Data Visualization
feature_names <- names(data)[-12]
plot_list <- lapply(feature_names, function(feature) {
  ggplot(data, aes_string(x = feature, y = "quality")) +
    geom_point() +
    labs(x = feature, y = "Quality")
})
multiplot(plotlist = plot_list, cols = 3)

# Prepare the data for modeling
X <- data[, -12]
y <- data$quality

# Split the data into training and testing sets
set.seed(42)
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Train the linear regression model
model <- train(x = X_train, y = y_train, method = "lm")

# Make predictions on the testing set
y_pred <- predict(model, newdata = X_test)

# Model evaluation
mse <- mean((y_test - y_pred)^2)
r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
print(paste("Mean Squared Error:", mse))
print(paste("R-squared Score:", r2))

# Plot the predicted vs. actual quality
ggplot(data.frame(y_test, y_pred), aes(x = y_test, y = y_pred)) +
  geom_point() +
  labs(x = "Actual Quality", y = "Predicted Quality") +
  ggtitle("Actual vs. Predicted Wine Quality")
