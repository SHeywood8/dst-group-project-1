# Load the training and test datasets
X_train <- read.csv("data/X_train.csv", row.names = 1)  # Set the first column as row names
y_train <- read.csv("data/y_train.csv", row.names = 1)  # Set the first column as row names
X_test <- read.csv("data/X_test.csv", row.names = 1)    # Set the first column as row names
y_test <- read.csv("data/y_test.csv", row.names = 1)    # Set the first column as row names

# Check the first few rows of each dataset
head(X_train)
head(y_train)
head(X_test)
head(y_test)

# Combine X_train and y_train into one data frame for rpart
train_data <- cbind(X_train, y_train)

# Load the necessary package
library(rpart)

# Replace "?" with NA
train_data[train_data == "?"] <- NA

# Fit the classification tree using rpart with NA handling
fit <- rpart(income ~ ., data = train_data, method = "class", control = rpart.control(cp = 1e-6))

# Get the cost-complexity pruning table
cptable <- fit$cptable

# Plot the predicted cross-validated error (xerror) against the cp (alpha) values on a log scale
plot(cptable[,"CP"], cptable[,"xerror"], type = "b", 
     xlab = expression(alpha ~ "(cp values, log scale)"), 
     ylab = "Cross-validated error (xerror)", 
     main = "Predicted Error vs Alpha (Cost-Complexity Parameter)",
     col = "blue", pch = 19, log = "x")  # Set log scale for x-axis

# Add error bars representing xstd (standard deviation of the error)
arrows(cptable[,"CP"], cptable[,"xerror"] - cptable[,"xstd"], 
       cptable[,"CP"], cptable[,"xerror"] + cptable[,"xstd"], 
       angle = 90, code = 3, length = 0.05, col = "red")

# Get surrogate splits information
surrogate_info <- fit$frame[fit$frame$var != "<leaf>", c("var", "n", "ncompete", "nsurrogate")]
surrogate_info <- surrogate_info[surrogate_info$nsurrogate > 0, ]  # Filter for nodes with surrogates

# Display the surrogate splits
print(surrogate_info)

# Optional: Create a more readable format for surrogate splits
for (i in seq_len(nrow(surrogate_info))) {
  cat("Node:", i, "\n")
  cat("Primary Split Variable:", surrogate_info$var[i], "\n")
  cat("Number of Observations:", surrogate_info$n[i], "\n")
  cat("Number of Competitors:", surrogate_info$ncompete[i], "\n")
  cat("Number of Surrogates:", surrogate_info$nsurrogate[i], "\n\n")
}
