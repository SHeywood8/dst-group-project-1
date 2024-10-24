# Load necessary packages
library(rpart)
library(caret)  # For confusion matrix and precision/recall calculations
library(pROC)   # For ROC curve

# Load the training and test datasets
X_train <- read.csv("data/X_train.csv", row.names = 1)  # Set the first column as row names
y_train <- read.csv("data/y_train.csv", row.names = 1)  # Set the first column as row names
X_test <- read.csv("data/X_test.csv", row.names = 1)    # Set the first column as row names
y_test <- read.csv("data/y_test.csv", row.names = 1)    # Set the first column as row names

# Combine X_train and y_train into one data frame for rpart
train_data <- cbind(X_train, y_train)

# Check if the fitted model file exists
if (!file.exists("fit_model.RData")) {
  # Fit the classification tree using rpart with NA handling
  fit <- rpart(income ~ ., data = train_data, method = "class", control = rpart.control(cp = 1e-6))
  
  # Save the fitted model 'fit' to a file called 'fit_model.RData'
  save(fit, file = "fit_model.RData")
} else {
  cat("Model has already been fitted and saved as 'fit_model.RData'.\n")
}

# Load the fitted model
load("fit_model.RData")

# Make predictions on the test set
predictions <- predict(fit, newdata = X_test, type = "class")
pred_probs <- predict(fit, newdata = X_test, type = "prob")[,2]  # Probability of the positive class

# Create a confusion matrix
confusion_mat <- confusionMatrix(as.factor(predictions), as.factor(y_test[,1]))
print(confusion_mat)

# Convert predictions and actual values to factors
predictions_factor <- as.factor(predictions)
y_test_factor <- as.factor(y_test[, 1])  # Make sure this matches the correct column for your outcome variable

# Extract precision, recall, and F1-score
precision <- posPredValue(predictions_factor, y_test_factor, positive = ">50K")  # Assuming '1' is the positive class
recall <- sensitivity(predictions_factor, y_test_factor, positive = ">50K")
F1 <- (2 * precision * recall) / (precision + recall)

# Print the metrics
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1, "\n")

# Create ROC curve
roc_curve <- roc(y_test_factor, pred_probs, levels = c("<=50K", ">50K"))

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")  # Print AUC value
