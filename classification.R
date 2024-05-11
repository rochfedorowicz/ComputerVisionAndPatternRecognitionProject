# Load necessary libraries
library(readxl)
library(e1071)
library(caret)
library(randomForest)
library(janitor)

# Load the data
file_path <- "W:/repos/WKiRO_proj/Project/cleaned_waltzdb_export.xlsx"
data <- read_excel(file_path) %>% clean_names()

# Preprocessing Data
data$classification <- as.factor(data$classification)

# Correcting factor levels to valid R variable names
levels(data$classification) <- make.names(levels(data$classification))

# Scale numeric input variables and convert characters to factors
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, scale)

# Split data into training and testing sets
set.seed(123)  # for reproducibility
training_index <- createDataPartition(data$classification, p = 0.8, list = FALSE)
training_data <- data[training_index, ]
testing_data <- data[-training_index, ]

# Setup training control for initial models (to save predictions for stacking)
control_base <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Train the base models
model_rf <- train(classification ~ ., data = training_data, method = "rf", trControl = control_base, tuneLength = 3)
model_svm <- train(classification ~ ., data = training_data, method = "svmRadial", trControl = control_base, tuneLength = 3)

# Predict on test data using base models
predictions_rf <- predict(model_rf, testing_data)
predictions_svm <- predict(model_svm, testing_data)

# Evaluate the base models
conf_matrix_rf <- confusionMatrix(predictions_rf, testing_data$classification)
conf_matrix_svm <- confusionMatrix(predictions_svm, testing_data$classification)

# Setup training control for meta-model (using CV predictions)
control_meta <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Combine predictions and actual values into a new training set for the meta-model
training_predictions <- data.frame(
  rf_prob_pos = model_rf$pred$amyloid,
  rf_prob_neg = model_rf$pred$amyloid,
  svm_prob_pos = model_svm$pred$non.amyloid,
  svm_prob_neg = model_svm$pred$non.amyloid,
  actual = training_data$classification
)

# Train the meta-model
meta_model <- train(actual ~ ., data = training_predictions, method = "glm",
                    family = "binomial",  # Specify the family for logistic regression
                    trControl = control_meta,
                    weights = ifelse(training_predictions$actual == "amyloid", 1.8, 1.02),
                    metric = "Accuracy",
                    maximize = TRUE)

# Prepare the test set predictions to feed into the meta-model
test_predictions_rf_pos <- predict(model_rf, testing_data, type = "prob")[, "amyloid"]
test_predictions_rf_neg <- predict(model_rf, testing_data, type = "prob")[, "non.amyloid"]
test_predictions_svm_pos <- predict(model_svm, testing_data, type = "prob")[, "amyloid"]
test_predictions_svm_neg <- predict(model_svm, testing_data, type = "prob")[, "non.amyloid"]
test_predictions <- data.frame(
  rf_prob_pos = test_predictions_rf_pos,
  rf_prob_neg = test_predictions_rf_neg,
  svm_prob_pos = test_predictions_svm_pos,
  svm_prob_neg = test_predictions_svm_neg
)

# Predict using the meta-model on the test set
final_predictions <- predict(meta_model, test_predictions)

# Evaluate the final model
final_results <- confusionMatrix(final_predictions, testing_data$classification)

# Print the confusion matrices
cat("Confusion Matrix for Random Forest:\n")
print(conf_matrix_rf)
cat("\nConfusion Matrix for SVM:\n")
print(conf_matrix_svm)
cat("\nConfusion Matrix for Meta-Classifier (Logistic Regression):\n")
print(final_results)
