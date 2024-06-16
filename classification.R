# Load necessary libraries
library(readxl)
library(e1071)
library(caret)
library(randomForest)
library(janitor)
library(dplyr)
library(pROC)

# Load the data
file_path <- "encoded_waltz.xlsx"
data <- read_excel(file_path) %>% clean_names()

# Preprocessing Data
data$classification <- as.factor(data$classification)

# Correcting factor levels to valid R variable names
levels(data$classification) <- make.names(levels(data$classification))

# Scale numeric input variables and convert charDacters to factors
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, scale)

# Handle missing values
data <- na.omit(data)

# Split data into training and testing sets
set.seed(123)  # for reproducibility
training_index <- createDataPartition(data$classification, p = 0.8, list = FALSE)
training_data <- data[training_index, ]
testing_data <- data[-training_index, ]

# Setup training control for initial models (to save predictions for stacking)
control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)

# Train the Random Forest model
model_rf <- train(classification ~ ., data = training_data, method = "rf", trControl = control, tuneLength = 3)

# Get the variable importance from the Random Forest model
importance_rf <- varImp(model_rf, scale = FALSE)
important_features <- rownames(importance_rf$importance)[order(importance_rf$importance$Overall, decreasing = TRUE)]

# Calculate top_n as 5% of the total number of features
total_features <- length(important_features)
top_n <- max(1, round(0.05 * total_features))  # Ensure at least one feature is selected
selected_features <- important_features[1:top_n]

# Train SVM model with all features (unchanged)
model_svm <- train(classification ~ ., data = training_data, method = "svmRadial", trControl = control, tuneLength = 3)

# Train GLM with PCA applied directly using only important features
formula_glm <- as.formula(paste("classification ~", paste(selected_features, collapse = " + ")))
model_glm <- train(formula_glm, data = training_data, method = "glm", trControl = control, preProcess = c("center", "scale", "pca"), tuneLength = 3)

# Predict on training data using base models (needed for meta-model training)
training_predictions_rf <- predict(model_rf, training_data, type = "prob")
training_predictions_svm <- predict(model_svm, training_data, type = "prob")

# Combine base model predictions with selected features from the training data
training_meta_data <- training_data %>% select(all_of(selected_features))
training_meta_data <- cbind(training_meta_data, 
                            rf_prob_pos = training_predictions_rf$amyloid,
                            rf_prob_neg = training_predictions_rf$non.amyloid,
                            svm_prob_pos = training_predictions_svm$amyloid,
                            svm_prob_neg = training_predictions_svm$non.amyloid)

# Add the classification column back for meta model training
training_meta_data$classification <- training_data$classification

# Train the meta-model using glm
meta_model_glm <- train(classification ~ ., data = training_meta_data, method = "glm",
                        trControl = control,
                        metric = "Accuracy",
                        maximize = TRUE)

# Prepare the test set predictions to feed into the meta-model
test_predictions_rf <- predict(model_rf, testing_data, type = "prob")
test_predictions_svm <- predict(model_svm, testing_data, type = "prob")

# Combine base model predictions with selected features from the testing data
testing_meta_data <- testing_data %>% select(all_of(selected_features))
testing_meta_data <- cbind(testing_meta_data, 
                           rf_prob_pos = test_predictions_rf$amyloid,
                           rf_prob_neg = test_predictions_rf$non.amyloid,
                           svm_prob_pos = test_predictions_svm$amyloid,
                           svm_prob_neg = test_predictions_svm$non.amyloid)

# Generate confusion matrices for individual models
conf_matrix_rf <- confusionMatrix(predict(model_rf, testing_data), testing_data$classification)
conf_matrix_svm <- confusionMatrix(predict(model_svm, testing_data), testing_data$classification)
conf_matrix_glm <- confusionMatrix(predict(model_glm, testing_data), testing_data$classification)

# Predict using the glm meta-model on the test set
final_predictions_glm <- predict(meta_model_glm, testing_meta_data)

# Evaluate the final model
final_results_glm <- confusionMatrix(final_predictions_glm, testing_data$classification)

# Print the confusion matrices
cat("Confusion Matrix for Random Forest:\n")
print(conf_matrix_rf)
cat("\nConfusion Matrix for SVM:\n")
print(conf_matrix_svm)
cat("\nConfusion Matrix for GLM:\n")
print(conf_matrix_glm)
cat("\nConfusion Matrix for GLM Meta-Classifier:\n")
print(final_results_glm)

# ROC and AUC calculations
roc_rf <- roc(testing_data$classification, test_predictions_rf[,2], levels = rev(levels(testing_data$classification)))
roc_svm <- roc(testing_data$classification, test_predictions_svm[,2], levels = rev(levels(testing_data$classification)))
test_predictions_glm <- predict(model_glm, testing_data, type = "prob")
roc_glm <- roc(testing_data$classification, test_predictions_glm[,2], levels = rev(levels(testing_data$classification)))
final_predictions_glm <- predict(meta_model_glm, testing_meta_data, type = "prob")
roc_meta_glm <- roc(testing_data$classification, test_predictions_glm[,2], levels = rev(levels(testing_data$classification)))

# Plot ROC curves
plot(roc_rf, col = "blue", main = "ROC Curves", lwd = 2)
lines(roc_svm, col = "red", lwd = 2)
lines(roc_glm, col = "purple", lwd = 5)
lines(roc_meta_glm, col = "green", lwd = 2)
legend("bottomright", legend = c("Random Forest", "SVM", "GLM", "Meta-GLM"), col = c("blue", "red", "purple", "green"), lwd = 2)

# Print AUC values
cat("\nAUC for Random Forest:", auc(roc_rf))
cat("\nAUC for SVM:", auc(roc_svm))
cat("\nAUC for GLM:", auc(roc_glm))
cat("\nAUC for Meta-GLM:", auc(roc_meta_glm))
