library(readr)
library(caTools)
library(e1071)
library(pROC)

# Read the csv file
dataset <- read.csv("./Social_Network_Ads.csv")
dataset <- dataset[, 3:5]

# Data Exploration
print(head(dataset))
print(str(dataset))
print(summary(dataset))

# .....................Data Exploration Done........................

# Data cleaning
missing_values <- colSums(is.na(dataset))
print(missing_values)

# Apply na.omit to remove rows with missing values
social_network_ads <- na.omit(dataset)

# .....................Data cleaning Done............................

# EDA Exploratory Data Analysis
pdf('ScatterPlot.pdf', width=5, height=5) # nolint
plot(dataset$Age, dataset$EstimatedSalary, main = "Age vs. Estimated Salary", xlab = "Age", ylab = "Estimated Salary") # nolint
dev.off()

# Distribution Analysis
pdf('Distribution.pdf', width=10, height=5) # nolint
par(mfrow = c(2,2)) # nolint
hist(dataset$Age, main = "Age Distribution", xlab = "Age")
hist(dataset$EstimatedSalary, main = "Estimated Salary Distribution", xlab = "Estimated Salary") # nolint
dev.off()

# Correlation Analysis
# The correlation is 0.15 indicate that weak positive relationship.
print(cor(dataset$Age, dataset$EstimatedSalary))

# Boxplot for every column
pdf('Detect outliers.pdf', width=10, height=5) # nolint
par(mfrow = c(2,2)) # nolint
boxplot(dataset$Age, main = "Age Box Plot")
boxplot(dataset$EstimatedSalary, main = "Estimated Salary Box Plot")
dev.off()

# ............................................... EDA Done ........................ # nolint

# Feature Scaling
dataset$Age <- scale(dataset$Age)
dataset$EstimatedSalary <- scale(dataset$EstimatedSalary)

# ....................Feature Scaling done..............................  # nolint

# Split the dataset into the train_test split
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split== TRUE) # nolint
test_set <- subset(dataset, split== FALSE) # nolint

# ......................Data Validation............................  # nolint

# Train the model
classifier <- naiveBayes(x = training_set[-3], y = training_set$Purchased)

# .............................Training Done.......................

#Prediction on test data
y_pred <- predict(classifier, newdata = test_set[-3])

#.............................Prediction Done......................

# Draw the confusion matrix
cm <- table(test_set[, 3], y_pred)

# ..........................Confusion matrix.......................

# Accuracy of the model
accuracy <- sum(diag(cm)) / sum(cm)
# Print the confusion matrix and accuracy
print(cm)
cat("Accuracy:", accuracy)

# .........................Accuracy of the model...................


# Calculate ROC curve of the model
predictions <- predict(classifier, newdata = training_set)
roc_obj <- roc(as.numeric(training_set$Purchased), as.numeric(predictions))
pdf('RocCurve.pdf', width=10, height=5) # nolint
par(mfrow = c(2,2)) # nolint
plot(roc_obj, print.auc = TRUE, main = "ROC Curve")
dev.off()

#...........................Receiver Operating Characteristic Draw...........

# Find the Auc_Score
auc_score <- auc(roc_obj)
# Print the AUC score
cat("AUC Score:", auc_score, "\n")

#................................Auc_Score.............................