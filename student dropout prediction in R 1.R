# Student Dropout Prediction Mini Project

students <- data.frame(
  Attendance = c(90, 45, 75, 30, 85,
                 50, 95, 40, 70, 25),

  Previous_Grade = c(80, 35, 65, 25, 78,
                     40, 90, 30, 60, 20),

  Study_Hours = c(5, 1, 3, 1, 4,
                  2, 6, 1, 3, 1),

  Target = c(
    "Graduate", "Dropout", "Graduate",
    "Dropout", "Graduate", "Dropout",
    "Graduate", "Dropout", "Graduate",
    "Dropout"
  )
)

# Display the dataset
print(students)

# Show the first six records
head(students)

# Show the number of rows and columns
dim(students)


# Step 4: Data Cleaning

# Check column names
names(students)

# Check missing values
colSums(is.na(students))

# Convert Target into factor
students$Target <- as.factor(students$Target)

# Count student outcomes
table(students$Target)

install.packages("ggplot2")

library(ggplot2)

ggplot(students, aes(x = Target)) +
  geom_bar() +
  labs(
    title = "Student Outcome Distribution",
    x = "Student Outcome",
    y = "Number of Students"
  ) +
  theme_minimal()

# Attendance Graph

ggplot(students, aes(x = Target, y = Attendance)) +
  geom_boxplot() +
  labs(
    title = "Attendance by Student Outcome",
    x = "Student Outcome",
    y = "Attendance (%)"
  ) +
  theme_minimal()

# Logistic Regression Model

model <- glm(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  family = binomial
)

# Display model results
summary(model)

# Step 8: Predict a New Student's Outcome

new_student <- data.frame(
  Attendance = 80,
  Previous_Grade = 70,
  Study_Hours = 4
)

# Predict probability
probability <- predict(
  model,
  newdata = new_student,
  type = "response"
)

# Display probability
print(probability)

# Predict student outcome

if (probability >= 0.5) {
  print("Graduate")
} else {
  print("Dropout")
}

# Predict outcomes for existing data

predicted <- predict(
  model,
  newdata = students,
  type = "response"
)

predicted_class <- ifelse(
  predicted >= 0.5,
  "Graduate",
  "Dropout"
)

# Compare actual and predicted outcomes

comparison <- data.frame(
  Actual = students$Target,
  Predicted = predicted_class
)

print(comparison)

# Calculate accuracy

accuracy <- mean(
  comparison$Actual == comparison$Predicted
)

print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


# Install Decision Tree package
install.packages("rpart")

# Load package
library(rpart)

# Build Decision Tree model

tree_model <- rpart(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  method = "class"
)

# Display model results
print(tree_model)

# Install plotting package
install.packages("rpart.plot")

# Load package
library(rpart.plot)

# Draw Decision Tree
rpart.plot(
  tree_model,
  main = "Student Dropout Prediction Decision Tree"
)

# Logistic Regression predictions
log_pred <- ifelse(
  predict(model, newdata = students, type = "response") >= 0.5,
  "Graduate",
  "Dropout"
)

log_accuracy <- mean(log_pred == students$Target)

# Decision Tree predictions
tree_pred <- predict(
  tree_model,
  newdata = students,
  type = "class"
)

tree_accuracy <- mean(tree_pred == students$Target)

# Compare results
results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree"),
  Accuracy = round(
    c(log_accuracy, tree_accuracy) * 100, 2
  )
)

print(results)

# Step 13: Final Results Table

results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree"),
  Accuracy = round(
    c(log_accuracy, tree_accuracy) * 100,
    2
  )
)



print(results)

# Confusion Matrix: Logistic Regression
log_confusion <- table(
  Actual = students$Target,
  Predicted = log_pred
)

print(log_confusion)

# Confusion Matrix: Decision Tree
tree_confusion <- table(
  Actual = students$Target,
  Predicted = tree_pred
)

print(tree_confusion)

# STEP 14: LOGISTIC REGRESSION PREDICTION

probability <- predict(
  log_model,
  newdata = new_student,
  type = "response"
)

print(probability)


# STEP 15: FINAL PREDICTION

if (probability >= 0.5) {
  prediction <- "Graduate"
} else {
  prediction <- "Dropout"
}

print(prediction)

















# Decision Tree Model - Updated

library(rpart)
library(rpart.plot)

tree_model <- rpart(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  method = "class",
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    cp = 0.001,
    maxdepth = 5
  )
)

# Display model
print(tree_model)

# Plot Decision Tree
rpart.plot(
  tree_model,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Student Dropout Prediction Decision Tree"
)























# ============================================================
# STUDENT DROPOUT PREDICTION MINI PROJECT
# Language: R
# Algorithms: Logistic Regression and Decision Tree
# ============================================================


# STEP 1: CREATE DATASET

students <- data.frame(
  Attendance = c(90, 45, 75, 30, 85,
                 50, 95, 40, 70, 25),
  
  Previous_Grade = c(80, 35, 65, 25, 78,
                     40, 90, 30, 60, 20),
  
  Study_Hours = c(5, 1, 3, 1, 4,
                  2, 6, 1, 3, 1),
  
  Target = c(
    "Graduate", "Dropout", "Graduate",
    "Dropout", "Graduate", "Dropout",
    "Graduate", "Dropout", "Graduate",
    "Dropout"
  )
)


# STEP 2: DISPLAY DATASET

print(students)

# First six records
head(students)

# Number of rows and columns
dim(students)

# Column names
names(students)


# STEP 3: DATA CLEANING

# Check missing values
colSums(is.na(students))

# Set outcome levels explicitly
students$Target <- factor(
  students$Target,
  levels = c("Dropout", "Graduate")
)

# Count student outcomes
table(students$Target)


# STEP 4: INSTALL AND LOAD PACKAGES
# Install each package only if it is not already installed.

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}

if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}

library(ggplot2)
library(rpart)
library(rpart.plot)


# STEP 5: STUDENT OUTCOME BAR GRAPH

ggplot(students, aes(x = Target)) +
  geom_bar() +
  labs(
    title = "Student Outcome Distribution",
    x = "Student Outcome",
    y = "Number of Students"
  ) +
  theme_minimal()


# STEP 6: ATTENDANCE BOXPLOT

ggplot(students, aes(x = Target, y = Attendance)) +
  geom_boxplot() +
  labs(
    title = "Attendance by Student Outcome",
    x = "Student Outcome",
    y = "Attendance (%)"
  ) +
  theme_minimal()


# STEP 7: LOGISTIC REGRESSION MODEL

model <- glm(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  family = binomial
)

# Display model summary
summary(model)

model <- glm(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  family = binomial
)

summary(model)


# STEP 8: PREDICT A NEW STUDENT'S OUTCOME

new_student <- data.frame(
  Attendance = 80,
  Previous_Grade = 70,
  Study_Hours = 4
)

# Probability of Graduate
probability <- predict(
  model,
  newdata = new_student,
  type = "response"
)

print(probability)

# Classify the new student
if (probability >= 0.5) {
  print("Predicted Outcome: Graduate")
} else {
  print("Predicted Outcome: Dropout")
}


# STEP 9: LOGISTIC REGRESSION PREDICTIONS

log_probability <- predict(
  model,
  newdata = students,
  type = "response"
)

log_pred <- factor(
  ifelse(log_probability >= 0.5,
         "Graduate", "Dropout"),
  levels = levels(students$Target)
)

# Compare actual and predicted values
log_comparison <- data.frame(
  Actual = students$Target,
  Predicted = log_pred
)

print(log_comparison)


# STEP 10: LOGISTIC REGRESSION CONFUSION MATRIX

log_confusion <- table(
  Actual = students$Target,
  Predicted = log_pred
)

print(log_confusion)

# Training accuracy
log_accuracy <- mean(
  log_pred == students$Target
)

print(paste(
  "Logistic Regression Training Accuracy:",
  round(log_accuracy * 100, 2), "%"
))


# STEP 11: BUILD DECISION TREE MODEL

tree_model <- rpart(
  Target ~ Attendance + Previous_Grade + Study_Hours,
  data = students,
  method = "class",
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    cp = 0,
    maxdepth = 5,
    xval = 0
  )
)

# Display tree model
print(tree_model)

printcp(tree_model)

summary(tree_model)

# STEP 12: Updated plotting code
png(
  "Decision_Tree.png",
  width = 1200,
  height = 800,
  res = 120
)

par(mar = c(2, 2, 4, 2))

rpart.plot(
  tree_model,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "GnBu",
  shadow.col = "gray",
  branch.col = "darkblue",
  tweak = 2,
  cex = 1.3,
  main = "Student Dropout Prediction Decision Tree"
)

dev.off()

# STEP 13: DECISION TREE PREDICTIONS

tree_pred <- predict(
  tree_model,
  newdata = students,
  type = "class"
)

tree_comparison <- data.frame(
  Actual = students$Target,
  Predicted = tree_pred
)

print(tree_comparison)


# STEP 14: DECISION TREE CONFUSION MATRIX

tree_confusion <- table(
  Actual = students$Target,
  Predicted = tree_pred
)

print(tree_confusion)

# Training accuracy
tree_accuracy <- mean(
  tree_pred == students$Target
)

print(paste(
  "Decision Tree Training Accuracy:",
  round(tree_accuracy * 100, 2), "%"
))


# STEP 15: COMPARE BOTH MODELS

results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree"),
  Training_Accuracy = round(
    c(log_accuracy, tree_accuracy) * 100,
    2
  )
)

print(results)


# STEP 16: FINAL PROJECT SUMMARY

cat("\n===== STUDENT DROPOUT PREDICTION RESULTS =====\n")

cat("Total Students:", nrow(students), "\n")

cat("Dropout Students:",
    sum(students$Target == "Dropout"), "\n")

cat("Graduate Students:",
    sum(students$Target == "Graduate"), "\n")

cat("Logistic Regression Training Accuracy:",
    round(log_accuracy * 100, 2), "%\n")

cat("Decision Tree Training Accuracy:",
    round(tree_accuracy * 100, 2), "%\n")

cat("\nProject execution completed.\n")

