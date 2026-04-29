# =============================
# House Rent Prediction Project
# R Programming
# =============================

# Install packages (Run only once)
install.packages("caTools")
install.packages("ggplot2")
install.packages("caret")

# Load libraries
library(caTools)
library(ggplot2)
library(caret)

# =============================
# Step 1: Load Dataset
# =============================

# Replace with your dataset file name
data <- read.csv("house_rent.csv")

# Display first rows
head(data)

# =============================
# Step 2: Data Preprocessing
# =============================

# Remove missing values
data <- na.omit(data)

# Convert categorical columns into factors
data$Location <- as.factor(data$Location)
data$Furnishing <- as.factor(data$Furnishing)

# =============================
# Step 3: Split Dataset
# =============================

set.seed(123)

split <- sample.split(data$Rent, SplitRatio = 0.8)

train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# =============================
# Step 4: Train Model
# =============================

model <- lm(Rent ~ Area + BHK + Bathroom + Location + Furnishing,
            data = train_data)

# Model summary
summary(model)

# =============================
# Step 5: Predict Rent
# =============================

predictions <- predict(model, newdata = test_data)

# =============================
# Step 6: Model Evaluation
# =============================

# RMSE Calculation
rmse <- sqrt(mean((test_data$Rent - predictions)^2))

print(paste("RMSE:", rmse))

# =============================
# Step 7: Visualization
# =============================

# Actual vs Predicted Graph
results <- data.frame(
  Actual = test_data$Rent,
  Predicted = predictions
)

ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Actual vs Predicted House Rent") +
  xlab("Actual Rent") +
  ylab("Predicted Rent")

# =============================
# Step 8: Predict New House Rent
# =============================

new_house <- data.frame(
  Area = 1200,
  BHK = 2,
  Bathroom = 2,
  Location = factor("Delhi", levels = levels(data$Location)),
  Furnishing = factor("Semi-Furnished", levels = levels(data$Furnishing))
)

predicted_rent <- predict(model, newdata = new_house)

print(paste("Predicted House Rent:", predicted_rent))

# =============================
# End of Project
# =============================