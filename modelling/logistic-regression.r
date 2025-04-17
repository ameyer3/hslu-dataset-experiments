install.packages("stringi")
install.packages("tidyr")
install.packages("broom")
install.packages("dbplyr")
install.packages("modelr")
install.packages("tidyverse")
install.packages("nnet")

library(MASS)
library(ISLR)
library(tidyverse)
library(nnet)

linked_data <- read.csv("linking/currents_with_microplastics.csv")
# north pacific
linked_data <- linked_data %>%
    filter((lon >= -180 & lon <= -120) & lat >=5 & lat <= 40)

# --------------------- Step 1: Data Splitting ---------------------

set.seed(123)
train_index <- sample(1:nrow(linked_data), 0.7 * nrow(linked_data))
train_data <- linked_data[train_index, ]
test_data <- linked_data[-train_index, ]

# Convert Concentration.Class to a factor with ordered levels (if applicable)
train_data$Concentration.Class <- factor(train_data$Concentration.Class, levels = c("Very Low", "Low", "Medium", "High", "Very High"), ordered = FALSE)
test_data$Concentration.Class <- factor(test_data$Concentration.Class, levels = c("Very Low", "Low", "Medium", "High", "Very High"), ordered = FALSE)

table(train_data$Concentration.Class)
table(test_data$Concentration.Class)

# --------------------- Step 2: Exploratory Data Analysis (on Training Data) ---------------------

# distribution of our target variable in the training set
ggplot(train_data, aes(x = Concentration.Class, fill = Concentration.Class)) +
  geom_bar() +
  labs(title = "Distribution of Microplastic Concentration Classes (Training Data)")

# potential relationships
plot_numerical_by_class <- function(data, variable, title) {
  ggplot(data, aes(x = Concentration.Class, y = .data[[variable]], fill = Concentration.Class)) +
    geom_boxplot() +
    labs(title = title, y = variable)
}

plot_numerical_by_class(train_data, "speed_avg", "Average Speed by Concentration Class (Training)")
plot_numerical_by_class(train_data, "speed_sum", "Total Speed by Concentration Class (Training)")
plot_numerical_by_class(train_data, "ve_avg", "Average Eastward Velocity by Concentration Class (Training)")
plot_numerical_by_class(train_data, "vn_avg", "Average Northward Velocity by Concentration Class (Training)")
plot_numerical_by_class(train_data, "buoy_count", "Buoy Count by Concentration Class (Training)")
plot_numerical_by_class(train_data, "measurement_count", "Measurement Count by Concentration Class (Training)")
plot_numerical_by_class(train_data, "lon", "Longitude by Concentration Class (Training)")
plot_numerical_by_class(train_data, "lat", "Latitude by Concentration Class (Training)")

# Summary statistics on the training data
train_data %>%
  group_by(Concentration.Class) %>%
  summarise(
    mean_speed_avg = mean(speed_avg, na.rm = TRUE),
    mean_speed_sum = mean(speed_sum, na.rm = TRUE),
    mean_ve_avg = mean(ve_avg, na.rm = TRUE),
    mean_vn_avg = mean(vn_avg, na.rm = TRUE),
    mean_buoy_count = mean(buoy_count, na.rm = TRUE),
    mean_measurement_count = mean(measurement_count, na.rm = TRUE),
    mean_lon = mean(lon, na.rm = TRUE),
    mean_lat = mean(lat, na.rm = TRUE),
    n = n()
  )

# --------------------- Step 3: Building Logistic Regression Models (on Training Data) ---------------------

# add more predictors each time
model_1 <- multinom(Concentration.Class ~ measurement_count, data = train_data)
summary(model_1)

model_2 <- multinom(Concentration.Class ~ measurement_count + speed_sum, data = train_data)
summary(model_2)

model_3 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count, data = train_data)
summary(model_3)

model_4 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  ve_avg + vn_avg, data = train_data)
summary(model_4)

# --------------------- Step 4: Model Comparison (Optional but Recommended) ---------------------

# We can use ANOVA-like tests for comparing nested multinomial models
anova(model_1, model_2)
anova(model_2, model_3)
anova(model_3, model_4)

# Lower AIC and BIC generally indicate a better model fit (though not a formal test here)
AIC(model_1, model_2, model_3, model_4)
BIC(model_1, model_2, model_3, model_4)

# --------------------- Step 5: Evaluating the Best Model on the Test Data ---------------------

best_model <- model_4

# Predict probabilities on the test data
predicted_probabilities_test <- predict(best_model, newdata = test_data, type = "probs")
head(predicted_probabilities_test)

# Predict the class based on the highest probability for the test data
predicted_classes_test <- predict(best_model, newdata = test_data)
table(predicted_classes_test, test_data$Concentration.Class)

# Calculate accuracy on the test data
accuracy_test <- mean(predicted_classes_test == test_data$Concentration.Class)
cat(paste("Accuracy of the Best Model on Test Data:", round(accuracy_test, 3), "\n"))

# --------------------- Step 6: Further Exploration and Model Extension ---------------------

# Now you can explore more complex models or visualizations,
# always keeping in mind the split data. For example, if you want
# to try interaction terms, you would train that model on the
# training data and then evaluate it on the test data.

# Example of an interaction model (trained on training data):
# No lets let some stuff interact TODO
model_interaction_train <- multinom(Concentration.Class ~ speed_avg * ve_avg + buoy_count, data = train_data)
predicted_classes_interaction_test <- predict(model_interaction_train, newdata = test_data)
accuracy_interaction_test <- mean(predicted_classes_interaction_test == test_data$Concentration.Class)
cat(paste("Accuracy of Interaction Model on Test Data:", round(accuracy_interaction_test, 3), "\n"))

# --------------------- 2. Interaction Terms ---------------------

# Model with interaction between average speed and eastward velocity
model_interaction_1 <- lm(Concentration.Numerical ~ speed_avg * ve_avg, data = linked_data_clean)
summary(model_interaction_1)

# Model with interaction between average speed and northward velocity
model_interaction_2 <- lm(Concentration.Numerical ~ speed_avg * vn_avg, data = linked_data_clean)
summary(model_interaction_2)

# Model with interaction between average speed and both velocity components
model_interaction_both <- lm(Concentration.Numerical ~ speed_avg * ve_avg * vn_avg, data = linked_data_clean)
summary(model_interaction_both)

# You can also include main effects along with interactions
model_interaction_with_main <- lm(Concentration.Numerical ~ speed_avg + ve_avg * vn_avg + buoy_count, data = linked_data_clean)
summary(model_interaction_with_main)

# Compare models with and without interactions
anova(model_extended, model_interaction_1)
anova(model_extended, model_interaction_with_main)

# --------------------- 3. Transformations of Variables ---------------------

# Log transformation of a positively skewed predictor (example: speed_sum - check its distribution)
ggplot(linked_data_clean, aes(x = speed_sum)) + geom_histogram() # Check skewness
model_log_speed_sum <- lm(Concentration.Numerical ~ log(speed_sum), data = linked_data_clean)
summary(model_log_speed_sum)

# Polynomial transformation (example: quadratic effect of average speed)
model_poly_speed_avg <- lm(Concentration.Numerical ~ speed_avg + I(speed_avg^2), data = linked_data_clean)
summary(model_poly_speed_avg)

# Combining transformations and other predictors
model_transformed_extended <- lm(Concentration.Numerical ~ log(speed_sum) + poly(speed_avg, 2) + ve_avg, data = linked_data_clean)
summary(model_transformed_extended)

# Comparing models with and without transformations
anova(model_extended, model_log_speed_sum)
anova(model_extended, model_poly_speed_avg)
anova(model_extended, model_transformed_extended)
# --------------------- Step 7: Visualization of Results (on Test Data Predictions) ---------------------

# You can create plots to visualize the predicted probabilities or class boundaries on the test set
# ... (similar visualization code as before, but using test_data and predictions on test_data) ...

ggplot(test_data, aes(x = speed_avg, fill = predicted_classes_test)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Concentration.Class) +
  labs(title = "Predicted vs. Actual Concentration by Average Speed (Test Data)")

# ... (other visualizations using test_data and predictions on test_data) ...


# Now you can start experimenting by:

# 1. Trying different combinations of predictors in your `lm()` formula.
# 2. Adding interaction terms between different pairs (or triplets) of predictors.
# 3. Applying different transformations (log, square root, polynomial) to various predictors based on their distributions and your hypotheses.
# 4. Combining more predictors with interaction and transformation terms.

# Example of a more complex model to start playing with:
complex_model <- lm(Concentration.Numerical ~ speed_avg * log(buoy_count + 1) + poly(vn_avg, 3) + ve_avg, data = linked_data_clean)
summary(complex_model)

# Remember to always check the summary of your models (R-squared, adjusted R-squared, p-values of coefficients)
# and use tools like `anova()` to compare nested models. Visualizing your data and model predictions
# (e.g., using ggplot2) is also crucial for understanding the relationships and the model fit.