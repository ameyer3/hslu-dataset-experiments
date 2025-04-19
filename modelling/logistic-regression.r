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

# --------------------- Step 2: Exploratory Data Analysis ---------------------

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

# it gets less (anti correlation?)
plot_numerical_by_class(train_data, "speed_avg", "Average Speed by Concentration Class (Training)")
# rises
plot_numerical_by_class(train_data, "speed_sum", "Total Speed by Concentration Class (Training)")
# not much
plot_numerical_by_class(train_data, "ve_avg", "Average Eastward Velocity by Concentration Class (Training)")
# even less
plot_numerical_by_class(train_data, "vn_avg", "Average Northward Velocity by Concentration Class (Training)")
# somehow rising but very low is also higher
plot_numerical_by_class(train_data, "buoy_count", "Buoy Count by Concentration Class (Training)")
# clear rise
plot_numerical_by_class(train_data, "measurement_count", "Measurement Count by Concentration Class (Training)")
# nothin much
plot_numerical_by_class(train_data, "lon", "Longitude by Concentration Class (Training)")
# rising somehow
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

model_5 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  lat + lon, data = train_data)
summary(model_5)

model_6 <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count +  ve_avg + vn_avg + lat +lon, data = train_data)
summary(model_6)

# --------------------- Step 4: Model Comparison ----------------------

# We can use ANOVA-like tests for comparing nested multinomial models
anova(model_1, model_2)
anova(model_2, model_3)
anova(model_3, model_4)
anova(model_4, model_5)
anova(model_5, model_6)
anova(model_1, model_2, model_3, model_4, model_5, model_6)



# Lower AIC and BIC generally indicate a better model fit (though not a formal test here)
AIC(model_1, model_2, model_3, model_4, model_5, model_6)
BIC(model_1, model_2, model_3, model_4, model_5, model_6)

# --------------------- Step 5: Evaluating the Best Model on the Test Data ---------------------

best_model <- model_3

# Predict probabilities on the test data
predicted_probabilities_test <- predict(best_model, newdata = test_data, type = "probs")
head(predicted_probabilities_test)

# Predict the class based on the highest probability for the test data
predicted_classes_test <- predict(best_model, newdata = test_data)
table(predicted_classes_test, test_data$Concentration.Class)

# Calculate accuracy on the test data
accuracy_test <- mean(predicted_classes_test == test_data$Concentration.Class)
cat(paste("Accuracy of the Best Model on Test Data:", round(accuracy_test, 3), "\n"))


best_model <- multinom(Concentration.Class ~ measurement_count + speed_sum + buoy_count, data = train_data)

ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_test)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Concentration.Class) +
  labs(title = "Predicted vs. Actual Concentration by Average Speed (Test Data)")

# --------------------- Step 6: Visualization---------------------

# 1. Choose a Predictor to Visualize Against: measurement_count
predictor_to_visualize <- "measurement_count"

# 2. Create a Range of Values
measurement_range <- seq(min(test_data[[predictor_to_visualize]], na.rm = TRUE),
                         max(test_data[[predictor_to_visualize]], na.rm = TRUE),
                         length.out = 100)

# 3. Create a New Data Frame for Prediction
# Hold other predictors at their mean
mean_speed_sum <- mean(test_data$speed_sum, na.rm = TRUE)
mean_buoy_count <- mean(test_data$buoy_count, na.rm = TRUE)

new_data_predict <- data.frame(
  measurement_count = measurement_range,
  speed_sum = mean_speed_sum,
  buoy_count = mean_buoy_count
)

# 4. Predict Probabilities
predicted_probabilities <- predict(best_model, newdata = new_data_predict, type = "probs")

# Convert to a long format for ggplot2
predicted_probs_long <- predicted_probabilities %>%
  as_tibble() %>%
  mutate(measurement_count = measurement_range) %>%
  pivot_longer(cols = -measurement_count, names_to = "Concentration.Class", values_to = "Probability")

# 5. Visualize the Predicted Probabilities
ggplot(predicted_probs_long, aes(x = measurement_count, y = Probability, color = Concentration.Class)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Predicted Probabilities vs.", predictor_to_visualize),
    x = "Measurement Count",
    y = "Predicted Probability"
  ) +
  theme_bw()



# 1. Choose a Predictor to Visualize Against: speed_sum
predictor_to_visualize <- "speed_sum"

# 2. Create a Range of Values
speed_sum_range <- seq(min(test_data[[predictor_to_visualize]], na.rm = TRUE),
                       max(test_data[[predictor_to_visualize]], na.rm = TRUE),
                       length.out = 100)

# 3. Create a New Data Frame for Prediction
new_data_predict_speed <- data.frame(
  measurement_count = mean(test_data$measurement_count, na.rm = TRUE),
  speed_sum = speed_sum_range,
  buoy_count = mean(test_data$buoy_count, na.rm = TRUE)
)

# 4. Predict Probabilities
predicted_probabilities_speed <- predict(best_model, newdata = new_data_predict_speed, type = "probs")

# Convert to a long format for ggplot2
predicted_probs_long_speed <- predicted_probabilities_speed %>%
  as_tibble() %>%
  mutate(speed_sum = speed_sum_range) %>%
  pivot_longer(cols = -speed_sum, names_to = "Concentration.Class", values_to = "Probability")

# 5. Visualize the Predicted Probabilities
ggplot(predicted_probs_long_speed, aes(x = speed_sum, y = Probability, color = Concentration.Class)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Predicted Probabilities vs.", predictor_to_visualize),
    x = "Total Speed",
    y = "Predicted Probability"
  ) +
  theme_bw()

# 1. Choose a Predictor to Visualize Against: buoy_count
predictor_to_visualize <- "buoy_count"

# 2. Create a Range of Values
buoy_count_range <- seq(min(test_data[[predictor_to_visualize]], na.rm = TRUE),
                         max(test_data[[predictor_to_visualize]], na.rm = TRUE),
                         length.out = 100)

# 3. Create a New Data Frame for Prediction
new_data_predict_buoy <- data.frame(
  measurement_count = mean(test_data$measurement_count, na.rm = TRUE),
  speed_sum = mean(test_data$speed_sum, na.rm = TRUE),
  buoy_count = buoy_count_range
)

# 4. Predict Probabilities
predicted_probabilities_buoy <- predict(best_model, newdata = new_data_predict_buoy, type = "probs")

# Convert to a long format for ggplot2
predicted_probs_long_buoy <- predicted_probabilities_buoy %>%
  as_tibble() %>%
  mutate(buoy_count = buoy_count_range) %>%
  pivot_longer(cols = -buoy_count, names_to = "Concentration.Class", values_to = "Probability")

# 5. Visualize the Predicted Probabilities
ggplot(predicted_probs_long_buoy, aes(x = buoy_count, y = Probability, color = Concentration.Class)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Predicted Probabilities vs.", predictor_to_visualize),
    x = "Buoy Count",
    y = "Predicted Probability"
  ) +
  theme_bw()

# -------------------------------------------------------------------------------------------
# Above: purely linear but good predicators found i think
# -------------------------------------------------------------------------------------------


# --------------------- Interaction Terms ---------------------
# measurement_count + speed_sum + buoy_count +  ve_avg + vn_avg

model_interaction_1 <- multinom(Concentration.Class ~ measurement_count * speed_sum, data = train_data)
summary(model_interaction_1)

model_interaction_2 <- multinom(Concentration.Class ~ measurement_count * buoy_count * speed_sum, data = train_data)
summary(model_interaction_2)

model_interaction_3 <- multinom(Concentration.Class ~ measurement_count * buoy_count + speed_sum, data = train_data)
summary(model_interaction_3)

model_interaction_4 <- multinom(Concentration.Class ~ measurement_count * speed_sum + buoy_count, data = train_data)
summary(model_interaction_4)

anova(best_model, model_interaction_1)
anova(best_model, model_interaction_2)
anova(best_model, model_interaction_3)
anova(best_model, model_interaction_4)
anova(best_model, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)

AIC(best_model, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)
BIC(best_model, model_interaction_1, model_interaction_2, model_interaction_3, model_interaction_4)

best_interactive_model <- model_interaction_4

predicted_classes_interaction_test <- predict(best_interactive_model, newdata = test_data)
accuracy_interaction_test <- mean(predicted_classes_interaction_test == test_data$Concentration.Class)
cat(paste("Accuracy of Interaction Model on Test Data:", round(accuracy_interaction_test, 3), "\n"))

ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_interaction_test)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Concentration.Class) +
  labs(title = "Predicted vs. Actual Concentration by Average Speed (Test Data)")
table(predicted_classes_interaction_test, test_data$Concentration.Class)

# non-interactive actually had better accuracy but this one has a better visualised prediction

# Figure out if they actually interact
# 1. Choose Predictors to Visualize Against
predictor_x <- "measurement_count"
predictor_color <- "speed_sum"

# 2. Create a Range of Values for the Predictors
measurement_range <- seq(min(test_data[[predictor_x]], na.rm = TRUE),
                         max(test_data[[predictor_x]], na.rm = TRUE),
                         length.out = 50) # Reduce for interaction plot

speed_sum_range <- seq(min(test_data[[predictor_color]], na.rm = TRUE),
                       max(test_data[[predictor_color]], na.rm = TRUE),
                       length.out = 3) # Choose a few representative values for color

# 3. Create a New Data Frame for Prediction (with combinations of predictors)
new_data_predict_interaction <- expand.grid(
  measurement_count = measurement_range,
  speed_sum = speed_sum_range,
  buoy_count = mean(test_data$buoy_count, na.rm = TRUE) # Hold the other predictor at its mean
)

# 4. Predict Probabilities
predicted_probabilities_interaction <- predict(model_interaction_4, newdata = new_data_predict_interaction, type = "probs")

# Convert to a long format for ggplot2
predicted_probs_long_interaction <- predicted_probabilities_interaction %>%
  as_tibble() %>%
  mutate(measurement_count = new_data_predict_interaction$measurement_count,
         speed_sum = new_data_predict_interaction$speed_sum) %>%
  pivot_longer(cols = -c(measurement_count, speed_sum), names_to = "Concentration.Class", values_to = "Probability")

# 5. Visualize the Predicted Probabilities (faceted by speed_sum)
ggplot(predicted_probs_long_interaction,
       aes(x = measurement_count, y = Probability, color = Concentration.Class)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ speed_sum, labeller = label_bquote(speed_sum == .(round(speed_sum, 2)))) +
  labs(
    title = paste("Predicted Probabilities vs.", predictor_x, "by", predictor_color),
    x = "Measurement Count",
    y = "Predicted Probability",
    color = "Concentration Class"
  ) +
  theme_bw()


ggplot(predicted_probs_long_interaction,
       aes(x = measurement_count, y = Probability, color = Concentration.Class, linetype = factor(speed_sum))) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Predicted Probabilities vs.", predictor_x, "with", predictor_color, "Interaction"),
    x = "Measurement Count",
    y = "Predicted Probability",
    color = "Concentration Class",
    linetype = "Total Speed"
  ) +
  theme_bw()

# There seesm to be an interaction

#--------------------------------------------
# ABOVE: interaction expirements-------------
#--------------------------------------------

# --------------------- Transformations of Variables ---------------------

ggplot(train_data, aes(x = speed_sum)) + geom_histogram()s
# is right skewed, log or poly might help
model_log_speed_sum <- multinom(Concentration.Class ~ log(speed_sum), data = train_data)
summary(model_log_speed_sum)
ggplot(train_data, aes(x = log(speed_sum))) + geom_histogram()


model_poly_speed_sum <- multinom(Concentration.Class ~ speed_sum + I(speed_sum^2), data = train_data)
summary(model_poly_speed_sum)
# more stable than the previous
model_poly_speed_sum <- multinom(Concentration.Class ~ poly(speed_sum, 2), data = train_data)
summary(model_poly_speed_sum)
# Compare log and poly to see which is better

ggplot(train_data, aes(x = measurement_count)) + geom_histogram()
# more right skewed
model_poly_measurement_count <- multinom(Concentration.Class ~ measurement_count + I(measurement_count^2), data = train_data)
summary(model_poly_measurement_count)

model_log_measurement_count <- multinom(Concentration.Class ~ log(measurement_count), data = train_data)
summary(model_log_measurement_count)
ggplot(train_data, aes(x = log(measurement_count))) + geom_histogram()
# is a bit left skewed now but not too much


model_transformed_extended <- multinom(Concentration.Class ~ log(measurement_count) * poly(speed_sum, 2) + buoy_count, data = train_data)
summary(model_transformed_extended)

anova(best_interactive_model, model_log_speed_sum)
anova(best_interactive_model, model_poly_speed_sum)
anova(best_interactive_model, model_log_measurement_count)
anova(best_interactive_model, model_poly_measurement_count)
anova(best_interactive_model, model_transformed_extended)


predicted_classes_interaction_test <- predict(model_transformed_extended, newdata = test_data)
accuracy_interaction_test <- mean(predicted_classes_interaction_test == test_data$Concentration.Class)
cat(paste("Accuracy of Interaction Model on Test Data:", round(accuracy_interaction_test, 3), "\n"))

ggplot(test_data, aes(x = measurement_count, fill = predicted_classes_interaction_test)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Concentration.Class) +
  labs(title = "Predicted vs. Actual Concentration by Average Speed (Test Data)")

# confusino matrix
table(predicted_classes_interaction_test, test_data$Concentration.Class)
# -> does actually seem best


# 1. Trying different combinations of predictors in your `lm()` formula.
# 2. Adding interaction terms between different pairs (or triplets) of predictors.
# 3. Applying different transformations (log, square root, polynomial) to various predictors based on their distributions and your hypotheses.
# 4. Combining more predictors with interaction and transformation terms.
