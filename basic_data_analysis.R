# Load necessary packages
# Install them if not already installed
if(!require(qualtRics)) install.packages("qualtRics")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(broom)) install.packages("broom") # For tidying model outputs
if(!require(psych)) install.packages("psych") # For describing data
if(!require(lm.beta)) install.packages("lm.beta") # For standardized beta in regression
if(!require(mediation)) install.packages("mediation") # For mediation analysis

library(qualtRics)
library(tidyverse)
library(broom)
library(psych)
library(lm.beta)
library(mediation)

rm(list = ls()) # Clean the working environment

# Sets the working directory to the folder containing this file
# You will be able to use relative paths to your data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Import Qualtrics data
# Replace 'your_file.csv' with the actual path to your Qualtrics export
data <- read_survey("../../your_file.csv")

# Basic data filtering (e.g., attention checks, incomplete responses)
# Assuming attention checks are named 'AttentionCheck1', 'AttentionCheck2'
filtered_data <- data %>%
  filter(!is.na(ResponseID),  # Remove incomplete responses
         AttentionCheck1 == 'Pass',  # Filter participants who passed attention checks
         AttentionCheck2 == 'Pass')

# Basic data transformations
# Ensure correct variable types (factors, numeric, etc.)
# Convert selected columns to factors and numerics if needed
filtered_data <- filtered_data %>%
  mutate(across(where(is.character), as.factor),  # Convert character variables to factors
         across(where(is.factor), as.numeric, .names = "num_{col}"))  # Convert factors to numerics if needed

# Reshape data (wide to long format for within-subjects designs)
# Assuming multiple responses like Time1, Time2, etc.
long_data <- filtered_data %>%
  pivot_longer(cols = starts_with("Time"),  # Use column prefixes like Time1, Time2
               names_to = "TimePoint", 
               values_to = "Response", 
               names_prefix = "Time") %>%
  group_by(ResponseID)  # Include ResponseID as a grouping variable

# Inspecting data distribution for key variables
# Descriptive statistics
describe(filtered_data$YourKeyVariable)  # Replace 'YourKeyVariable' with the actual variable name
ggplot(filtered_data, aes(x = YourKeyVariable)) +
  geom_histogram(binwidth = 0.5) +  # Adjust binwidth as needed
  labs(title = "Distribution of Key Variable", x = "Key Variable", y = "Frequency")

# T-Tests
# Between-subjects t-test (e.g., compare two groups)
t_test_between <- t.test(YourDV ~ YourIV, data = filtered_data)  # Replace with your DV and IV
tidy(t_test_between)

# Within-subjects t-test (e.g., compare repeated measures)
t_test_within <- t.test(Response ~ TimePoint, data = long_data, paired = TRUE)
tidy(t_test_within)

# T-test for difference from a midpoint (e.g., test if means differ from 0)
t_test_midpoint <- t.test(filtered_data$YourKeyVariable, mu = 0)  # Replace with your variable and midpoint
tidy(t_test_midpoint)

# ANOVAs
# One-way ANOVA (between-subjects)
anova_between <- aov(YourDV ~ YourIV, data = filtered_data)
summary(anova_between)

# Repeated-measures ANOVA (within-subjects)
anova_within <- aov(Response ~ TimePoint + Error(ResponseID/TimePoint), data = long_data)
summary(anova_within)

# Nonparametric tests
# Wilcoxon rank-sum test (non-parametric equivalent of between-subjects t-test)
wilcox_test_between <- wilcox.test(YourDV ~ YourIV, data = filtered_data)
tidy(wilcox_test_between)

# Wilcoxon signed-rank test (non-parametric equivalent of within-subjects t-test)
wilcox_test_within <- wilcox.test(Response ~ TimePoint, data = long_data, paired = TRUE)
tidy(wilcox_test_within)

# Regression analysis
# Simple linear regression
lm_model <- lm(YourDV ~ YourIV, data = filtered_data)
summary(lm_model)
lm.beta(lm_model)  # Get standardized beta coefficients

# Proportions test (e.g., test if proportions differ from chance, 0.5)
prop_test <- prop.test(x = sum(filtered_data$YourBinaryVariable == 1),  # Number of successes (1s)
                       n = nrow(filtered_data),  # Total number of observations
                       p = 0.5)  # Chance level (e.g., 50%)
tidy(prop_test)

# Mediation analysis
# Assuming variables: IV (X), mediator (M), DV (Y)
model.m <- lm(Mediator ~ IV, data = filtered_data)
model.y <- lm(DV ~ IV + Mediator, data = filtered_data)
mediation_model <- mediate(model.m, model.y, treat = "IV", mediator = "Mediator")
summary(mediation_model)

# Saving the results
# Save t-test and regression summaries to file if needed
write.csv(tidy(t_test_between), file = "t_test_between_results.csv")
write.csv(tidy(lm_model), file = "regression_results.csv")

