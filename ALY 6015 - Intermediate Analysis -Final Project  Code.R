cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
#clears packages
options(scipen = 100) # disables scientific notation for entire R


# Load necessary libraries
library(pacman) # pacman package helps to manage packages (load/install)
p_load(tidyverse) # Loads tidyverse for data manipulation and visualization
p_load(janitor) # Loads janitor for data cleaning
library(ggplot2) # For creating plots
library(dplyr) # For data manipulation
library(tidyr) # For tidying data
library(reshape2) # For reshaping data
library(corrplot) # For correlation plots
library(stats)
library(randomForest)



loans_data <- read.csv("loans_full_schema.csv")



# To view the first few rows of the dataset
head(loans_data) 

# Check the structure
str(loans_data)

# Summary of the dataset
summary(loans_data)

# Check for missing values
colSums(is.na(loans_data))


# Subset the dataset to keep only the specified columns
loans_data_clean <- loans_data %>%
  select(emp_length, state, homeownership, annual_income, 
         debt_to_income, earliest_credit_line, inquiries_last_12m, 
         total_credit_lines, open_credit_lines, total_credit_limit, 
         total_credit_utilized, num_historical_failed_to_pay, 
         months_since_90d_late, current_installment_accounts, 
         num_satisfactory_accounts, num_accounts_120d_past_due, 
         num_accounts_30d_past_due, total_debit_limit, 
         num_total_cc_accounts, num_cc_carrying_balance, num_mort_accounts, 
         account_never_delinq_percent, loan_purpose, loan_amount, 
         term, interest_rate, grade)

# Replace NA in 'months_since_90d_late' with 0
loans_data_clean$months_since_90d_late[is.na(loans_data_clean$months_since_90d_late)] <- 0

# Handling missing values for numerical columns - fill with median
numerical_columns <- c("emp_length", "annual_income", "debt_to_income", "inquiries_last_12m", 
                       "total_credit_lines", "open_credit_lines", "total_credit_limit", 
                       "total_credit_utilized", "num_historical_failed_to_pay", "current_installment_accounts", 
                       "num_satisfactory_accounts", "num_accounts_120d_past_due", 
                       "num_accounts_30d_past_due", "total_debit_limit", "num_total_cc_accounts", 
                       "num_cc_carrying_balance", "num_mort_accounts", "account_never_delinq_percent", 
                       "loan_amount", "term", "interest_rate")

for (col in numerical_columns) {
  if (sum(is.na(loans_data_clean[[col]])) > 0) {
    loans_data_clean[[col]][is.na(loans_data_clean[[col]])] <- median(loans_data_clean[[col]], na.rm = TRUE)
  }
}

# Convert categorical columns to factor type
loans_data_clean$state <- as.factor(loans_data_clean$state)
loans_data_clean$homeownership <- as.factor(loans_data_clean$homeownership)
loans_data_clean$loan_purpose <- as.factor(loans_data_clean$loan_purpose)
loans_data_clean$grade <- as.factor(loans_data_clean$grade)
loans_data_clean$emp_length <- as.numeric(as.character(loans_data_clean$emp_length)) 

# Save the cleaned dataset
write.csv(loans_data_clean, "loans_data_clean.csv", row.names = FALSE)

# Output the first few rows of the cleaned dataset
head(loans_data_clean)


summary(loans_data_clean)

# Removing outliers based on 'annual_income'
annual_income_q1 <- quantile(loans_data_clean$annual_income, 0.25, na.rm = TRUE)
annual_income_q3 <- quantile(loans_data_clean$annual_income, 0.75, na.rm = TRUE)
annual_income_iqr <- IQR(loans_data_clean$annual_income, na.rm = TRUE)
# Define bounds
lower_bound <- annual_income_q1 - 1.5 * annual_income_iqr
upper_bound <- annual_income_q3 + 1.5 * annual_income_iqr
# Filter outliers
loans_data_clean <- loans_data_clean %>%
  filter(annual_income >= lower_bound & annual_income <= upper_bound)



# Removing outliers based on 'interest_rate'
IR_q1 <- quantile(loans_data_clean$interest_rate, 0.25, na.rm = TRUE)
IR_q3 <- quantile(loans_data_clean$interest_rate, 0.75, na.rm = TRUE)
IR_iqr <- IQR(loans_data_clean$interest_rate, na.rm = TRUE)
# Define bounds
lower_bound <- IR_q1 - 1.5 * IR_iqr
upper_bound <- IR_q3 + 1.5 * IR_iqr
# Filter outliers
loans_data_clean <- loans_data_clean %>%
  filter(interest_rate >= lower_bound & interest_rate <= upper_bound)



# Histograms for Numeric Variables: distribution of annual_income to see how income levels of borrowers are distributed.
ggplot(loans_data_clean, aes(x = annual_income)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Frequency")

# Boxplots for Numeric Variables by Categories: comparing interest_rate across different grades.
ggplot(loans_data_clean, aes(x = grade, y = interest_rate, fill = grade)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Interest Rate by Loan Grade", x = "Grade", y = "Interest Rate")


# Scatter Plots for Relationship Between Variables: plotting annual_income against loan_amount could reveal if higher incomes correspond to larger loans.
ggplot(loans_data_clean, aes(x = annual_income, y = loan_amount)) + 
  geom_point(alpha = 0.5) + 
  theme_minimal() + 
  labs(title = "Loan Amount vs. Annual Income", x = "Annual Income", y = "Loan Amount")




# EDA AND MODEL FOR QUESTION 2


# 1. Bar Plot for Loan Purpose by Homeownership
ggplot(loans_data_clean, aes(x = loan_purpose, fill = homeownership)) +
  geom_bar(position = "dodge") +
  labs(title = "Loan Purpose by Homeownership Status",
       x = "Loan Purpose",
       y = "Count of Loans") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Box Plot for Loan Amount by Homeownership
ggplot(loans_data_clean, aes(x = homeownership, y = loan_amount, fill = homeownership)) +
  geom_boxplot() +
  labs(title = "Distribution of Loan Amounts by Homeownership Status",
       x = "Homeownership Status",
       y = "Loan Amount") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()


# 3. Density Plot for Loan Amount by Loan Purpose
ggplot(loans_data_clean, aes(x = loan_amount, fill = loan_purpose)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density of Loan Amounts by Loan Purpose",
       x = "Loan Amount",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()




# Create a contingency table for homeownership status and loan purpose
contingency_table <- with(loans_data_clean, table(homeownership, loan_purpose))

# Conduct the Chi-Square test of independence
chi_sq_test <- chisq.test(contingency_table)

# Print the test results
print(chi_sq_test)


# Fit the ANOVA model to compare the mean loan amounts across homeownership status
aov_model <- aov(loan_amount ~ homeownership, data = loans_data_clean)

# Get a summary of the ANOVA model
summary(aov_model)





# EDA AND MODELS FOR QUESTION 1


# Visualization 1: Distribution of Interest Rates
ggplot(loans_data_clean, aes(x = interest_rate)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Interest Rates", x = "Interest Rate (%)", y = "Count") +
  theme_minimal()


# Visualization 2: Interest Rate vs. Annual Income
ggplot(loans_data_clean, aes(x = annual_income, y = interest_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Interest Rate vs. Annual Income", x = "Annual Income ($)", y = "Interest Rate (%)") +
  theme_minimal()


# Visualization 3: Interest Rate by Homeownership Status
ggplot(loans_data_clean, aes(x = homeownership, y = interest_rate, fill = homeownership)) +
  geom_boxplot() +
  labs(title = "Interest Rate by Homeownership Status", x = "Homeownership Status", y = "Interest Rate (%)") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()


# Visualization 4: Interest Rate vs. Debt-to-Income Ratio
ggplot(loans_data_clean, aes(x = debt_to_income, y = interest_rate)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Interest Rate vs. Debt-to-Income Ratio", x = "Debt-to-Income Ratio", y = "Interest Rate (%)") +
  theme_minimal()





#Q1:1.	How do various borrower characteristics such as annual income, employment length, debt-to-income ratio, and homeownership status affect the interest rate of a loan? 
# Select relevant columns for borrower characteristics
# Select relevant columns for borrower characteristics
borrower_characteristics <- loans_data[c('emp_length', 'homeownership', 'annual_income', 
                                         'verified_income', 'debt_to_income', 'annual_income_joint', 
                                         'verification_income_joint', 'total_credit_lines', 
                                         'open_credit_lines', 'total_credit_limit', 
                                         'total_credit_utilized', 'num_historical_failed_to_pay', 
                                         'months_since_90d_late','loan_purpose', 'loan_amount')]

# View the table of borrower characteristics
borrower_characteristics


# Select relevant columns for Loan Approval Rates and Terms
loan_approval_terms <- loans_data[c('term', 'interest_rate', 'grade')]


set.seed(123) # For reproducibility
sample_indices <- sample.int(n = nrow(loans_data), size = floor(0.75 * nrow(loans_data)), replace = FALSE)
train_data <- loans_data[sample_indices, ]
test_data <- loans_data[-sample_indices, ]

# Fit linear regression model using only specified borrower characteristics
model <- lm(interest_rate ~ emp_length + homeownership + annual_income + verified_income + 
              debt_to_income + annual_income_joint + verification_income_joint + 
              total_credit_lines + open_credit_lines + total_credit_limit + 
              total_credit_utilized + num_historical_failed_to_pay + 
              months_since_90d_late + loan_purpose + loan_amount, 
            data = train_data)

# View model summary
summary(model)

# Create residuals plot
plot(model, which = 1)



colSums(is.na(loans_data))
loans_data$emp_length[is.na(loans_data$emp_length)] <- 'Unknown'

loans_data$debt_to_income[is.na(loans_data$debt_to_income)] <- median(loans_data$debt_to_income, na.rm = TRUE)
# Binary indicator for joint application
loans_data$joint_application <- ifelse(is.na(loans_data$annual_income_joint), 0, 1)

# For joint application variables, impute missing values for rows where joint_application == 1
loans_data$annual_income_joint[is.na(loans_data$annual_income_joint) & loans_data$joint_application == 1] <- median(loans_data$annual_income_joint, na.rm = TRUE)

# Assign a large value to indicate never been delinquent
loans_data$months_since_last_delinq[is.na(loans_data$months_since_last_delinq)] <- 999
loans_data$months_since_90d_late[is.na(loans_data$months_since_90d_late)] <- 999

loans_data$months_since_last_credit_inquiry[is.na(loans_data$months_since_last_credit_inquiry)] <- median(loans_data$months_since_last_credit_inquiry, na.rm = TRUE)

loans_data$num_accounts_120d_past_due[is.na(loans_data$num_accounts_120d_past_due)] <- 0

colSums(is.na(loans_data))

# Impute 'debt_to_income_joint' for joint applications
is_joint_application <- loans_data$joint_application == 1

# Median imputation for 'debt_to_income_joint' but only for joint applications
median_debt_to_income_joint <- median(loans_data$debt_to_income_joint, na.rm = TRUE)
loans_data$debt_to_income_joint[is.na(loans_data$debt_to_income_joint) & is_joint_application] <- median_debt_to_income_joint


glm_model <- glm(interest_rate ~ emp_length + homeownership + annual_income + 
                   verified_income + debt_to_income + months_since_last_delinq + 
                   total_credit_lines + open_credit_lines + total_credit_limit + 
                   total_credit_utilized + num_historical_failed_to_pay + 
                   months_since_90d_late + loan_purpose + loan_amount + 
                   joint_application,  # Assuming you've created this indicator
                 family = Gamma(link = "log"), data = loans_data)

summary(glm_model)

library(car)
vif(glm_model)

plot(residuals(glm_model) ~ fitted(glm_model))
abline(h = 0, col = "red")


# Extract model coefficients and confidence intervals
model_coef <- broom::tidy(glm_model, conf.int = TRUE)

# Create the coefficient plot
ggplot(model_coef, aes(estimate, reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Coefficient Estimate", y = "Predictor", title = "Effect of Predictors on Interest Rate", subtitle = "GLM Model Coefficients with Confidence Intervals") +
  theme_minimal()

plot(glm_model, which = 1)





