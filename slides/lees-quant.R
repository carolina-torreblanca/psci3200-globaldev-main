# Illustrating Mean Values in R -------------------------------------------

# Set seed for reproducibility of random numbers
# set.seed() function ensures that the sequence of random numbers can be reproduced.
# This is useful for consistency in examples and debugging.
set.seed(123)

# Generate a vector of 10 random numbers from a normal distribution
# rnorm() function generates random numbers from a normal distribution
# mean = 10 specifies the mean of the distribution
# sd = 5 specifies the standard deviation of the distribution
my_vector <- rnorm(10, mean = 10, sd = 5)

# print() function prints objects to your Console
print(my_vector)

# Step 1: Sum the values in the vector
# sum() function calculates the sum of all elements in the vector
sum_values <- sum(my_vector)
print(sum_values)

# Step 2: Count the number of elements in the vector
# length() function returns the number of elements in the vector
count_elements <- length(my_vector)
print(count_elements)

# Step 3: Calculate the mean by dividing the sum by the number of elements
# Mean is calculated by summing all the values and dividing by the count of elements
mean_value <- sum_values / count_elements

# cat() function concatenates and prints objects. It outputs a string to the console.
# In this case, it prints the text "Calculated mean:" followed by the calculated value.
cat("Calculated mean:",
             mean_value)

# You can also use the built-in R function to get the same result
# mean() function calculates the mean of the elements in the vector
cat("Mean using built-in function:",
             mean(my_vector))





# Illustrating Variance Calculation in R ----------------------------------

# Set seed for reproducibility of random numbers
set.seed(123)

# Generate another vector of 10 random numbers from a normal distribution
# mean = 10 specifies the mean of the distribution
# sd = 5 specifies the standard deviation of the distribution
dat <- rnorm(10, mean = 10, sd = 5)
cat("Generated vector:", dat)

# Sort the vector in ascending order
# sort() function sorts the elements of the vector in ascending order
dat <- sort(dat)
cat("Sorted vector:", dat)

# Calculate and store the variance of the sorted vector using the built-in function
# var() function calculates the variance of the elements in the vector
o_var <- var(dat)
cat("Original variance (using built-in function):", o_var)

# Calculate variance by hand ----------------------------------------------

# Step 1: Calculate the mean of the vector
# sum() function calculates the sum of all elements in the vector
sum_values <- sum(dat)

# length() function returns the number of elements in the vector
count_elements <- length(dat)

# Mean is calculated by summing all the values and dividing by the count of elements
mean_value <- sum_values / count_elements

# Step 2: Calculate the squared differences from the mean
# Initialize a vector to store squared differences
squared_differences <- (dat - mean_value)^2
cat("Squared differences from the mean:", squared_differences)

# Step 3: Calculate the sum of squared differences
sum_squared_differences <- sum(squared_differences)
cat("Sum of squared differences:", sum_squared_differences)

# Step 4: Divide the sum of squared differences by the number of elements minus 1 (N-1)
# This is the variance calculation for a sample
manual_variance <- sum_squared_differences / (count_elements - 1)
cat("Calculated variance by hand:", manual_variance)

# Verify the result using the built-in var() function
cat("Variance using built-in function:", var(dat))



# Variance puts more weight on values further from the mean ---------------

# Create a new vector for making a big addition to the largest number
# Copy the sorted vector to b_dat (now we have two copies: dat and b_dat)
b_dat <- dat

# Store the length of the vector
# length() function returns the number of elements in the vector
ind <- length(b_dat)
cat("Length of the vector:", ind)

# Add 4 to the largest number in the vector
# Access the largest element using the index ind and increase it by 4
b_dat[ind] <- b_dat[ind] + 4
cat("Vector after adding 4 to the largest number:", b_dat)

# Calculate the new variance after adding 4 to the largest number
b_var <- var(b_dat)
cat("New variance after big addition:", b_var)

# Calculate and print the increase in variance
val <- b_var - o_var
cat("Variance increases by", val)

# Create a new vector for making a small addition
# Copy the sorted vector to s_dat
s_dat <- dat

# Add 4 to the second smallest number in the vector
# Access the second smallest element using the index ind-2 and increase it by 4
s_dat[ind-2] <- s_dat[ind-2] + 4
cat("Vector after adding 4 to the second smallest number:", s_dat)

# Calculate the new variance after adding 4 to the smallest number
s_var <- var(s_dat)
cat("New variance after small addition:", s_var)

# Calculate and print the increase in variance after the small addition
val <- s_var - o_var
cat("Variance increases by", val)

# Even though we are adding the same number (4), the increase in variance is
# much larger when we add 4 to a larger number




# Illustrating Correlations in R ------------------------------------------

# Set seed for reproducibility of random numbers
set.seed(1234)

# Generate two vectors of 10 random numbers each from a normal distribution
# mean = 10 specifies the mean of the distribution
# sd = 5 specifies the standard deviation of the distribution
x <- rnorm(10, mean = 10, sd = 5)
y <- rnorm(10, mean = 10, sd = 5)

# Print the generated vectors
cat("Vector X:", x, "\n")
cat("Vector Y:", y)

# Calculate the means of both vectors
mean_x <- mean(x)
mean_y <- mean(y)

# Calculate the covariance between the two vectors
# Cov(X, Y) = sum((X_i - mean_X) * (Y_i - mean_Y)) / (n - 1)
covariance <- sum((x - mean_x) * (y - mean_y)) / (length(x) - 1)
cat("Covariance between X and Y:", covariance)

# Verify the result using the built-in cov() function
cat("Covariance using built-in function:", cov(x,y))

# Calculate the standard deviations of both vectors
sd_x <- sd(x)
sd_y <- sd(y)

# Calculate the correlation coefficient between the two vectors
# Cor(X, Y) = Cov(X, Y) / (sd_X * sd_Y)
correlation <- covariance / (sd_x * sd_y)
cat("Correlation coefficient between X and Y:", correlation)

# Verify the result using the built-in cor() function
cat("Correlation coefficient using built-in function:", cor(x,y))

# Calculate the variance of X
# Variance = sum((X_i - mean_X)^2) / (n - 1)
variance_x <- sum((x - mean_x)^2) / (length(x) - 1)

# Calculate the slope (beta) of Y on X
# Beta_X = Cov(X, Y) / Var(X)
beta_x <- covariance / variance_x
cat("Slope of Y on X:", beta_x)

# Verify the result using the built-in lm() function
cat("Slope using built-in function:",
    lm(y ~ x)$coefficient[2])



# Illustrating Multiple Regression in R -----------------------------------

# Load required libraries
# dplyr is used for data manipulation
# modelsummary is used to summarize and display regression models
library(dplyr)
library(modelsummary)

# Generate example data ----------------------------------------------------

# Set seed for reproducibility of random numbers
# This ensures that the random numbers generated can be reproduced in future runs
set.seed(123)

# Create a data frame with 100 observations
data <- data.frame(
  X1 = rnorm(100, mean = 10, sd = 2),    # Generate 100 random numbers for the first independent variable
  X2 = rnorm(100, mean = 5, sd = 2),     # Generate 100 random numbers for the second independent variable
  Y = rnorm(100, mean = 10, sd = 2)      # Generate 100 random numbers for the dependent variable
)

# Introduce correlation between X1 and Y
# Modify Y to have a linear relationship with X1 plus some random noise
# This is done to simulate a realistic scenario where Y is influenced by X1
data$Y <- 0.5 * data$X1 + rnorm(100, mean = 10, sd = 1)

# Run simple linear regression ----------------------------------------------

# Run simple linear regression model (Y ~ X1)
# lm() function fits linear models
# Here, we are modeling Y as a function of X1
simple_model <- lm(Y ~ X1, data = data)

# Run multiple regression ---------------------------------------------------

# Run multiple regression model (Y ~ X1 + X2)
# In this model, we are modeling Y as a function of both X1 and X2
multiple_model <- lm(Y ~ X1 + X2, data = data)

# Summarize the output of both models ---------------------------------------

# modelsummary() function provides a summary of the regression models
# list() function is used to include both models in the summary
# "Simple Linear Regression" and "Multiple Regression" are titles for the models in the summary
# estimate parameter formats the output of the estimates
# {estimate} displays the coefficient estimate
# {stars} adds significance stars based on p-values
# ({std.error}) displays the standard error in parentheses
# statistic = NULL omits the test statistics
# gof_omit parameter omits some goodness-of-fit statistics from the summary
# 'IC|RMSE|Log|F|R2$|Std.' is a regular expression pattern to match and omit specific statistics
modelsummary(
  list(
    "Simple Linear Regression" = simple_model,
    "Multiple Regression" = multiple_model
  ),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
  output = "dataframe"
)



# Difference-in-Differences -----------------------------------------------

# Set seed for reproducibility of random numbers
# This ensures that the random numbers generated can be reproduced in future runs
set.seed(1235)

# Create a data frame with 200 observations
data <- data.frame(
  treatment = rep(c(1, 0), each = 100),   # Create a binary treatment variable: 1 for treatment, 0 for control
  # Create a binary post variable: 1 for post-treatment period, 0 for pre-treatment period
  post = rep(c(1, 0), each = 50, times = 2),

  outcome = c(
    # Generate 50 random numbers for control group in the pre-treatment period
    rnorm(50, mean = 10, sd = 2),
              # Generate 50 random numbers for control group in the post-treatment period
              rnorm(50, mean = 10, sd = 2),
              # Generate 50 random numbers for treatment group in the pre-treatment period
              rnorm(50, mean = 10, sd = 2),
              # Generate 50 random numbers for treatment group in the post-treatment period
              rnorm(50, mean = 12, sd = 2))
)

# Print random rows of the generated data
# sample_n() function randomly samples n rows from the data frame
set.seed(123)  # Setting seed for reproducibility of the random sample
sampled_data <- sample_n(data, 6)
print(sampled_data)

# Run difference-in-differences model --------------------------------------

# Run difference-in-differences (DID) model (outcome ~ treatment * post)
# lm() function fits linear models
# Here, we are modeling the outcome as a function of treatment, post, and their interaction
# The interaction term (treatment * post) captures the difference-in-differences effect
did_model <- lm(outcome ~ treatment * post, data = data)

# Summarize the output -----------------------------------------------------

# modelsummary() function provides a summary of the regression models
modelsummary(did_model,
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
  output = "dataframe"
)


# Illustrate Heterogeneous Effects in R -----------------------------------

# Set seed for reproducibility of random numbers
set.seed(123)

# Create a data frame with 200 observations
data <- data.frame(
  treatment = rep(c(1, 0), each = 100),      # Create a binary treatment variable: 1 for treatment, 0 for control
  group = rep(c(1, 0), each = 50, times = 2), # Create a binary group variable: 1 for Group 1, 0 for Group 2
  outcome = c(rnorm(50, mean = 10, sd = 2),  # Group 1: treatment
              rnorm(50, mean = 10, sd = 2),  # Group 2: treatment
              rnorm(50, mean = 10, sd = 2),  # Group 1: control
              rnorm(50, mean = 15, sd = 2))  # Group 2: control
)


# Run interaction model to illustrate heterogeneous effects ----------------

# Run linear regression model with interaction term (outcome ~ treatment * group)
# lm() function fits linear models
# Here, we are modeling the outcome as a function of treatment, group, and their interaction
# The interaction term (treatment * group) captures the heterogeneous effects
interaction_model <- lm(outcome ~ treatment * group, data = data)

# Summarize the output -----------------------------------------------------

# modelsummary() function provides a summary of the regression models
modelsummary(
  list(
    lm(outcome ~ treatment + group, data = data),    # Model without interaction term
    lm(outcome ~ treatment * group, data = data)     # Model with interaction term
  ),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
  output = "dataframe"
)


# Illustrative Additive Index in R ----------------------------------------

# Set seed for reproducibility of random numbers
set.seed(123)

# Create a data frame with 200 observations
data <- data.frame(
  variable1 = rnorm(200, mean = 50, sd = 10),  # Generate 200 random numbers for the first variable
  variable2 = rnorm(200, mean = 30, sd = 5)   # Generate 200 random numbers for the second variable
)

# Create an additive index variable
# This index is simply the sum of variable1 and variable2
data <- data %>%
  mutate(additive_index = variable1 + variable2)

# Print the first few rows of the generated data
# head() function shows the first six rows of the data frame by default
# The sum of variable1 and variable2 is equivalent to the additive_index value
head(data)


# Illustrate Averaged Z-Score ---------------------------------------------

# Set seed for reproducibility of random numbers
set.seed(123)

# Create a data frame with 200 observations
data <- data.frame(
  variable1 = rnorm(200, mean = 50, sd = 10),  # Generate 200 random numbers for the first variable
  variable2 = rnorm(200, mean = 30, sd = 5),   # Generate 200 random numbers for the second variable
  variable3 = rnorm(200, mean = 20, sd = 3)    # Generate 200 random numbers for the third variable
)

# Standardize variables to create z-scores ---------------------------------

# Standardize each variable to have a mean of 0 and a standard deviation of 1
# mutate() is a function from the dplyr package that allows you to create or transform variables within a data frame.
# Each new variable is defined within the mutate() function using the syntax: new_variable = transformation(existing_variable).
# Here, we create three new variables (z_variable1, z_variable2, z_variable3)
# which are the standardized (z-score) versions of variable1, variable2, and variable3.
data <- data %>%
  mutate(
    z_variable1 = (variable1 - mean(variable1)) / sd(variable1),  # Z-score for variable1
    z_variable2 = (variable2 - mean(variable2)) / sd(variable2),  # Z-score for variable2
    z_variable3 = (variable3 - mean(variable3)) / sd(variable3)   # Z-score for variable3
  )

# Create an averaged z-score index -----------------------------------------

# Create an index by averaging the z-scores of the variables
# rowMeans() is a base R function that calculates the mean of each row for the specified columns.
# select() is a dplyr function used to select specific columns from a data frame.
# starts_with() is a function within select() that allows you to select columns with a specified prefix.
# In this case, select(., starts_with("z_variable")) selects all columns whose names start with "z_variable"
# rowMeans(select(., starts_with("z_variable"))) calculates the average of these selected columns for each row
# The resulting averaged z-score is stored in a new column called average_z_score.
data <- data %>%
  mutate(average_z_score = rowMeans(select(., starts_with("z_variable"))))

# Print the first few rows of the generated data
# head() function shows the first six rows of the data frame by default
# This is useful to get a quick look at the structure and contents of the data
# The printed output will display the columns: variable1, variable2, variable3,
# z_variable1, z_variable2, z_variable3, and average_z_score
head(data)
