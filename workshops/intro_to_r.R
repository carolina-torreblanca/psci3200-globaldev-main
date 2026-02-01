# =============================================================================
# Introduction to R
# PSCI 3200 - Global Development
# Carolina Torreblanca
# =============================================================================

# =============================================================================
# PART 1: The Two Rules of R
# =============================================================================

# Rule 1: Everything that EXISTS is an OBJECT
# Rule 2: Everything that HAPPENS is a FUNCTION

# Let's see this in action:

# Creating objects (assignment)
x <- 5
y = 10
my_name <- "Carolina"

# Functions do things to objects
sum(x, y)
print(my_name)
class(x)

# Even operators are functions!
x + y
`+`(x, y)  # same thing


# =============================================================================
# PART 2: Types of Objects
# =============================================================================

# --- Vectors (1-dimensional) ---
# Numeric
numbers <- c(1, 2, 3, 4, 5)
class(numbers)

# Character (text/strings)
names <- c("Alice", "Bob", "Carolina")
class(names)

# Logical (TRUE/FALSE)
passed <- c(TRUE, FALSE, TRUE)
class(passed)

# Factor (categorical)
treatment <- factor(c("control", "treatment", "control"))
class(treatment)

# --- Accessing elements ---
numbers[1]        # first element
numbers[2:4]      # elements 2 through 4
names[c(1, 3)]    # first and third elements

# --- Data frames (2-dimensional) ---
# Most common structure for data analysis
# Rows = observations, Columns = variables

my_data <- data.frame(
  name = c("Alice", "Bob", "Carolina"),
  age = c(22, 25, 30),
  score = c(85, 92, 78)
)

my_data

# Accessing columns
my_data$name
my_data$age

# Accessing rows and columns
my_data[1, ]      # first row
my_data[, 2]      # second column
my_data[1, 2]     # first row, second column


# =============================================================================
# PART 3: Packages - Extending R
# =============================================================================

# R comes with "base R" functions
# Packages add more functions for specific tasks

# Install a package (only need to do once)
# install.packages("tidyverse")

# Load a package (need to do every session)
library(tidyverse)

# tidyverse includes:
# - dplyr: data manipulation
# - ggplot2: visualization
# - readr: reading data
# - and more...


# =============================================================================
# PART 4: Loading Data
# =============================================================================

# R comes with built-in datasets - no need to load files!
data()  # see all available datasets

# We'll use mtcars - data on 32 cars from 1974
# mpg: miles per gallon
# cyl: number of cylinders (4, 6, or 8)
# hp: horsepower
# wt: weight (1000 lbs)
# am: transmission (0 = automatic, 1 = manual)

cars <- mtcars  # copy to a new name for clarity

# For your own data, you would use:
# my_data <- read.csv("path/to/your/file.csv")
# my_data <- read.csv("https://some-website.com/data.csv")


# =============================================================================
# PART 5: Exploring Data
# =============================================================================

# First look
head(cars)          # first 6 rows
tail(cars)          # last 6 rows
head(cars, 10)      # first 10 rows

# Structure and dimensions
dim(cars)           # rows x columns
nrow(cars)          # number of rows
ncol(cars)          # number of columns
names(cars)         # column names
str(cars)           # structure (types of each column)

# Summary statistics
summary(cars)

# Individual columns
mean(cars$mpg)
sd(cars$mpg)
min(cars$hp)
max(cars$hp)
table(cars$cyl)
table(cars$am)


# =============================================================================
# PART 6: Data Manipulation with dplyr
# =============================================================================

# dplyr uses "verbs" for common operations
# and the pipe %>% to chain them together

# --- select(): choose columns ---
cars %>%
  select(mpg, cyl, hp)

# --- filter(): choose rows ---
cars %>%
  filter(cyl == 4)

cars %>%
  filter(mpg > 25)

cars %>%
  filter(cyl == 4 & mpg > 25)

# --- mutate(): create new columns ---
cars %>%
  mutate(hp_per_cyl = hp / cyl)

cars %>%
  mutate(
    hp_per_cyl = hp / cyl,
    is_automatic = am == 0
  )

# --- summarize(): collapse to summary statistics ---
cars %>%
  summarize(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    n = n()
  )

# --- group_by(): do operations by group ---
cars %>%
  group_by(cyl) %>%
  summarize(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    n = n()
  )

# --- Chaining multiple operations ---
cars %>%
  filter(am == 1) %>%
  group_by(cyl) %>%
  summarize(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    n = n()
  ) %>%
  arrange(desc(avg_mpg))

# --- Save results to a new object ---
cars_summary <- cars %>%
  group_by(cyl) %>%
  summarize(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    n = n()
  )

cars_summary


# =============================================================================
# PART 7: Visualization with ggplot2
# =============================================================================

# ggplot2 uses a "grammar of graphics"
# Build plots layer by layer

# --- Histogram ---
ggplot(cars, aes(x = mpg)) +
  geom_histogram()

# Add some style
ggplot(cars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Fuel Efficiency",
       x = "Miles per Gallon",
       y = "Count")

# --- Histogram by group ---
# First, make cyl a factor for better plotting
cars <- cars %>%
  mutate(cyl = factor(cyl))

ggplot(cars, aes(x = mpg, fill = cyl)) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "identity") +
  labs(title = "Fuel Efficiency by Number of Cylinders")

# --- Box plot ---
ggplot(cars, aes(x = cyl, y = mpg)) +
  geom_boxplot()

ggplot(cars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  labs(title = "Fuel Efficiency by Cylinders",
       x = "Number of Cylinders",
       y = "Miles per Gallon") +
  theme_minimal()

# --- Scatter plot ---
ggplot(cars, aes(x = hp, y = mpg)) +
  geom_point()

ggplot(cars, aes(x = hp, y = mpg, color = cyl)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Horsepower vs Fuel Efficiency",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal()

# --- Bar plot (for summaries) ---
ggplot(cars_summary, aes(x = factor(cyl), y = avg_mpg, fill = factor(cyl))) +
  geom_col() +
  labs(title = "Average MPG by Number of Cylinders",
       x = "Cylinders",
       y = "Average MPG") +
  theme_minimal()


# =============================================================================
# PART 8: Quick Analysis - Does Engine Size Affect Efficiency?
# =============================================================================

# Compare mpg across cylinder groups
cars %>%
  group_by(cyl) %>%
  summarize(
    avg_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    n = n()
  )

# Simple linear regression
model <- lm(mpg ~ hp, data = cars)
summary(model)

# Visualize the relationship
ggplot(cars, aes(x = hp, y = mpg)) +
  geom_point(aes(color = cyl), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Does Horsepower Affect Fuel Efficiency?",
       subtitle = "1974 Motor Trend Car Data",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal()


# =============================================================================
# YOUR TURN
# =============================================================================

# 1. What's the average horsepower for each cylinder group?

# 2. Filter to only manual transmission cars (am == 1).
#    What's the average mpg by cylinder group?

# 3. Create a histogram of horsepower, colored by cylinder group

# 4. Create a scatter plot of weight (wt) vs mpg

