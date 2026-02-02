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
View(my_data)

# Accessing columns -- give them the money
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

# =============================================================================
# PART 5: Exploring Data
# =============================================================================

# First look
head(cars)          # first 6 rows
tail(cars)          # last 6 rows
head(cars, 10)      # first 10 rows
View(cars)

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

# mpg - miles per gallon
# cyl - cylinders
# disp - displacement (idk)
# hp - gross horsepower
# drat rear axle ration (idk)
# wt - weight
# qsc 1/4 mile time
# am - transmission (0 = auomoatic, 1, manual)
# gear #
# carb number of carburators

# =============================================================================
# PART 5b: Data Manipulation with Base R
# =============================================================================

# Before dplyr, people used base R for everything
# Still useful to know!

# --- Subsetting rows ---
cars[cars$cyl == 4, ]              # filter: only 4-cylinder cars
cars[cars$mpg > 25, ]              # filter: mpg greater than 25

# --- Subsetting columns ---
cars[, c("mpg", "hp")]             # select: just mpg and hp
cars[, 1:3]                        # select: first 3 columns

# --- Both at once ---
cars[cars$cyl == 4, c("mpg", "hp")]  # 4-cylinder cars, only mpg and hp

# --- Creating new columns ---
cars$hp_per_cyl <- cars$hp / cars$cyl

# --- How would you create a new variable with the names of each car?

# ?????

# --- Sorting ---
cars[order(cars$mpg), ]            # sort by mpg (ascending)
cars[order(-cars$mpg), ]           # sort by mpg (descending)

# --- Aggregating (base R way) ---
aggregate(mpg ~ cyl, data = cars, FUN = mean)  # mean mpg by cylinder

# Base R works, but dplyr is more readable...


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

# ifelse
cars %>%
  mutate(
    hp_per_cyl = hp / cyl,
    is_stickshift = ifelse(am == 1, 1, 0)
  )


# --- arrange

cars %>% arrange(-qsec)

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

# --- are any of these changes saved in the data?
# ???


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
# PART 6b: Basic Visualization with Base R
# =============================================================================

# Before ggplot, we used base R plotting
# Quick and dirty, good for exploring

# --- Simple scatter plot ---
plot(cars$hp, cars$mpg)

# --- Add labels ---
plot(cars$hp, cars$mpg,
     main = "Horsepower vs MPG",
     xlab = "Horsepower",
     ylab = "Miles per Gallon")

# we can add a linear regression line (more on that later)

my_regression <- lm(cars$mpg~cars$hp)

plot(cars$hp, cars$mpg,
     main = "Horsepower vs MPG",
     xlab = "Horsepower",
     ylab = "Miles per Gallon")
abline(my_regression, add = T, col = "red")


# --- Histogram ---
hist(cars$mpg)
hist(cars$mpg, breaks = 10, col = "red", main = "Distribution of MPG")

# --- Correlation ---
cor(cars$hp, cars$mpg)  # correlation coefficient: -0.78 (negative relationship)

# Correlation matrix for multiple variables
cor(cars[, c("mpg", "hp", "wt", "disp")])

# --- Quick pairs plot (all correlations at once) ---
pairs(cars[, c("mpg", "hp", "wt")])

# Base R plots are fast for exploration
# ggplot is better for publication-quality figures


# =============================================================================
# PART 7: Visualization with ggplot2
# =============================================================================

# ggplot2 uses a "grammar of graphics"
# Build plots layer by layer
# more detail

# --- Histogram ---
ggplot(cars, aes(x = mpg)) +
  geom_histogram()

# Add some style
ggplot(cars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "white") +
  labs(title = "Distribution of Fuel Efficiency",
       x = "Miles per Gallon",
       y = "Count")


# --- Scatter plot ---
ggplot(cars, aes(x = hp, y = mpg)) +
  geom_point()

ggplot(cars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Horsepower vs Fuel Efficiency",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal()

# add a linear regression

ggplot(cars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, alpha = 0.7) + # with a new stat
  geom_smooth(method = lm, formula = "y ~x", color = "red") +
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
# PART 8: Importing and Exporting Data
# =============================================================================

# --- CSV files (most common) ---

# Export to CSV
write.csv(cars_summary, "cars_summary.csv", row.names = FALSE)

# Import from CSV
my_data <- read.csv("cars_summary.csv")

# --- RDS files (R's native format) ---
# Preserves R object exactly (factors, types, etc.)
# Smaller file size, faster to read/write

# Export to RDS
saveRDS(cars_summary, "cars_summary.rds")

# Import from RDS
my_data <- readRDS("cars_summary.rds")

# --- Excel files (need readxl package) ---
# install.packages("readxl")
# library(readxl)
# my_data <- read_excel("data.xlsx", sheet = 1)

# --- From the web ---
# my_data <- read.csv("https://example.com/data.csv")

# --- Tip: check your working directory ---
getwd()                    # where R is looking for files
# setwd("/path/to/folder")  # change working directory

# --- Clean up: remove files we created ---
# file.remove("cars_summary.csv")
# file.remove("cars_summary.rds")


# =============================================================================
# YOUR TURN
# =============================================================================

# 1. What's the average horsepower for each cylinder group?


# 2. Create a scatter plot of weight (wt) vs mpg



