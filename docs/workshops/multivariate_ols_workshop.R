# -------------------------------- #
# PSCI 3200
# Understanding multiple regression
# Carolina Torreblanca
# -------------------------------- #

require(tidyverse)

# We are going to use a pre-loaded dataset
?swiss

# Load dataset
data(swiss)

# 47 observations, not very many!
str(swiss)

# Lets say we are interested in predicting Infant Mortality
summary(swiss$Infant.Mortality) # this is in %

# We are first using Education as predictor
# we can fil a simple bivariate OLS

bi_model <- lm(Infant.Mortality ~ Education, data = swiss)

# How do we interpret the coefficients?
# Looking at the R^2 is this a good or a bad prediction?
summary(bi_model)

# Not really much of an effect
plot(swiss$Infant.Mortality, swiss$Education,
     main = "Bivariate",
     xlab = "Infant Mortality",
     ylab = "% Educated Beyond Primary",
     pch = 19, col = "blue")
abline(bi_model, col = "red", lwd = 2)

# We can add predictiors easily into our model

multi_mode <-  lm(Infant.Mortality ~ Education + Catholic, data = swiss)

# Notice the coefficient of Education changes when we add catholic!
# This is likely because "catholic" was related to education and mortality
# By adding it to the regression we "control" for it
# The interpretation also changes!
# Now its the marginal change in y (pp) when x increases by 1 unit (pp) HOLDING
# Fixed all other covariates

summary(multi_mode)

# Now our model is not a line so much as a plane
# THat is what we mean by "holding constant": looking only

# Install and load the necessary package
install.packages("scatterplot3d")
library(scatterplot3d)

# Create 3D scatter plot
scatter <- scatterplot3d(swiss$Education, swiss$Catholic, swiss$Infant.Mortality,
                         pch = 19, color = "blue",
                         xlab = "Education",
                         ylab = "Catholic (%)",
                         zlab = "Infant Mortality",
                         main = "3D Scatter Plot of Infant Mortality vs Education & Catholic")

# Predict plane values
grid_vals <- expand.grid(Education = seq(min(swiss$Education),
                                         max(swiss$Education), length.out = 10),
                         Catholic = seq(min(swiss$Catholic),
                                        max(swiss$Catholic), length.out = 10))
grid_vals$Predicted <- predict(multi_mode, newdata = grid_vals)

# Add regression plane
scatter$plane3d(multi_mode, col = "red", lty = "solid")

# You can add many many covariates although it's difficult to visualize

mega_multi_mode <-  lm(Infant.Mortality ~ Education + Catholic +
                         Agriculture + Examination + Fertility , data = swiss)

# Ok how do we interpret the coefficient on fertility
summary(mega_multi_mode)

# While we cannot visualize our model, we can use it still to predict our
# y hats

y_hats <- predict(mega_multi_mode)

#
ggplot() +
  geom_point(aes(y = y_hats, x = swiss$Infant.Mortality)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(title = "Predicted vs Observed Infant Mortality",
       y = "Predicted by the model", x = "Observed") +
  coord_cartesian(ylim = c(15, 25),
                  xlim = c(15, 25))





