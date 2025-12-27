# Grading formula for PSCI 3200
library(tidyverse)

## Read-in data
dat = readxl::read_xlsx("C:/Users/jerem/Dropbox/psci_spring25/submissions.xlsx")

## Create assignment groups by column

# quizzes and workshops (.025 each)
qw = c("01_quarto", "02_gitrepo", "03_fprqd",  "quiz")

# data assignments (.12 each)
da = c("aau_survey", "04_fpdesign", "05_fptest")

# final project (.44)
fp = c("final_project")


dat <- dat %>%
  mutate(across(qw, ~ .x * 5),    # Multiply columns in 'qw' by 0.025
         across(da, ~ .x * 0.12),     # Multiply columns in 'da' by 0.12
         final_project = final_project * 0.44,  # Multiply 'final_project' by 0.44
         grade = round(rowSums(across(c(qw, da, fp))), digits = 0))  # Calculate row sums across selected columns

mean(dat$grade, na.rm = T)
median(dat$grade, na.rm = T)

ggplot(dat, aes(x=grade, color=Classification)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=1)+
  geom_density(alpha=.2, fill="grey") +
  geom_vline(aes(xintercept=mean(grade, na.rm = T)),
             color="blue", linetype="dashed", size=1)+
  theme_bw()

write.csv(dat %>%
            select(Name, grade), "/home/jeremy//Dropbox/psci_spring25/final_grades.csv")

# Categorize the grades into specified ranges
dat <- dat %>%
  mutate(grade_range = case_when(
    grade >= 50 & grade < 60 ~ "50-60",
    grade >= 60 & grade < 70 ~ "60-70",
    grade >= 70 & grade < 80 ~ "70-80",
    grade >= 80 & grade < 90 ~ "80-90",
    grade >= 90 & grade <= 100 ~ "90-100",
    TRUE ~ "Other"
  ))

# Count the number of rows in each grade range
dat %>%
  count(grade_range)

