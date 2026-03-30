# ============================================================
# Data Wrangling Workshop: Olympics & Development
# PSCI 3200 — Spring 2026
# ============================================================
#
# BEFORE RUNNING THIS SCRIPT:
# 1. You should have created a project folder with input/, output/, code/
# 2. You should have created an RProject in that folder
# 3. You should have saved olympics_raw.csv and wb_indicators_panel.csv in input/
# 4. Save THIS script in your code/ folder
# 5. Open the .Rproj file to launch RStudio — this sets your working directory
#
# ============================================================

# --- Setup ---------------------------------------------------

library(tidyverse)
library(modelsummary)


# =============================================================
# PART 1: LOAD AND EXPLORE THE OLYMPICS DATA
# =============================================================

# Read the data using a RELATIVE path (works because of the RProject)
olympics_raw <- read_csv("input/olympics_raw.csv")

# Explore: how big is the data? what variables do we have?
dim(olympics_raw)
glimpse(olympics_raw)
head(olympics_raw)

# Q: what is the unit of observation?

summary(olympics_raw$year) # since 1896!
table(olympics_raw$noc) # the country in a three letter olympic code situation

# --- Clean: medal counts by country-Olympics ---

medals_by_country <- olympics_raw %>%
  filter(season == "Summer", !is.na(medal)) %>%
  group_by(noc, year) %>%     # group by country and year
  summarize(total_medals = n(),
            gold = sum(medal == "Gold"),
            silver = sum(medal == "Silver"),
            bronze = sum(medal == "Bronze")) %>%
  arrange(noc, year)

# Q: what is the unit of observation now?
head(medals_by_country, 10)

# -- lets see how medals have changed per olympic

per_ol <- medals_by_country %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(total_medals = sum(total_medals),
            gold = sum(gold),
            silver = sum(silver),
            bronze = sum(bronze))

# but i want to see all four lines at the same time -
# i cant do it easily with this data format

ggplot(per_ol) +
  geom_line(aes(x = year, y = total_medals), color = "black") +
  geom_line(aes(x = year, y = gold), color = "gold") +
  geom_line(aes(x = year, y = silver), color = "blue") +
  geom_line(aes(x = year, y = bronze), color = "orange") +
  theme_classic(base_size = 14) +
  labs(x = "", y = "Total medals")

# i can also pivot so its easier
# two types of reshape: longer or wider. wider -> make more columns
# longer > make fewer columns
# we want a column that is "medal type" and another that is "count" instead of four columns

per_ol_rs <- per_ol %>%
  pivot_longer(cols = c("total_medals", "silver", "gold", "bronze"),
               names_to = "medal_type", values_to = "count")

# see how beautiful
View(per_ol_rs)

# and now we can harness the power of ggplot
ggplot(per_ol_rs) +
  geom_line(aes(x = year, y = count, color = medal_type, group = medal_type)) +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("total_medals" = "black", "gold" = "gold",
                                "silver" = "blue", "bronze" = "orange")) +
  labs(x = "", y = "Total medals") +
  scale_x_continuous(breaks = seq(1890, 2016, 5))

# so 2000-ish medal count is stable
# lets see best performing countries

medals_by_country_modern <- medals_by_country %>%
  filter(year >= 2000) %>%
  group_by(noc) %>%
  summarize(mean_tot_meds = mean(total_medals, na.rm = T),
            tot_olympics = n(),
            mean_gold = mean(gold),
            mean_silver = mean(silver),
            mean_bronze = mean(bronze)) %>%
  arrange(-mean_tot_meds)

# --- Visualize top 20 ---

medals_by_country_modern %>%
  slice_max(mean_tot_meds, n = 20) %>%
  ggplot(aes(x = reorder(noc, mean_tot_meds), y = mean_tot_meds)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = NULL, y = "Average Medals per Olympics",
       title = "Top 20 Countries by Average Medal Count",
       subtitle = "Summer Olympics, 2000-2016") +
  theme_minimal(base_size = 14)

# another pivot so we can plot the bars by type of medal
medals_by_country_modern_rs <- medals_by_country_modern %>%
  pivot_longer(cols = c("mean_tot_meds", "mean_silver", "mean_gold", "mean_bronze"),
               names_to = "medal_type", values_to = "mean")

# what if we want by type of medal?
# careful: slice_max on the reshaped data would mix medal types together
# instead, find the top 20 countries by TOTAL, then filter the reshaped data to those

top20 <- medals_by_country_modern %>%
  slice_max(mean_tot_meds, n = 20) %>% # instead of sorting do it in one
  pull(noc) # make a vector! 

# also drop mean_tot_meds so we don't stack total on top of components
medals_by_country_modern_rs %>%
  filter(noc %in% top20, medal_type != "mean_tot_meds") %>%
  mutate(medal_type = str_remove(medal_type, "mean_"),
         medal_type = factor(medal_type, levels = c("bronze", "silver", "gold"))) %>%
  ggplot(aes(x = reorder(noc, mean), y = mean, fill = medal_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("gold" = "gold3", "silver" = "grey70", "bronze" = "chocolate3"),
                    name = "Medal") +
  labs(x = NULL, y = "Average Medals per Olympics",
       title = "Top 20 Countries by Medal Type",
       subtitle = "Summer Olympics, 2000-2016") +
  theme_minimal(base_size = 14)


# ==============================================================
# EXERCISE 1:
# What does the age distribution of medal winners look like?
# Make a histogram of age for medal winners post 2000
# Then use facet_wrap(~sport) to see if it varies by sport.
# Hint: filter to !is.na(medal) and year >= 2016, then
# use geom_histogram(aes(x = age))
# ==============================================================

# Your code here:



# =============================================================
# PART 2: LOAD AND CLEAN THE WORLD BANK DATA
# =============================================================

# We want to see whether developmental outcomes make countries
# more successful in the Olympics
# This data is a PANEL: multiple years per country (2000, 2004, 2008, 2012, 2016)
# It includes GDP per capita, population, and female labor force participation

wb_raw <- read_csv("input/wb_indicators_panel.csv")

glimpse(wb_raw)

# Q: what is the unit of observation?
# Q: how many years? how many countries?

n_distinct(wb_raw$year)
n_distinct(wb_raw$country)

# Problem: World Bank includes aggregate regions (e.g., "World", "Sub-Saharan Africa")
# We need to remove those
table(wb_raw$country) # 

wb_clean <- wb_raw %>%
  filter(!is.na(iso2c)) %>%
  filter(str_detect(country, "World|Africa|small states|dividend|Euro|Asia|Latin Americ|Fragile|High|IBRD|IDA|IDA|Least developed|income|Heavily|blend|total|developed|North America|Not class|OECD|Small")==F) %>%
  select(country, iso2c, iso3c, year, gdp_pc, population, fem_lfp) %>%
  filter(!is.na(gdp_pc)) # lets remove missing GDP

# Q: what is the unit of observation?
nrow(wb_clean)


# ==============================================================
# EXERCISE 2:
# What are the 10 richest countries by GDP per capita in 2016?
# The 10 poorest? Use filter() and slice_max() / slice_min().
# ==============================================================

# Your code here:



# =============================================================
# PART 3: MERGING THE DATASETS
# =============================================================

# Our Olympics data is country-year (noc + year)
# Our WB data is country-year (iso3c + year)
# So we can merge on BOTH country AND year — this is a panel merge!

# But first: the Olympics uses NOC codes, World Bank uses ISO codes
table(medals_by_country$noc)
table(wb_clean$iso3c)

# --- 3.1 Prepare the Olympics data for merging ---

# We need medals at the country-year level (we already have this!)
# But lets also add gender breakdown — we'll need it later

table(olympics_raw$sport[olympics_raw$season=="Summer"&olympics_raw$year>=2000])

# team sports: Badminton, Baseball,Basketball,Beach Volleyball,Football,Handball,Hockey,Rugby Sevens,Softball,Synchronized Swimming,Volleyball,Weightlifting

team_sports <- c("Badminton", "Baseball", "Basketball", "Beach Volleyball", 
                 "Football", "Handball", "Hockey", "Rugby Sevens", "Softball", 
                 "Synchronized Swimming", "Volleyball", "Water Polo")

medals_for_merge <- olympics_raw %>%
  filter(season == "Summer", !is.na(medal), year >= 2000) %>%
  mutate(team_sport = ifelse(sport %in% team_sports, 1, 0)) %>% 
  group_by(noc, year) %>%
  summarize(total_medals = n(), 
            tot_meds_teams = sum(team_sport),
            gold_medals = sum(medal == "Gold"),
            female_medals = sum(sex == "F"), 
            male_medals = sum(sex == "M"),
    .groups = "drop") %>%
  mutate(female_share = female_medals / total_medals,
         team_share = tot_meds_teams / total_medals)

# Q: what does female_share capture?
# Q: what does team share?
head(medals_for_merge)


# --- 3.2 Diagnose the matching problem ---

# Which medal-winning countries have NO match in the World Bank data?
unmatched <- medals_for_merge %>%
  distinct(noc) %>%
  anti_join(wb_clean %>% distinct(iso3c), by = c("noc" = "iso3c"))

# --- 3.3 Build a crosswalk ---

# A crosswalk is a small lookup table that maps one set of codes to another
noc_to_iso <- tribble( ~noc,  ~iso3c,
                       "GER", "DEU",
                       "DEN", "DNK",
                       "SUI", "CHE",
                       "NED", "NLD",
                       "GRE", "GRC",
                       "POR", "PRT",
                       "CRO", "HRV",
                       "RSA", "ZAF",
                       "BUL", "BGR",
                       "MAS", "MYS",
                       "CHI", "CHL",
                       "IRI", "IRN",
                       "LAT", "LVA",
                       "SIN", "SGP",
                       "PHI", "PHL",
                       "MGL", "MNG",
                       "TPE", "TWN",
                       "KOS", "XKX",
                       "INA", "IDN",
                       "VIE", "VNM",
                       "BRN", "BHR",
                       "PUR", "PRI",
                       "FIJ", "FJI",
                       "BAH", "BHS",
                       "UAE", "ARE",
                       "TGA", "TON",
                       "KSA", "SAU",
                       "NGR", "NGA")

# Apply the crosswalk
medals_for_merge <- medals_for_merge %>%
  left_join(noc_to_iso, by = "noc") %>%
  mutate(iso3c = ifelse(is.na(iso3c), noc, iso3c)) # if missing its because its the same


# --- 3.4 Merge on country AND year! ---

# This is a panel merge: each country-year in Olympics gets matched
# to the same country-year in World Bank

merged <- medals_for_merge %>%
  left_join(wb_clean, by = c("iso3c", "year"))

# Q: what is the unit of observation?
glimpse(merged)

# How many country-years matched?
sum(!is.na(merged$gdp_pc))

# Which still didn't match?
merged %>% filter(is.na(gdp_pc)) %>% select(noc, iso3c, year) %>% distinct()


# ==============================================================
# EXERCISE 3:
# Use inner_join() instead of left_join(). How many rows do you
# get? Why is it different?
# ==============================================================

# Your code here:



# --- 3.5 Save cleaned data to output/ ---

write_csv(merged, "output/olympics_wb_merged.csv")


# =============================================================
# PART 4: ANALYSIS
# =============================================================

# --- 4.1 Create new variables ---

merged <- merged %>%
  filter(!is.na(gdp_pc), !is.na(population)) %>%
  mutate(
    medals_per_million = total_medals / (population / 1e6),
    log_gdp_pc = log(gdp_pc),
    log_pop = log(population))

# Q: what is the unit of observation?
# how many countries? how many years?1
n_distinct(merged$noc)
n_distinct(merged$year)


# --- 4.2 Visualize ---

# Total medals vs GDP per capita (all country-years pooled)
ggplot(merged, aes(x = log_gdp_pc, y = total_medals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_text(
    data = merged %>% slice_max(total_medals, n = 8),
    aes(label = noc), nudge_y = 5, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 0.8) +
  scale_size_continuous(name = "Population (millions)", range = c(1, 12)) +
  labs(
    x = "Log GDP per Capita (constant 2015 US$)",
    y = "Total Medals",
    title = "Does National Wealth Predict Olympic Success?",
    subtitle = "Summer Olympics, 2000-2016 (all country-years)") +
  theme_minimal(base_size = 14)

# Female medal share vs female labor force participation
merged %>% 
  filter(total_medals > 0) %>% 
ggplot(aes(x = fem_lfp, y = female_share)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 0.8) +
  labs(
    x = "Female Labor Force Participation Rate (%)",
    y = "Female Share of Medals",
    title = "Do countries with more women in the workforce win more women's medals?",
    subtitle = "Summer Olympics, 2000-2016") +
  theme_minimal(base_size = 14)


# --- 4.3 Regression: Simple OLS ---

# start simple: does GDP predict medals?
models <- list() # THIS IS A LIST - OR ABIG BUCKET OF SMALLER BUCKETS 
models[["Bivariate"]] <- lm(total_medals ~ log_gdp_pc, data = merged)
models[["+ Population"]] <- lm(total_medals ~ log_gdp_pc + log_pop, data = merged)

modelsummary(
  models,
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = "IC|RMSE|Log|F|R2$|Std.")

# Q: what is the problem with this regression?
# Countries like USA and China appear 5 times (once per Olympics)
# Countries that are ALWAYS rich and ALWAYS win medals drive the result
# We can't tell: is it that rich countries win more, or that some
# countries are just good at both?

# --- 4.4 Regression: Country Fixed Effects ---

# Fixed effects control for everything that is CONSTANT within a country
# (culture, geography, sports tradition, etc.)
# Now we're asking: when a country gets RICHER over time, does it win MORE medals?

models_fe <- list()
models_fe[["OLS"]] <- lm(total_medals ~ log_gdp_pc + log_pop, data = merged)
models_fe[["Country FE"]] <- lm(total_medals ~ log_gdp_pc + log_pop + factor(noc), data = merged)
models_fe[["Country + Year FE"]] <- lm(total_medals ~ log_gdp_pc + log_pop + factor(noc) + factor(year), data = merged)

modelsummary(
  models_fe,
  coef_omit = "factor\\(.*",          # hide the country and year dummies
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = "IC|RMSE|Log|F|R2$|Std.")

# Q: what happens to the GDP coefficient when we add country fixed effects?
# Q: why might it change?


# --- 4.5 Prediction ---

# Now lets USE the model to predict: how many medals would a country
# win given its GDP and population?

# use our country + year FE model
final_model <- lm(total_medals ~ log_gdp_pc + log_pop + factor(noc) + factor(year), data = merged)

# predict for every country-year in our data
merged$predicted_medals <- predict(final_model, newdata = merged)

# who overperforms? who underperforms?
merged <- merged %>%
  mutate(residual = total_medals - predicted_medals)

# top overperformers: countries that win WAY more than the model predicts
merged %>%
  select(noc, year, total_medals, predicted_medals, residual) %>%
  slice_max(residual, n = 10)

# top underperformers: countries that win WAY fewer than expected
merged %>%
  select(noc, year, total_medals, predicted_medals, residual) %>%
  slice_min(residual, n = 10)

# visualize: predicted vs actual
ggplot(merged, aes(x = predicted_medals, y = total_medals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(
    data = merged %>% slice_max(abs(residual), n = 10),
    aes(label = paste0(noc, " ", year)), size = 2.5, nudge_y = 3) +
  labs(
    x = "Predicted Medals (from model)",
    y = "Actual Medals",
    title = "Predicted vs. Actual Medal Counts",
    subtitle = "Points above the line = overperformers") +
  theme_minimal(base_size = 14)

# predict for a HYPOTHETICAL country
# Q: how many medals would a country with Nigeria's population
# but Norway's GDP per capita win?
# We need a simpler model for this (no FE — can't predict for a "new" country)

simple_model <- lm(total_medals ~ log_gdp_pc + log_pop, data = merged)

hypothetical <- tibble(
  log_gdp_pc = log(75000),     # Norway-ish GDP per capita
  log_pop = log(200000000)      # Nigeria-ish population
)

predict(simple_model, newdata = hypothetical)

# Q: does that seem reasonable? what's missing from the model?


# --- 4.6 Discussion: who beats the model? ---

# average residual per country across all Olympics
country_residuals <- merged %>%
  group_by(noc) %>%
  summarize(mean_residual = mean(residual, na.rm = TRUE)) %>%
  arrange(desc(mean_residual))

# top 10 overperformers + top 10 underperformers
top10 <- country_residuals %>% slice_max(mean_residual, n = 30)
bottom10 <- country_residuals %>% slice_min(mean_residual, n = 30)
outliers <- bind_rows(top10, bottom10)

# diverging bar chart: bars go left (underperform) or right (overperform)
ggplot(outliers, aes(x = reorder(noc, mean_residual), y = mean_residual,
                     fill = mean_residual > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    guide = "none") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(x = NULL, y = "Average Residual (Actual - Predicted Medals)",
       title = "Who Beats the Model?",
       subtitle = "Top 10 overperformers and underperformers, Summer Olympics 2000-2016") +
  theme_minimal(base_size = 14)

# Q: look at this together as a class
# - what do the overperformers have in common?
# - what do the underperformers have in common?
# - what is the model MISSING? (state investment in sport? history?
#   hosting advantage? political system?)
# - could you add a variable to the model that would explain these gaps?

