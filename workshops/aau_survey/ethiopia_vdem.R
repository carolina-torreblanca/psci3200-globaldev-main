# libraries
library(tidyverse)
library(vdemdata)
library(countrycode)
source("code/funcs.R")

# dictionary
dict = tribble(~dirty, ~clean,
        "v2x_delibdem", "Deliberative democracy (0-1)",
        "v2x_libdem", "Liberal democracy (0-1)",
        "v2x_egaldem", "Egalitarian democracy (0-1)",
        "v2x_partipdem", "Participatory democracy (0-1)",
        "v2x_freexp_altinf", "Free expression (0-1)",
        "v2x_polyarchy", "Polyarchy (0-1)")


# oecd countries
oecd = c("United States of America", 
         "Germany", "France", "United Kingdom", "Australia",
         "Austria", "Belgium", "Canada", "Chile", "Colombia", 
         "Costa Rica", "Czech Republic", "Denmark", "Estonia", 
         "Finland", "Greece", "Hungary", "Iceland", "Ireland", 
         "Israel", "Italy", "Japan", "South Korea", "Latvia", 
         "Lithuania", "Mexico", "Netherlands", "New Zealand", 
         "Norway", "Poland", "Portugal", "Slovak Republic", 
         "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey")

# get data
subset_dem = vdem |> 
  select(e_regiongeo, country_name, year, v2x_polyarchy, v2x_libdem, 
         v2x_partipdem, v2x_delibdem,
         v2x_egaldem, v2x_freexp_altinf) |> 
  filter(year >= 1945) |> 
  filter(country_name %in% c("Ethiopia") | country_name %in% oecd) |> 
  as_tibble() |> 
  mutate(is_ethiopia = country_name == "Ethiopia") |> 
  pivot_longer(cols = contains("v2x_"), names_to = "index",
               values_to = "values") |> 
  left_join(dict, by = c("index" = "dirty"))


grouped_avg_dem = subset_dem |> 
  group_by(year, is_ethiopia, clean) |> 
  summarise(values = mean(values, na.rm = TRUE)) |> 
  mutate(is_ethiopia = case_when(is_ethiopia == TRUE ~ "Ethiopia", 
                                 is_ethiopia == FALSE ~ "OECD average"))







# Ethnicity ---------------------------------------------------------------



# dictionary
dict = tribble(~dirty, ~clean,
               "v2peapssoc", "Equality of access to public services (latent interval)",
               "v2peasjsoc", "Equality of access to state jobs",
               "v2peasbsoc", "Equality of access to business opportunities", 
               "v2lgdsadlo", "Representation of disadvantaged groups (latent interval)")

# subset data
subset_ethn = vdem |> 
  select(country_name, year, v2peapssoc, v2peasjsoc, 
         v2peasbsoc, v2lgdsadlo) |> 
  filter(year >= 1945) |> 
  filter(country_name %in% c("Ethiopia") | country_name %in% oecd) |> 
  as_tibble() |> 
  mutate(is_ethiopia = country_name == "Ethiopia") |> 
  pivot_longer(cols = v2peapssoc:v2lgdsadlo, names_to = "index",
               values_to = "values") |> 
  left_join(dict, by = c("index" = "dirty"))


# take averages
grouped_avg_ethn = subset_ethn |> 
  group_by(year, is_ethiopia, clean) |> 
  summarise(values = mean(values, na.rm = TRUE)) |> 
  mutate(is_ethiopia = case_when(is_ethiopia == TRUE ~ "Ethiopia", 
                                 is_ethiopia == FALSE ~ "OECD average"))




## combined figure
plot_data = bind_rows(filter(grouped_avg_dem, 
                             str_detect(clean, "Participatory|Free")), 
          filter(grouped_avg_ethn, str_detect(clean, "services|Representation"))) |> 
  mutate(clean = fct_relevel(clean, "Equality of access to public services (latent interval)",
                             after = Inf))


ggplot(plot_data, aes(x = year, y = values, color = is_ethiopia)) + 
  geom_line(linewidth = 1.8) + 
  facet_wrap(vars(clean), scales = "free_y", 
             ncol = 2) + 
  labs(x = NULL, y = "Index scores", color = NULL) + 
  theme(legend.position = "top") + 
  scale_color_manual(values = c("black", "grey60"))

ggsave(filename = "figures/vdem_ethiopia_combined.pdf", 
       device = cairo_pdf, width = 12, height = 7)

ggsave(filename = "figures/vdem_ethiopia_combined.png", 
       device = "png", width = 12, height = 7)
