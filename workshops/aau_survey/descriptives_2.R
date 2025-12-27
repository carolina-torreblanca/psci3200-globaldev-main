# libraries
library(tidyverse)
library(here)
library(broom)
library(patchwork)
source(here("code/funcs.R"))

# read data
full = read_rds(here("data", "clean", "clean_endline.rds"))
outcome_dictionary = read_csv(here("data", "clean", "outcome_labels_full_index.csv"))


# political participation -------------------------------------------------

outcome = outcome_dictionary[outcome_dictionary$type == "Political engagement", c("var", "label")] %>%
  filter( var %in% c("q13_4_1", "q16_3", "q15_3", "q13_5_1") )

participate.bin = 
  full %>% 
  select(outcome$var) %>%
  mutate_at( outcome$var, as.numeric) %>%
  mutate(`Contacted gov't official` = ifelse(`q13_4_1`>0,1,0),
         `Intention to join political party` = ifelse(`q16_3`>0,1,0),
         `Member of political party` = ifelse(`q15_3`>0,1,0),
         `Signed a petition` = ifelse(`q13_5_1`>0,1,0)) %>%
  select(!outcome$var)

ggplot(data.frame(Mean = colMeans(participate.bin, na.rm=T), Items = names(participate.bin))) +
  geom_col(aes(Items, Mean),fill = palette[8]) +
  labs(x = "Participation Types",
       y = "Share of Students That Participated", 
       title = "How have students participated since June 2022?")

ggsave(filename = here("figures/endline-participation-share.png"),
       device = "png", width = 10, height = 6)

