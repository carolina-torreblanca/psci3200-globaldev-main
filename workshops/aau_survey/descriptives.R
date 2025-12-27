# libraries
library(tidyverse)
library(here)
library(broom)
library(patchwork)
source(here("code/funcs.R"))

# read data
full = read_rds(here("data", "clean", "clean_endline.rds"))


# who were messages sent to? and how many?
p1 = full$q82_split %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  ggplot(aes(x = reorder(., n), y = n)) + 
  geom_col(fill = palette[8]) + 
  coord_flip() + 
  scale_x_discrete(labels = scales::label_wrap(20)) + 
  labs(x = NULL, y = "Number of messages sent", 
       title = "Who did students want to contact?")

#TODO: some orgs maybe got zero messages and that's not captured in this graph


p2 = full %>% 
  group_by(q82) %>% 
  tally() %>% 
  ggplot(aes(x = factor(q82), y = n)) + 
  geom_col(fill = palette[8]) + 
  coord_flip() + 
  labs(x = "Number of messages sent by each student",
       y = "Number of students", 
       title = "How much messages did students send?")


p1 / p2
ggsave_both(folder = "figures", name = "sent_messages")


# participation
participate = full %>% 
  select(`Attend meeting` = q13_1_1, 
         `Protest` = q13_2_1, 
         `NGO event` = q13_3_1, 
         `Signed petition` = q13_4_1, 
         `Contact official` = q13_5_1, 
         `Contact student representative` = q13_6_1, 
         `Contact an NGO` = q13_7_1)

ggplot(data.frame(Mean = colMeans(participate, na.rm=T), 
                  Items = names(participate))) +
  geom_col(aes(Items, Mean),fill = palette[8]) +
  labs(x = "Participation Types",
       y = "Average Number of Times", 
       title = "How have students participated since June 2022?")

ggsave(filename = here("figures/endline-participation-mean.pdf"), 
       device = cairo_pdf, width = 10, height = 6)

ggsave(filename = here("figures/endline-participation-mean.png"), 
       device = "png", width = 10, height = 6)

#share participated
participate.bin = 
  participate %>% 
  mutate(`Attend meeting` = ifelse(`Attend meeting`>0,1,0),
         `Protest` = ifelse(`Protest`>0,1,0),
         `NGO event` = ifelse(`NGO event`>0,1,0),
         `Signed petition` = ifelse(`Signed petition`>0,1,0),
         `Contact official` = ifelse(`Contact official`>0,1,0),
         `Contact student representative` = ifelse(`Contact student representative`>0,1,0),
         `Contact an NGO` = ifelse(`Contact an NGO`>0,1,0))

ggplot(data.frame(Mean = colMeans(participate.bin, na.rm=T), Items = names(participate.bin))) +
  geom_col(aes(Items, Mean),fill = palette[8]) +
  labs(x = "Participation Types",
       y = "Share of Students That Participated", 
       title = "How have students participated since June 2022?")

ggsave(filename = here("figures/endline-participation-share.png"),
       device = "png", width = 10, height = 6)

ggsave(filename = here("figures/endline-participation-share.pdf"),
       device = cairo_pdf, width = 10, height = 6)

# obstacles
obstacle = full %>% 
  select(`Lack of resources` = q14_1, 
         `Fear of judgment` = q14_2, 
         `Fear of consequences` = q14_3, 
         `Lack of interest` = q14_4, 
         `Lack of information` = q14_5) %>% 
  drop_na() %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  tally() %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(value = fct_relevel(value,"Always","Often","Sometimes","Never"), 
         value = fct_rev(value))

ggplot(obstacle, aes(x = value, y = pct)) + 
  geom_col(fill = palette[8]) + 
  facet_wrap(vars(name), scales = "free") + 
  hrbrthemes::scale_y_percent(limits = c(0, 1)) + 
  labs(y = "Percent of respondents", x = NULL, 
       title = "How often do obstacles prevent participation?", 
       subtitle = glue::glue("Number of respondents = {scales::comma(sum(obstacle$n)/5)}")) + 
  scale_x_discrete(labels = scales::label_wrap(10))

ggsave(filename = here("figures/endline-obstacles.pdf"), 
       device = cairo_pdf, width = 10, height = 6)

ggsave(filename = here("figures/endline-obstacles.png"), 
       device = "png", width = 10, height = 6)
