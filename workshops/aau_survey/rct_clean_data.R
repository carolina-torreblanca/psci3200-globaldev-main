# -------------------------------------------------------------------------
## Build dataset for RCT IE Analysis
## 
# -------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
source(here("code", "funcs.R"))

# Read in Baseline Qualtrics data --------------------------------------------------

## baseline data, blocks, and assignment (original + reserve)
baseline = read_rds(here("data", "logistics", "data_assignment.rds"))  # created with code/logistics/block_randomization.R  

# Read in status data -----------------------------------------------------

## reserve sample realization
replace_uncontacted = read_csv(here("data", "participants", "reserve_list", "new_data_reserved.csv")) # NA in final column means they didn't pick up their phone (however, 4 are in the T attendance list we read-in later)
stopifnot(table(replace_uncontacted[is.na(replace_uncontacted$...22), ]$recipient_email %in% baseline$recipient_email )["TRUE"] == 22)

## create column in baseline data indicating final treatment status (for ITT)
baseline = baseline %>%
  mutate(reserve_nc = case_when(recipient_email %in% replace_uncontacted[is.na(replace_uncontacted$...22), ]$recipient_email ~ 1, reserve == 1 ~ 0)) %>%
  mutate(treatment_final = if_else( (treatment == 1 | (reserve == 1 & reserve_nc != 1)), 1, 0 ) )

## group assignments by day (created with code/logistics/group_randomization_dayx.R)
gda = list.files(path = here("data", "participants"),  
                 pattern = "group_assignment_day\\d.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.x, col_types = cols(.default = "c")), .id = "file_name") %>%
  rename(tef_day = file_name)

## merge group assignment with baseline
baseline = baseline %>%
  left_join(gda %>% select(!q48_7))

# This is a bit weird: 
# 5 were included in the lists from Saron for group randomization but were also listed as not being contacted (NA for column ...22) on the reserve_list/new_data_reserved.csv
# 4 attended; 1 did not (alazarkebede390@gmail.com)
# Best guess: before final randomization, they succeeded in contacting several that did not respond when the earlier randomization list was being assembled but failed to update reservation list
# table(is.na(baseline$tef_day), baseline$treatment_final)
# baseline %>% filter(!is.na(tef_day) & treatment_final == 0) %>% select(recipient_email)

rm(replace_uncontacted, gda)

# Read in attendance data -------------------------------------------------

## read-in actual attendance records
attendance = list.files(path = here("data", "IPD"),  
                        pattern = "Total.*Participants.csv", full.names = TRUE) %>% # I had to manually save each as csv
  map_df(~read_csv(.x, col_types = cols(.default = "c"), col_names = F), .id = "file_name")

## create attendance variable (for TOT)
baseline = baseline %>%
  mutate(attendance = if_else( recipient_email %in% attendance$X1, 1, 0))

# extract rows in attendance but not treatment (these were reserves with NA in contact column, leading us to believe they were not called)
baseline = baseline %>% 
  # filter(treatment_final == 0 & attendance == 1) %>%
  # select(recipient_email)
  mutate(treatment_final = ifelse(treatment_final == 0 & attendance == 1, 1, treatment_final))
# check distribution of attendees by treatment group 
table(baseline$treatment_final, baseline$attendance)

# clean endline data -------------------------------------------


## original endline data
raw_endline = read_csv(here("data", "raw", "endline.csv")) %>% 
  janitor::clean_names() %>% 
  # get rid of anonymous links; these are either tests or scammers
  filter(distribution_channel != "anonymous") %>% 
  rename(treatment_status = external_reference) %>% 
  mutate(sample = "endline")


# make dictionary
endline_dictionary = make_dictionary(raw_endline)

# cols to drop
drop = c("status", "ip_address", "duration_in_seconds", 
         "recorded_date", "recipient_last_name",
         "recipient_first_name", "distribution_channel", "q46")

# merge and drop
subset_endline = 
  raw_endline %>% 
  select(-all_of(drop), -contains("q48_"),
         -contains("q47_"), -matches("seed"), 
         -matches("traits"))


# clean data
clean_endline = subset_endline %>% 
  clean_data()


# update dictionary
endline_dictionary = bind_rows(endline_dictionary) %>% 
  filter(qualtrics_name %in% names(clean_endline))


# add labels to dataframe
dict_labels = endline_dictionary %>% 
  select(qualtrics_name, description) %>% 
  deframe() %>% 
  as.list()

clean_endline = clean_endline %>% 
  labelled::set_variable_labels(.labels = dict_labels, .strict = FALSE)




# clean baseline and merge in ---------------------------------------------


# what variables to carry over from baseline?
baseline_ctrls = read_csv(here("data", "clean", "outcome_labels_full.csv")) %>% 
  drop_na(var_baseline) %>% 
  pull(var_baseline) %>% 
  unique()

# subset to relevant variables in baseline data so we can merge into endline
clean_baseline = baseline %>% 
  as_tibble() %>% 
  # clean relevant variables
  clean_base() %>% 
  # keep variables to merge into endline
  select(any_of(baseline_ctrls), recipient_email, blocks, treatment_final, attendance) %>% 
  # append baseline to these vars
  rename_with(.cols = -c(recipient_email, blocks, treatment_final, attendance), .fn = ~ paste0(.x, "_baseline"))


# # did all controls make it?
# baseline_ctrls %in% str_remove(names(clean_baseline), "_baseline") # yes
# 
# # are all endline participants in baseline?
# all(clean_endline$recipient_email %in% clean_baseline$recipient_email) # yes


# merge in
clean_endline = 
  left_join(clean_endline, clean_baseline, by = "recipient_email")

# create new column with z-scores for each outcome ------------------------

# read outcome labels
outcome_dictionary = read_csv(here("data", "clean", "outcome_labels_full.csv")) %>% 
  # just original outcome variables from PAP
  drop_na(var) %>% 
  slice(1:58) %>%
  mutate(type = str_replace(type, "Out-group", "Outgroup") )

# get vars for type of outcomes we want
cols = outcome_dictionary %>% 
  pull(var)

cols_base = outcome_dictionary %>% 
  drop_na(var_baseline) %>% 
  mutate(var_baseline = paste0(var_baseline, "_baseline")) %>%
  pull(var_baseline)

cols = c(cols, cols_base)

# create new columns that will be standardized
clean_endline[, paste0(cols, "_st")] = clean_endline[, cols]
cols = names(clean_endline[, paste0(cols,"_st")])
clean_endline = clean_endline %>% ungroup()

# Create z-score function from Kling, Liberman, and Katz (2007)
z_score = function(x, y){
  # calculate mean and sd of control group
  c_mean = mean( as.numeric( unlist(x[x["treatment_final"] == 0, y])) , na.rm = T)
  c_sd = sd( as.numeric( unlist(x[x["treatment_final"] == 0, y])) , na.rm = T)
  # subtract control group mean; divide by control group SD
  ( as.numeric(x[, y, drop = TRUE]) - c_mean) / c_sd
}

# calculate z-scores
for (i in cols) {
  clean_endline[,i] = z_score(clean_endline, i)
}


# create averaged z-score for each outcome family -------------------------
index_frame = data.frame(matrix( ncol = ncol(outcome_dictionary), nrow=0, dimnames = list(NULL, names(outcome_dictionary) )))

for (i in unique(outcome_dictionary$type)[unique(outcome_dictionary$type) !="Identity Rank"] ) {
  # endline index variables
  vars = outcome_dictionary %>% filter(type == i) %>% rowwise() %>% mutate(var = str_c(var, "_st"))%>% ungroup() %>% pull(var)
  if(length(vars) > 1){
    index = paste0(gsub(" ", "_", i), "_Index")
    clean_endline = clean_endline %>% rowwise() %>% mutate( "{index}" := mean(c_across(all_of(vars)), na.rm = TRUE)) %>% ungroup()
  } else{
    index = NA
  }
  # baseline index variables
  vars = outcome_dictionary %>% filter(type == i) %>% rowwise() %>% mutate(var_baseline = str_c(var_baseline, "_baseline_st")) %>% ungroup() %>% drop_na(var_baseline) %>% pull(var_baseline)
  if(length(vars) > 1 ){
    index_base = paste0(gsub(" ", "_", i), "_Index_baseline")
    clean_endline = clean_endline %>% rowwise() %>% mutate( "{index_base}" := mean(c_across(all_of(vars)), na.rm = TRUE)) %>% ungroup()
  } else{
    index_base = NA
  }
  # store names
  index_frame[i ,] = c(index, "Index", "z-score", i, gsub("_baseline", "", index_base), NA)
}


# final format of outcome dictionary --------------------------------------

# bind index variables into outcome dictionary for use when running models
outcome_dictionary = bind_rows(index_frame %>% `rownames<-`( NULL ) %>% drop_na(var), outcome_dictionary %>% arrange(type, label) ) %>%
  arrange(type)
#sapply(clean_endline[, c(index_frame$var, index_frame[!is.na(index_frame$var_baseline),]$var_baseline)], function(x) sum(is.na(x)))

# add variable range to outcome dictionary
outcome_dictionary$var_range = NA
for(i in 1:nrow(outcome_dictionary)){
  vr = range( as.numeric(unlist(clean_endline[, outcome_dictionary[i,]$var])), na.rm = T)
  if( str_detect(outcome_dictionary[i,]$var, "_Index") ){
    outcome_dictionary[i,]$var_range = paste0("scale: ", round(vr[1], digits = 1), "-", round(vr[2], digits = 1))
  } else{
    outcome_dictionary[i,]$var_range = paste0("scale:", round(vr[1]), "-", round(vr[2]) )
  }
}


# output ------------------------------------------------------------------

write_rds(clean_endline, file = here("data", "clean", "clean_endline.rds"))
write_rds(endline_dictionary, file = here("data", "clean", "dictionary_endline.rds"))
write_csv(outcome_dictionary, file = here("data", "clean", "outcome_labels_full_index.csv"))

