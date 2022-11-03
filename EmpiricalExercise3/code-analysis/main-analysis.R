#' Main analysis for empirical exercise 3
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, janitor)

# Importing and Cleaning up the dataset -----------------------------------
in_data.path <- "EmpiricalExercise3/data/data-in/Ericson 2014/DataFiles/Data_main.dta"
out_data.path <- "EmpiricalExercise3/data/data-out"

raw_data <- read_dta(in_data.path)

data <- raw_data %>% 
  group_by(orgParentCode) %>%
  mutate(firstyr_us = min(year)) %>%
  mutate(offeredb4_us = 1*(year > firstyr_us)) %>%
  ungroup() %>%
  group_by(orgParentCode, state) %>%
  mutate(firstyr_state = min(year)) %>%
  mutate(offeredb4_state = 1*(year > firstyr_state)) %>%
  ungroup() %>%
  mutate(enhanced_benefit = 1*(benefit == "E")) %>%
  group_by(uniqueID) %>%
  mutate(cohort = min(year)) %>%
  mutate(yearOfPlan = year - cohort + 1)

# Main Analysis -------------------------------------------------------------

#1. Reproduce table 1 and 2
#Table 1: Summary statistic table
tab_1 = data %>%
  filter(yearOfPlan == 1) %>%
  group_by(cohort) %>%
  summarize(
    mean_premium = round(mean(premium, na.rm = TRUE),0), 
    std_dev_premium = round(sd(premium, na.rm = TRUE),0),
    mean_deductible = round(mean(deductible, na.rm = TRUE),0),
    std_dev_deductible = round(sd(deductible, na.rm = TRUE),0),
    frac_enhanced_benefit = round(sum(enhanced_benefit, na.rm = TRUE)/n(),2),
    frac_firm_offeredb4_us = round(sum(offeredb4_us, na.rm = TRUE)/n(),2),
    frac_firm_offeredb4_state = round(sum(offeredb4_state, na.rm = TRUE)/n(),2),
    n_unique_firm = n_distinct(orgParentCode, na.rm = TRUE),
    n_plans = n()) %>%
    transpose() %>%
    row_to_names(row_number = 1) %>%

#Table 2: 
