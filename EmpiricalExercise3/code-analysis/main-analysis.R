#' Main analysis for empirical exercise 3
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, janitor, rdrobust, reshape2, rddensity)

# Importing and Cleaning up the dataset -----------------------------------
in_data.path <- "EmpiricalExercise3/data/data-in/Ericson 2014/DataFiles"
out_data.path <- "EmpiricalExercise3/data/data-out"

raw_data_main <- read_dta(paste0(in_data.path, "/Data_main.dta"))
raw_data_sub <- read_dta(paste0(in_data.path, "/Data_subsidyinfo.dta"))

data_main <- raw_data_main %>% 
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

data_sub <- raw_data_sub %>%
  melt(id = c("PDPregion")) %>%
  mutate(year = as.numeric(substring(variable, 2))) %>%
  select("PDPregion", "year", "value") %>%
  rename("LISsubsidy" = "value") 

data <- data_main %>%
  left_join(data_sub, by = c("PDPregion", "year")) %>%
  mutate(LISPremium = premium - LISsubsidy)%>%
  group_by(state, year) %>%
  mutate(stateYrEnroll = sum(enrollment, na.rm = T)) %>%
  ungroup() %>%
  mutate(S = enrollment/stateYrEnroll)%>%
  mutate(lnS = log(S))

# Main Analysis -------------------------------------------------------------

#1. Reproduce table 1
#Table 1: Summary statistic table
tab_1 = data_main %>%
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
    t() %>%
    row_to_names(row_number = 1)

#2. Reproduce Figure 3
#Figure 3: Effect of 2006 Benchmark Status on 2006 Enrollment (RD)
fig3_data <- data %>%
  filter(year == 2006) %>%
  filter((LISPremium>=-10) & (LISPremium<=10)) %>%
  filter(benefit == "B") %>%
  select("uniqueID", "LISPremium", "lnS")

binsize = 0.5
nbin = 10/binsize

source("EmpiricalExercise3/code-analysis/rd_graph3_fn.R")
q2_graph = erison_fig3(nb = nbin, return_obj = "graph")

#3. Figure 3 with different nbin
#3.1 nbin = 10
q3_graph_nb10 = erison_fig3(nb = 10, return_obj = "graph")

#3.2 nbin = 30
q3_graph_nb30 = erison_fig3(nb = 30, return_obj = "graph")

#4. Find the optimal number of bins with an evenly-spaced binning strategy
q4_graph = erison_fig3(nb = "auto", return_obj = "graph")

q4_lin_mod = erison_fig3(nb = "auto", return_obj = "linear_mod")
optimal_h_lin = q4_lin_mod$J_MV

#5. Manipulation tests
q5_test_result = rddensity(fig3_data$LISPremium)
suppressWarnings(summary(q5_test_result))
q5_fig = rdplotdensity(q5_test_result, fig3_data$LISPremium, plotRange = c(-10, 10))

#6. Table 3 Panel A and B
  
