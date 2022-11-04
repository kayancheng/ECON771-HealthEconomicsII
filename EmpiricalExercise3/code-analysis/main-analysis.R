#' Main analysis for empirical exercise 3
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, janitor, rdrobust, reshape2)

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

rd.result_quartic <- rdplot(fig3_data$lnS, fig3_data$LISPremium, 
                    c = 0,
                    p = 4,
                    nbin = nbin,
                    h = 10,
                    title = "",
                    x.label="Monthly Premium - LIS Subsidy, 2006", 
                    y.label="Log Enrollment Share, 2006")

rd.result_linear <- rdplot(fig3_data$lnS, fig3_data$LISPremium, 
                    c = 0,
                    p = 1,
                    nbin = nbin,
                    h = 4,
                    title = "",
                    x.label="Monthly Premium - LIS Subsidy, 2006", 
                    y.label="Log Enrollment Share, 2006")

bin.avg_linear <- as_tibble(rd.result_linear$vars_bins)
poly_linear <- as_tibble(rd.result_linear$vars_poly)

bin.avg_quartic <- as_tibble(rd.result_quartic$vars_bins)
poly_quartic <- as_tibble(rd.result_quartic$vars_poly)

plot.q2 <- bin.avg_linear %>% ggplot(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + 
  geom_point() + theme_bw() +
  poly_linear %>% geom_line(mapping = aes(x=rdplot_x,y=rdplot_y), size = 0.8, linetype = "dashed", colour = "gray69") + 
  poly_quartic %>% geom_line(mapping = aes(x=rdplot_x,y=rdplot_y), size = 0.5) +
  xlab("Monthly Premium - LIS Subsidy, 2006") + ylab("Log Enrollment Share, 2006")



