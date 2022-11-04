#' Main analysis for empirical exercise 3
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, janitor, rdrobust, reshape2, rddensity, tibble, ivreg)

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

#6.1 Table 3 Panel A
tab3_data_temp <- data %>%
  filter(benefit == "B") %>%
  filter(year == 2006) %>%
  filter((LISPremium>=-4) & (LISPremium<=4))

uniqueID_list_RD_window2006 = unique(tab3_data_temp$uniqueID)

tab3_data <- data  %>%
  mutate(AboveBen = 1*(LISPremium>=0)) %>%
  mutate(LISPremiumPos = LISPremium*AboveBen) %>%
  mutate(BelowBen = 1*(LISPremium<=0)) %>%
  mutate(LISPremiumNeg = LISPremium*BelowBen) %>%
  mutate(BelowBen_2006_temp = 1*((year == 2006)&(BelowBen == 1))) %>%
  group_by(uniqueID) %>%
  mutate(L1.LISPremiumPos = lag(LISPremiumPos, n = 1, default = NA, order_by=year)) %>%
  mutate(L2.LISPremiumPos = lag(LISPremiumPos, n = 2, default = NA, order_by=year)) %>%
  mutate(L3.LISPremiumPos = lag(LISPremiumPos, n = 3, default = NA, order_by=year)) %>%
  mutate(L4.LISPremiumPos = lag(LISPremiumPos, n = 4, default = NA, order_by=year)) %>%
  mutate(L1.LISPremium = lag(LISPremium, n = 1, default = NA, order_by=year)) %>%
  mutate(L2.LISPremium = lag(LISPremium, n = 2, default = NA, order_by=year)) %>%
  mutate(L3.LISPremium = lag(LISPremium, n = 3, default = NA, order_by=year)) %>%
  mutate(L4.LISPremium = lag(LISPremium, n = 4, default = NA, order_by=year)) %>%
  mutate(L1.LISPremiumNeg = lag(LISPremiumNeg, n = 1, default = NA, order_by=year)) %>%
  mutate(L2.LISPremiumNeg = lag(LISPremiumNeg, n = 2, default = NA, order_by=year)) %>%
  mutate(L3.LISPremiumNeg = lag(LISPremiumNeg, n = 3, default = NA, order_by=year)) %>%
  mutate(L4.LISPremiumNeg = lag(LISPremiumNeg, n = 4, default = NA, order_by=year)) %>%
  ungroup()
  
uniqueID_list_BelowBen_2006 = unique(tab3_data$uniqueID[tab3_data$BelowBen_2006_temp==1])
tab3_data$BelowBen_2006 = 0
tab3_data$BelowBen_2006[tab3_data$uniqueID %in% uniqueID_list_BelowBen_2006] = 1

tab3_data_2006 <- tab3_data %>%
  filter(year == 2006) %>%
  filter(uniqueID %in% uniqueID_list_RD_window2006)

for(yr in 2007:2010){
  dist2006 = yr - 2006
  
  temp_df = tab3_data %>%
    filter(year == yr)  %>%
    filter(uniqueID %in% uniqueID_list_RD_window2006) %>%
    select(-c("LISPremiumNeg", "LISPremiumPos")) %>%
    rename("LISPremiumNeg" = paste0("L",dist2006,".LISPremiumNeg")) %>%
    rename("LISPremiumPos" = paste0("L",dist2006,".LISPremiumPos"))
  
  assign(paste0("tab3_data_", yr), temp_df)
}

models_ll = list(
  q6_2006_ll = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos, data=tab3_data_2006),
  q6_2007_ll = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos, data=tab3_data_2007),
  q6_2008_ll = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos, data=tab3_data_2008),
  q6_2009_ll = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos, data=tab3_data_2009),
  q6_2010_ll = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos, data=tab3_data_2010)
  )

modelsummary(models_ll, vcov = ~orgParentCode, 
             estimate ="{estimate}{stars}", coef_omit = "Intercept",
             gof_map = c("nobs", "r.squared"))


#6.2 Table 3 Panel B
models_sq = list(
  q6_2006_sq = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos + I(LISPremiumNeg^2) + I(LISPremiumPos^2), 
                  data=tab3_data_2006),
  q6_2007_sq = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos + I(LISPremiumNeg^2) + I(LISPremiumPos^2), 
                  data=tab3_data_2007),
  q6_2008_sq = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos + I(LISPremiumNeg^2) + I(LISPremiumPos^2), 
                  data=tab3_data_2008),
  q6_2009_sq = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos + I(LISPremiumNeg^2) + I(LISPremiumPos^2),
                  data=tab3_data_2009),
  q6_2010_sq = lm(lnS ~ BelowBen_2006 + LISPremiumNeg + LISPremiumPos + I(LISPremiumNeg^2) + I(LISPremiumPos^2),
                  data=tab3_data_2010)
)

rows <- tribble(~term, ~q6_2006_sq, ~q6_2007_sq,  ~q6_2008_sq, ~q6_2009_sq, ~q6_2010_sq,
                'Premium - Subsidy, 2006', 'Quadratic', 'Quadratic',   'Quadratic', 'Quadratic', 'Quadratic')

attr(rows, 'position') <- 3

panel_B = modelsummary(models_sq, vcov = ~orgParentCode, 
             estimate ="{estimate}{stars}", coef_map = "BelowBen_2006",
             gof_map = c("nobs", "r.squared"),
             add_rows = rows, output = "data.frame")[, c(2, 4:8)]

#6.3 Table 3 Combining Panel A and B
Tab3 = modelsummary(models_ll, vcov = ~orgParentCode, 
                    estimate ="{estimate}{stars}", coef_omit = "Intercept",
                    gof_map = c("nobs", "r.squared"),
                    add_rows = panel_B)

#7. Minimal coverage error (CE)-optimal bandwidths
q7 = function(yr, ll){
  if(ll == TRUE){
    p = 1
  } else {
    p = 2
  }
  temp_df = eval(parse(text=paste0("tab3_data_",yr)))
  result = rdrobust(temp_df$lnS, -temp_df$LISPremium, p = p, bwselect = "cerrd")
  coef = round(result$coef[1],3)
  se = round(result$se[1],3)
  bw = round(result$bws[1,1],3)
  se = paste0("(",round(se,3),")")
  result_list = data.frame(coef, se, bw)
  return(result_list)
}


for (i in 2006:2010){
  if (i == 2006){
    q7_tab_A = q7(i, ll = TRUE )
  } else {
    q7_tab_A = rbind(q7_tab_A, q7(i, ll = TRUE))
  }
}

for (i in 2006:2010){
  if (i == 2006){
    q7_tab_B = q7(i, ll = FALSE)
  } else {
    q7_tab_B = rbind(q7_tab_B, q7(i, ll = FALSE))
  }
}

rownames(q7_tab_A) = c(2006:2010)

q7_tab_A = q7_tab_A %>%
  t()

rownames(q7_tab_B) = c(2006:2010)

q7_tab_B = q7_tab_B %>%
  t()

#8. IV (Use the presence of Part D low-income subsidy as an IV for market share)
#y: ln(Premium)/x: lnS/ z: LIS
q8_data = data  %>%
  filter(benefit == "B") %>%
  mutate(lnP = log(premium))

q8_result = ivreg(lnP ~ lnS | LIS, data = q8_data)

modelsummary(q8_result, vcov = ~orgParentCode, 
             estimate ="{estimate}{stars}", coef_omit = "Intercept",
             gof_map = c("nobs", "r.squared"))



  
