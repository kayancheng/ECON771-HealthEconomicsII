#' Main analysis for empirical exercise 2
#' Ka Yan CHENG
#' 

# Preliminaries -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, ivmodel)

# Load workspace for the data -----------------------------------------------
load("EmpiricalExercise2/data/data-out/emp_ex2_df.RData")
load("EmpiricalExercise2/data/data-out/emp_ex2_iv.RData")

# Merge the 2 dataframes ----------------------------------------------------
#Create variables for 1. total spending/ 2. claims/ 3. patients
PUF = PUF %>%
  transform(spending = AVERAGE_MEDICARE_ALLOWED_AMT*LINE_SRVC_CNT,
            claims = LINE_SRVC_CNT,
            patients = BENE_UNIQUE_CNT) %>%
  select(c("NPI", "YEAR", "spending", "claims", "patients"))

colnames(PUF) = tolower(colnames(PUF))

PUF_y_i = aggregate(.~npi+year, PUF, sum) #Aggregate across practices of same physician in same year

colnames(MDPPAS) = tolower(colnames(MDPPAS))

combined_df = PUF_y_i %>%
  left_join(MDPPAS,
            by = c("npi", "year")) %>%
  mutate(int = ifelse((pos_opd/(pos_opd + pos_office + pos_asc)>=0.75),1,0)) %>%
  drop_na()

# Main Analysis -------------------------------------------------------------

#1.Summary statistics on physician-level Medicare spending, claims, and patients
#mean/standard deviation/min/max

#Summary statistic table
#1. Spending
tab_spending = combined_df %>%
  group_by(year) %>% 
  summarize(
    mean = mean(spending, na.rm = TRUE), 
    std_dev = sd(spending, na.rm = TRUE),
    min = min(spending, na.rm = TRUE),
    max = max(spending, na.rm = TRUE)
  )

#2. Claims
tab_claims = combined_df %>%
  group_by(year) %>% 
  summarize(
    mean = mean(claims, na.rm = TRUE), 
    std_dev = sd(claims, na.rm = TRUE),
    min = min(claims, na.rm = TRUE),
    max = max(claims, na.rm = TRUE)
  )

#3.Patients
PUF_tab_pat = combined_df %>%
  group_by(year) %>% 
  summarize(
    mean = mean(patients, na.rm = TRUE), 
    std_dev = sd(patients, na.rm = TRUE),
    min = min(patients, na.rm = TRUE),
    max = max(patients, na.rm = TRUE)
  )

#2. Plot the mean of total physician-level claims for integrated versus non-integrated physicians over time.
#Create the integration variable
mean_claim_bint_df <- combined_df %>%
  group_by(year, int) %>%
  mutate(int = factor(int)) %>%
  summarize(mean = mean(claims))

#Plot the graph
mean_claim_bint_graph <- ggplot(mean_claim_bint_df, aes(x = year, y = mean, col=int))+
  geom_line(size=1.5) +
  theme(plot.title = element_text(size=12)) +
  labs(title = "Mean of total physician-level claims from 2012 to 2017",
       subtitle = "Integrated versus Non-integrated Physicians",
       x = "Year",
       y = "Mean number of claims")+
  scale_color_discrete(name="Physicians Type",
                       labels=c("Non-integrated","Integrated"))

#3. TWFE
#Change claims to log_claims variable
combined_df = combined_df %>%
  mutate(claims = log(claims))

#Left with just physicians that havent integrated yet in 2012
npi_list_ni2012 = combined_df[(combined_df$year == 2012 & combined_df$int == 0),]$npi
combined_df_ni2012 = combined_df %>%
  filter(npi %in% npi_list_ni2012)
TWFE = feols(claims ~ int | npi + year, data= combined_df_ni2012)

#4. Robustness Checks for Omitted Variable Bias
#Set up the grid
rho_grid = c(0.5, 1, 1.5, 2)
R_sq_max_grid = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

#Gather relevant info from TWFE
delta_D_x1 = as.numeric(TWFE$coefficients)
R_sq_D_x1 = as.numeric(r2(TWFE, "ar2"))

#Do the only dummy regression
d_reg = summary(lm(claims ~ int-1, data= combined_df_ni2012))
delta_D = d_reg$coefficients[1]
R_sq_D = d_reg$adj.r.squared

#Function to get the delta_star
delta_star = function(R_sq_max, rho){
  delta_s = delta_D_x1 - 
    rho*(delta_D - delta_D_x1)*((R_sq_max - R_sq_D_x1)/(R_sq_D_x1 - R_sq_D))
  return(delta_s)
}

#Function to construct the bound format
bound_fn = function(value){
  paste0("[",round(delta_D_x1,2), ",", round(value,2),"]")
}

#Table to report the result
robomit_result = as.data.frame(outer(rho_grid, R_sq_max_grid, delta_star)) %>%
  mutate_all(bound_fn)
rownames(robomit_result) = rho_grid
colnames(robomit_result) = R_sq_max_grid

#5. IV-2SLS-TWFE
#Add iv into the dataframe
combined_df_ni2012 = combined_df_ni2012 %>%
  left_join(iv,
            by = c("year", "group1"="tax_id")) %>%
  drop_na()
  
#First Stage: Regress X on Z
fs = feols(int ~ practice_rev_change | npi + year, data= combined_df_ni2012)
combined_df_ni2012$int_hat = fs$fitted.values

#Reduced form: Regress Y on Z
rf = feols(claims ~ practice_rev_change | npi + year, data= combined_df_ni2012)

#2SLS Estimator: Regress Y on Xhat
TSLS = feols(claims ~ int_hat | npi + year, data= combined_df_ni2012)

#6. Durbin-Wu-Hausman test 
#First step:
combined_df_ni2012$v_hat = combined_df_ni2012$int - combined_df_ni2012$int_hat
#Second step:
DW_S2 = feols(claims ~ int_hat + v_hat | npi + year, data= combined_df_ni2012)

#7. Weak instruments
#AR Wald Statistics (We will need to use FWL)
#FWL first step (claims on fes)
wi_fwl_1 = feols(claims ~ 1 | npi + year, data= combined_df_ni2012)
combined_df_ni2012$y_res = wi_fwl_1$residuals
#FWL second step (int on fes)
wi_fwl_2 = feols(int ~ 1 | npi + year, data= combined_df_ni2012)
combined_df_ni2012$int_res = wi_fwl_2$residuals
#FWL third step (int on fes)
wivm = ivmodelFormula(y_res~int_res|practice_rev_change, 
                      data = combined_df_ni2012)

AR_ci = AR.test(wivm)$ci.info
#v.s. standard t-statistics
TSLS_ci = paste0("[",TSLS$coeftable$Estimate-1.96*TSLS$coeftable$`Std. Error`, ",",
                TSLS$coeftable$Estimate+1.96*TSLS$coeftable$`Std. Error`,"]")

#tF adjusted standard error
#Inspecting the first step F statistics:
fs_f = (fs$coefficients[["practice_rev_change"]]/fs$se[["practice_rev_change"]])^2
#Since the first stage f-statistics is so huge, no adjustment will be made
#Therefore the test conclusion will be the same too

#8. Borusyak and Hull (2021)
for (y in 2012:2017){
  iv_temp = iv %>%
    filter(year == y)
  iv_vec = as.numeric(unlist(iv_temp["practice_rev_change"]))
  len = length(iv_vec)
  mu_vec = replicate(len,mean(sample(iv_vec,size = 100, replace = TRUE)))
  if (y == 2012){
    mu = mu_vec
  } else {
    mu = append(mu, mu_vec)
  }
}

iv$iv_mu = mu

#Recentered iv
iv = iv%>%
  mutate(iv_rc = practice_rev_change-mu)

#Add the recentered iv into df
combined_df_ni2012 = combined_df_ni2012 %>%
  left_join(iv,
            by = c("year", "group1"="tax_id", "practice_rev_change"))

#Re-estimate 2SLS
#First Stage: Regress X on Z
nfs = feols(int ~ iv_rc | npi + year, data= combined_df_ni2012)
combined_df_ni2012$nint_hat = nfs$fitted.values

#2SLS Estimator: Regress Y on Xhat
nTSLS = feols(claims ~ nint_hat | npi + year, data= combined_df_ni2012)

# Save workspace to pass to Rmd ------------------------------------------------
save.image (file = "EmpiricalExercise2/emp_ex2.RData")







