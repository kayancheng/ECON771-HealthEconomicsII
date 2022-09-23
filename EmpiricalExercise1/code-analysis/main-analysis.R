#' Main analysis for empirical exercise 1
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stargazer, withr, fixest, modelsummary, did, HonestDiD)

out_dir =  "EmpiricalExercise1/output"

# Import data -------------------------------------------------------------
out_data.path <- "EmpiricalExercise1/data/data-out/"

dataset_list = c("HCRIS_Data", "medicaid_expansion", "pos-data-combined")

for (i in dataset_list){
  assign( paste0(i, "_df"),
    read.table(paste0(out_data.path, i ,".txt"),
               header = TRUE, fill = TRUE, 
               colClasses=c("character"),
               check.names = FALSE,
               sep = '\t', quote = "")
  )
}

# Cleaning data -------------------------------------------------------------

##1. Tidy up individual dataframe
## Medicaid_expansion
medicaid_expansion_df = 
  medicaid_expansion_df %>%
  mutate(State = c(state.abb, 'DC')[match(State, #abbreviation of state (used in HCRIS and pos)
                                          c(state.name, 'District of Columbia'))],
    year = format(as.Date(medicaid_expansion_df$date_adopted), format="%Y")) %>%
  rename(year_expand = year, state = State, expanded_ever = expanded) %>% 
  select(state, expanded_ever, year_expand)
  
## POS
`pos-data-combined_df` = 
  `pos-data-combined_df` %>%
  rename(provider_number = provider) %>%
  filter(category == "Hospital") %>%
  select(provider_number, state, own_type, year)%>%
  mutate(private = ifelse( own_type == "Non-profit Private" | own_type == "Profit" , 1, 0),
         non_profit_private = ifelse( own_type == "Non-profit Private" , 1, 0))

## HCRIS
HCRIS_Data_df = 
  HCRIS_Data_df %>%
  filter(year>=2003 & year <=2019) %>%
  select(provider_number, uncomp_care, tot_pat_rev, year)

##2. Merging into one dataset with only relevant variables (also drop rows with na uncomp_care)
combined_df = HCRIS_Data_df %>%
  
                    left_join(`pos-data-combined_df`,
                    by = c("provider_number", "year")) %>%
  
                    left_join(
                    medicaid_expansion_df,
                    by = "state") %>%
  
                    drop_na(uncomp_care, tot_pat_rev, expanded_ever) %>%
  
                    transform(uncomp_care = as.numeric(uncomp_care), 
                              tot_pat_rev = as.numeric(tot_pat_rev),
                              year = as.numeric(year), 
                              year_expand = as.numeric(year_expand),
                              expanded_ever = as.integer(as.logical(expanded_ever))) %>%
  
                    transform(uncomp_care = uncomp_care/1000000, 
                              tot_pat_rev = tot_pat_rev/1000000) %>%
                    
                    mutate(expanded_t = ifelse(year < year_expand | is.na(year_expand), 0, 1))

# Main Analysis -------------------------------------------------------------

## 1.Summary statistics on hospital total revenues and uncompensated care
   #mean/standard deviation/min/max
with_dir(out_dir, stargazer(combined_df[2:3]*(1/1000000), 
                                    type = "latex", digits = 1, 
                                    title = "Summary Statistics (in Million Dollars)",
                                    style = "qje",
                                    out = "summarystatistics.tex"))

## 2.Plot of mean hospital uncompensated care from 2013 to 2019 (by ownership type)

full_mean_unc_df <- combined_df %>%
                  group_by(year) %>% 
                    summarize(mean = mean(uncomp_care))

full_mean_unc_graph <- ggplot(full_mean_unc_df, aes(x = year, y = mean))+
                      theme(plot.title = element_text(size=12)) +
                     geom_line(size=1.5) +
                      labs(title = "Average Hospital Uncompensated Care from 2013 to 2019",
                           x = "Year",
                           y = "Million dollars")
ggsave(path = out_dir, filename = "full_mean_unc_graph.png")

#By group
mean_unc_bg_df <- combined_df %>%
                        filter(private == 1) %>% #Get only private data
                        group_by(year, non_profit_private) %>%
                        mutate(non_profit_private = factor(non_profit_private)) %>% 
                        summarize(mean = mean(uncomp_care))

mean_unc_bg_graph <- ggplot(mean_unc_bg_df, aes(x = year, y = mean, col=non_profit_private))+
                                  geom_line(size=1.5) +
                                  theme(plot.title = element_text(size=12)) +
                                  labs(title = "Average Hospital Uncompensated Care from 2013 to 2019",
                                       subtitle = "By organization type, private hospitals only",
                                  x = "Year",
                                  y = "Million dollars")+
  scale_color_discrete(name="Orginization Type",
                       labels=c("Non-profit Private","Profit Private"))
ggsave(path = out_dir, filename = "mean_unc_bg_graph.png")

## Investigation on the effect of Medicaid expansion on hospital uncompensated care
## 3.TWFE estimation 
  ## 3.1 Full sample (treated vs non-treated)
  TWFE_full = feols(uncomp_care ~ expanded_t | provider_number + year, data= combined_df)

  ## 3.2 2014 treatment group v.s. never-treated
  t_tnt_2014.data = combined_df %>%
    filter(year_expand == 2014| is.na(year_expand))
  
  TWFE_2014 = feols(uncomp_care ~ expanded_t | provider_number + year, data= t_tnt_2014.data)

  ## 3.3 2015 treatment group v.s. never-treated
  t_tnt_2015.data = combined_df %>%
    filter(year_expand == 2015| is.na(year_expand))
  
  TWFE_2015 = feols(uncomp_care ~ expanded_t | provider_number + year, data= t_tnt_2015.data)

  ## 3.4 2016 treatment group v.s. never-treated
  t_tnt_2016.data = combined_df %>%
    filter(year_expand == 2016| is.na(year_expand))
  
  TWFE_2016 = feols(uncomp_care ~ expanded_t | provider_number + year, data= t_tnt_2016.data)
  
  ##3.5 Combine results from 3.1 to 3.4 into a table
  msummary(list("Full"=TWFE_full, "2014"=TWFE_2014, "2015"=TWFE_2015, "2016"=TWFE_2016),
           shape=term + statistic ~ model, 
           gof_map=NA,
           coef_omit='Intercept',
           stars = TRUE,
           coef_rename = c("expanded_t" = "Treatment")
  )

## 4.Event study
  ##4.1 Full sample (treated vs non-treated)
  ES.data = combined_df %>%
    mutate(relative_t_expand = year - year_expand) %>%
    mutate(relative_t_expand = coalesce(relative_t_expand, 0))
  #Replaced NA in relative_t_expand to 0 so no losing data in reg, eventually will times 0
  #so replacing is not affecting result
  
  ES_full = feols(uncomp_care~i(relative_t_expand, expanded_ever, ref=-1) | provider_number + year,
                  cluster=~state,
                  data= ES.data)

  ##4.2 2014 treatment group v.s. never-treated
  ES_2014.data = t_tnt_2014.data %>%
    mutate(relative_t_expand = year - year_expand) %>%
    mutate(relative_t_expand = coalesce(relative_t_expand, 0))
  #Replaced NA in relative_t_expand to 0 so no losing data in reg, eventually will times 0
  #so replacing is not affecting result
      
  ES_2014 = feols(uncomp_care~i(relative_t_expand, expanded_ever, ref=-1) | provider_number + year,
                  cluster=~state,
                  data= ES_2014.data)
  
  ##4.3 Combine results from 4.1 to 4.2 into a table
  msummary(list("Full"=ES_full, "2014"=ES_full),
           shape=term + statistic ~ model, 
           gof_map=NA,
           stars = TRUE
  )

## 5.SA Event study
  ##5.1 2014 treatment group v.s. never-treated
  SA_141516.data = ES.data %>%  #Focus on 2014/2015/2016
    mutate(
    year_expand = 
      ifelse(expanded_ever ==0, 10000, year_expand), #Any number will be fine, they are regarded as untreated gp in next line
    relative_t_expand =  ifelse(expanded_ever ==0, -1, relative_t_expand)
    ) #Cohort with only negative `relative_t_expand` are treated as never treated
  
  #By default SA makes `relative_t_expand` = -1 as control group
  SA_141516 = feols(uncomp_care~sunab(year_expand, relative_t_expand) | state + year,
                  cluster=~state,
                  data=SA_141516.data)

  msummary(list("SA_141516"=SA_141516),
           shape=term + statistic ~ model, 
           gof_map=NA,
           stars = TRUE
  )
  
  
## 6. Event study graph on SA event study
  iplot(SA_141516)

## 7. CS estimator
  CS.data = combined_df %>%
    mutate(year_expand=ifelse(is.na(year_expand),0,year_expand)) %>%
    group_by(provider_number) %>%
    mutate(provider_number=cur_group_id()) %>% ungroup()
  
  CS = att_gt(yname="uncomp_care", tname="year", idname="provider_number",
                   gname="year_expand",
                   data=CS.data, panel=TRUE, est_method="dr",
                   allow_unbalanced_panel=TRUE)
  CS_event <- aggte(CS, type="dynamic")

## 8. Rambachan and Roth 
  #Sensitivity plot of the estimated CS ATT
  source("EmpiricalExercise1/code-analysis/honestdid_fn.R")
  M_grid = c(500, 1000, 1500, 2000)
  
  hd_cs_smooth_never <- honest_did(CS_event,
                                   type="smoothness",
                                   Mvec=M_grid)
  
  #Drop 0 as that is not really allowed
  hd_cs_smooth_never$robust_ci <- hd_cs_smooth_never$robust_ci[-1,]
  
  # make sensitivity analysis plots
  cs_HDiD_smooth <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                          hd_cs_smooth_never$orig_ci)
  cs_HDiD_smooth

