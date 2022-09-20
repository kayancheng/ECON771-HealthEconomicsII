#' Main analysis for empirical exercise 1
#' Ka Yan CHENG
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

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
  rename(year_expand = year, state = State) %>%
  select(state, expanded, year_expand)
  
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
  
                    drop_na(uncomp_care)

# Main Analysis -------------------------------------------------------------

## 1.Summary statistics on hospital total revenues and uncompensated care

## 2.Plot of mean hospital uncompensated care from 2013 to 2019 (by owership type)

## Investigation on the effect of Medicaid expansion on hospital uncompensated care
## 3.TWFE estimation 
  ## 3.1 Full sample (treated vs non-treated)

  ## 3.2 2014 treatment group v.s. never-treated

  ## 3.3 2015 treatment group v.s. never-treated

  ## 3.4 2016 treatement group v.s. never-treated

## 4.Event study
  ##4.1 Full sample (treated vs non-treated)

  ##4.2 2014 treatment group v.s. never-treated

## 5.SA Event study

## 6. Event study graph on SA event study

## 7. CS estimator

## 8. Rambachan and Roth 

