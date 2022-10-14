#' IV Construction for Empirical Exercise 2
#' Ka Yan CHENG
#' 
#' This script Constructions the price shock IV by using info from 3 sources:
#' 1. MDPPAS
#' 2. utilization-payment-puf
#' 3. PFS_update_data.txt
#' 

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table)

# Importing individual dataset ----------------------------------------------
#1. taxid.base
in_data.path.mdppas <- "EmpiricalExercise2/data/data-in/MDPPAS"
taxid.base = read.csv(paste0(in_data.path.mdppas, 
                             "/PhysicianData_2009.csv"),
                      header = TRUE)[c("npi", "group1")]

#2. medicare.puf
in_data.path.puf <- "EmpiricalExercise2/data/data-in/utilization-payment-puf"

rel_columns = c("NPI",
                "LINE_SRVC_CNT",
                "HCPCS_CODE",
                "NPPES_CREDENTIALS")

for (i in 2012:2017){
  
  dir = paste0(in_data.path.puf, "/", i, "/")
  df = list.files(path=dir, pattern="*.csv|*.txt")
  
  if (i<=2013|i==2016){
    assign(paste0("medicare.puf", i, "_df"),
           fread(paste0(dir,df), select = rel_columns))
    
    PUF_temp = get(paste0("medicare.puf", i, "_df")) %>%
      filter(NPPES_CREDENTIALS == "M.D."|NPPES_CREDENTIALS == "MD") %>%
      mutate("YEAR" = i) %>%
      select(-c("NPPES_CREDENTIALS"))
    
  } else {
    
    assign(paste0("medicare.puf", i, "_df"),
           fread(paste0(dir,df), select = tolower(rel_columns)))
    
    PUF_temp = get(paste0("medicare.puf", i, "_df")) %>%
      filter(nppes_credentials == "M.D.") %>%
      mutate("Year" = i) %>%
      select(-c("nppes_credentials"))
    
    colnames(PUF_temp) = toupper(colnames(PUF_temp))
    
  }
  
  
  if (i == 2012){
    medicare.puf = PUF_temp[-1,]
  } else{
    medicare.puf = bind_rows(medicare.puf, PUF_temp[-1,])
  }
  
  rm(PUF_temp)
  rm(list = paste0("medicare.puf", i, "_df"))
}

colnames(medicare.puf) = tolower(colnames(medicare.puf))
medicare.puf = medicare.puf %>%
  mutate (line_srvc_cnt = log(line_srvc_cnt))
#3. pfs.yearly
in_data.path.pfs <- "EmpiricalExercise2/data/data-in/PFS_update_data.txt"

pfs =  read.table(in_data.path.pfs,
                  header = TRUE, fill = TRUE,
                  check.names = FALSE,
                  sep = '\t', quote = "")

# Constructing the price.shock IV using given function ----------------------

for (i in 2012:2017){
  
  medicare.puf.yearly = medicare.puf %>%
    filter(year == i)
  
  if(i <=2013){
    pfs.yearly = pfs %>%
      filter(year == i)
  } else {
    pfs.yearly = pfs %>%
      filter(year == 2013)
    pfs.yearly$year = i
  }
  
  
  price.shock <- medicare.puf.yearly %>% inner_join(taxid.base, by="npi") %>%
  inner_join(pfs.yearly %>% 
               select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), 
             by=c("hcpcs_code"="hcpcs")) %>%
  mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
  mutate(price_shock = case_when(
    i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
    i>2013  ~ dprice_rel_2010),
    denom = line_srvc_cnt*price_nonfac_orig_2010,
    numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
  group_by(npi) %>%
  summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(group1)) %>%
  ungroup() %>%
  mutate(phy_rev_change=phy_numer/phy_denom) %>%    
  group_by(tax_id) %>%
  summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
  ungroup()
  
  price.shock$year = i
  
  if(i == 2012){
    iv = price.shock
  } else {
    iv = bind_rows(price.shock, iv)
  }
}


rm(list = c("df", "dir", "i", "in_data.path.mdppas", "in_data.path.puf", "rel_columns",
            "medicare.puf", "medicare.puf.yearly", "pfs", "pfs.yearly", "price.shock",
            "taxid.base",  "in_data.path.pfs"))

# Save workspace ----------------------------------------------------------
save.image (file = "EmpiricalExercise2/data/data-out/emp_ex2_iv.RData")

