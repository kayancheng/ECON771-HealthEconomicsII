#' Data Preparation for Empirical Exercise 2
#' Ka Yan CHENG
#' 
#' This script helps to merge related information from 3 sources:
#' 1. MDPPAS
#' 2. utilization-payment-puf
#' 3. PFS_update_data.txt
#' for our analysis

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table)

# Importing and Cleaning up individual dataset ----------------------------

##1. MDPPAS for physician integration
in_data.path.mdppas <- "EmpiricalExercise2/data/data-in/MDPPAS"

for (i in 2012:2017){
  assign( paste0("MDPPAS", i, "_df"),
          read.csv(paste0(in_data.path.mdppas, paste0("/PhysicianData_",i,".csv")),
                     header = TRUE)[c("npi", "Year", "pos_opd", "pos_office", "pos_asc", "group1")])
  
  MDPPAS_temp = get(paste0("MDPPAS", i, "_df"))
  
  if (i == 2012){
    MDPPAS = MDPPAS_temp
  } else{
    MDPPAS = bind_rows(MDPPAS, MDPPAS_temp)
  }
  
  rm(MDPPAS_temp)
  rm(list = paste0("MDPPAS", i, "_df"))
  
}

##2. utilization-payment-puf
in_data.path.puf <- "EmpiricalExercise2/data/data-in/utilization-payment-puf"

rel_columns = c("NPI", "NPPES_CREDENTIALS",
               "LINE_SRVC_CNT",
               "AVERAGE_MEDICARE_ALLOWED_AMT",
               "BENE_UNIQUE_CNT")

for (i in 2012:2017){
  
  dir = paste0(in_data.path.puf, "/", i, "/")
  df = list.files(path=dir, pattern="*.csv|*.txt")
  
  if (i<=2013|i==2016){
    assign(paste0("PUF", i, "_df"),
           fread(paste0(dir,df), select = rel_columns))
    
    PUF_temp = get(paste0("PUF", i, "_df")) %>%
      filter(NPPES_CREDENTIALS == "M.D."|NPPES_CREDENTIALS == "MD") %>%
      mutate("YEAR" = i) %>%
      select(-c("NPPES_CREDENTIALS"))
    
  } else {
    
    assign(paste0("PUF", i, "_df"),
           fread(paste0(dir,df), select = append(tolower(rel_columns[rel_columns!="AVERAGE_MEDICARE_ALLOWED_AMT"]),
                                                 "average_Medicare_allowed_amt")))
    
    PUF_temp = get(paste0("PUF", i, "_df")) %>%
      filter(nppes_credentials == "M.D.") %>%
      mutate("Year" = i) %>%
      select(-c("nppes_credentials"))
    
    colnames(PUF_temp) = toupper(colnames(PUF_temp))
    
  }
               
  
  if (i == 2012){
    PUF = PUF_temp[-1,]
  } else{
    PUF = bind_rows(PUF, PUF_temp[-1,])
  }
  
  rm(PUF_temp)
  rm(list = paste0("PUF", i, "_df"))
         
}

rm(list = c("df", "dir", "i", "in_data.path.mdppas", "in_data.path.puf", "rel_columns"))

# Save workspace ----------------------------------------------------------
save.image (file = "EmpiricalExercise2/data/data-out/emp_ex2_df.RData")

