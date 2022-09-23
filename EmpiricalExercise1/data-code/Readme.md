## Content description

This folder contains code used for getting raw data of the analysis, which includes:

1. ``unzip.R`` for unzipping data from HCRIS and Provider of Services files
    - The unzipped files goes to folder data/data-out/unzip_raw
    
  
2. Code from Prof. Ian McCarthy on organizing data from 3 different sources
  - Hospital Cost Report Information System (HCRIS) - [Original Repo](https://github.com/imccart/HCRIS)
  - Provider of Services files - [Original Repo](https://github.com/imccart/cms-pos)
  - Medicaid Expansion - [Original Repo](https://github.com/imccart/Insurance-Access)
    - The output files goes to folder data/data-out
    
## Instructions

Please run the following code in R following the specific sequence to prepare the data:

1. ``unzip.R``
2. Code from Prof. Ian McCarthy
    - HCRIS-code-from-ian/``_HCRIS_Data.R``
    - Insurance-access-code-from-ian/``_BuildFinalData.R``
    - pos-code-from-ian/``data-combined.R``
