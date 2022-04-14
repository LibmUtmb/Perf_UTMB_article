# UTMB_dataMining

Stucture of the repo

1. Original data 
	- Raw data -csv files- from de UTMB database 

2. Prepared data 
	- all csv with raw datasets are named "RAW_tablename.csv" (it correspond to a brut extract of the DB)
	- all R script named 'YYYYMMMDD_RAW_WRK_tablename.R" correspond to
		 cleaning script -deleting unuse variables, encoding others, naming, checking for missing data and outliers-
	- all cleaning script have at least one output named "WRK_tablename.csv" which is the clean version of datasets

3. Analysis
	- contain all working script to analyse data (check the title block on each script for more information

4. Insights 
	- Analyses insights script (mostly Rmarkdown or presentation, which are note push in the repo)