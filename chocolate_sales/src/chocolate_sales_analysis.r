#install required packages
required_packages <- c("dplyr", "ggeasy", "ggplot2", "ggrepel", "janitor","lubridate", "skimr", "tidyverse", "waffle", "packrat")

pkgs <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs)

#load installed packages
lapply(pkgs, library, character.only=TRUE)

#import raw data
sales_df <-read_csv("Chocolate_Sales.csv")

#verify data
glimpse(sales_df)
nrow(sales_df)

## Clean Data ##
#clean column names 
sales_df <- clean_names(sales_df)
glimpse(sales_df)

# check for duplicates and N/A
sum(sales_df[duplicated(sales_df),])
sum(is.na(sales_df))
 