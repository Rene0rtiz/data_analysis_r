# install packages
install.packages("dplyr")
install.packages("ggeasy")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("tidyverse")

# load packages
library(dplyr)
library(ggeasy)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(janitor)
library(skimr)
library(tidyverse)

# change working directory
# import dataset
cats_df <- read_csv("cats_dataset.csv")

# verify data
glimpse(cats_df)
nrow(cats_df)
n_distinct(cats_df$Breed)
n_distinct(cats_df$`Age (Years)`)
n_distinct(cats_df$`Weight (kg)`)
n_distinct(cats_df$Color)
n_distinct(cats_df$Gender)

# checks: duplicates, N/A


## Clean Data ##

# remove unnecessary

# clean column names

# change formats

# export clean dataset

## Analysis ##

# summary

# visualizations