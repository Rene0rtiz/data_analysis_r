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

# Clean Data ##
# clean column names#
cats_df <- clean_names(cats_df)
# checks: duplicates, N/A
sum(cats_df[duplicated(cats_df),])
sum(is.na(cats_df))


n_distinct(cats_df$breed)
#unique(cats_df$breed)  #Does not support piping
cats_df %>%
  group_by(breed) %>%
  arrange(breed) %>%
  count() %>%
  print(n = 30)
  
n_distinct(cats_df$age_years)
cats_df %>%
  distinct(age_years) %>%
  summarise(
    min_age = min(age_years),
    max_age = max(age_years),
    avg_age = mean(age_years)
  )
  
n_distinct(cats_df$weight_kg)
cats_df %>%
  distinct(weight_kg) %>%
  summarise(
    min_weight = min(weight_kg),
    max_weight = max(weight_kg),
    avg_weight = mean(weight_kg)
  )

n_distinct(cats_df$color)
cats_df %>%
  arrange(color) %>%
  group_by(color) %>%
  count()

n_distinct(cats_df$gender)






# remove unnecessary



# change formats

# export clean dataset

## Analysis ##

# summary

# visualizations