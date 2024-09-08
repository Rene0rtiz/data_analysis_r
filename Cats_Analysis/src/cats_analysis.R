# install packages
install.packages("dplyr")
install.packages("ggeasy")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("tidyverse")
install.packages("ggsci")

# load packages
library(dplyr)
library(ggeasy)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(janitor)
library(skimr)
library(tidyverse)
library(ggsci)

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
## export clean dataset ##
# write_csv(dataframe, "~/Path/to/directory/filename.csv)


## Analysis ##

# summary
summary(cats_df[c("age_years", "weight_kg","gender")])

breed_gender_breakdown <- cats_df %>%
  group_by(breed, gender) %>%
  count()

# visualizations
# Bar plot
breed_gender_plot <- ggplot(breed_gender_breakdown, aes( x = gender, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~breed)
# Plot text and formatting
breed_gender_plot +
  geom_text(aes(label = n), vjust = 1.15) +
  ggtitle("Cat Breed Gender Distribution") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(), axis.ticks = element_blank()) +
  ylab("Count") +
  xlab("Gender")

#weight_plot <- 
ggplot(cats_df, aes(x = gender, y = weight_kg, fill = gender)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_tron() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.35) +
  coord_flip() +
  facet_wrap(~breed)
  
