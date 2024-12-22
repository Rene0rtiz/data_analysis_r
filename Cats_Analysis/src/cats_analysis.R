# install packages
required_packages <- c("dplyr", "ggeasy", "ggplot2", "ggrepel", "janitor", "lubridate", "skimr", "tidyverse", "waffle", "packrat")

install.packages("packrat")

# load packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# change working directory
# import dataset
cats_df <- read_csv("cats_dataset.csv")

# verify data
glimpse(cats_df)
nrow(cats_df)

# Clean Data ##
# clean column names#
cats_df <- clean_names(cats_df)
glimpse(cats_df)
# checks: duplicates, N/A
sum(cats_df[duplicated(cats_df), ])
sum(is.na(cats_df))

# remove unnecessary
# Add column for weights in lbs
cats_df <- cats_df %>%
  mutate(weight_lb = weight_kg * 2.205)


## export clean dataset ##
# write_csv(dataframe, "~/Path/to/directory/filename.csv)


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

# overall cat weights
n_distinct(cats_df$weight_kg)
cats_df %>%
  distinct(weight_kg, weight_lb) %>%
  summarise(
    min_weight = min(weight_kg),
    min_weight_lb2 = min(weight_lb),
    max_weight = max(weight_kg),
    max_weight_lb = max(weight_lb),
    avg_weight_kg = mean(weight_kg),
    avg_weight_lb = mean(weight_lb)
  )

weights_by_breed <- cats_df %>%
  group_by(breed) %>%
  summarise(
    min_weight_kg = min(weight_kg),
    min_weight_lb = min(weight_lb),
    max_weight_kg = max(weight_kg),
    max_weight_lb = max(weight_lb),
    avg_weight_kg = mean(weight_kg),
    avg_weight_lb = mean(weight_lb),
    med_weight_kg = median(weight_kg),
    med_weight_lb = median(weight_lb)
  ) %>%
  print(n = 30)


n_distinct(cats_df$color)
cats_df %>%
  arrange(color) %>%
  group_by(color) %>%
  count()

n_distinct(cats_df$gender)

## Analysis ##

# summary
summary(cats_df[c("age_years", "weight_kg", "gender")])

breed_gender_breakdown <- cats_df %>%
  group_by(breed, gender) %>%
  count()

# visualizations
# Bar plot
breed_gender_plot <-
  ggplot(breed_gender_breakdown, aes(x = gender, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~ breed)
# Plot text and formatting
breed_gender_plot +
  geom_text(aes(label = n), vjust = 1.15) +
  ggtitle("Cat Breed Gender Distribution") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  ylab("Count") +
  xlab("Gender")

# weight boxplot in kg
weight_plot_kg <- ggplot(cats_df, aes(x = gender, y = weight_kg, fill = gender)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  scale_fill_tron() +
  coord_flip() +
  facet_wrap(~ breed)
# Plot text and formatting
weight_plot_kg +
  ggtitle("Weight By Breed") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_blank()) +
  ylab("Weights (in kg)")

# weight boxplot in lb
weight_plot_lb <- ggplot(cats_df, aes(x = gender, y = weight_lb, fill = gender)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  scale_fill_tron() +
  coord_flip() +
  facet_wrap(~ breed)
# Plot text and formatting
weight_plot_lb +
  ggtitle("Weight By Breed") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_blank()) +
  ylab("Weights (in lb)")

# life longevity by breed
breed_ages <- cats_df %>%
  group_by(breed) %>%
  summarise(
    min_age = (min(age_years)),
    max_age = (max(age_years)),
    med_age = (median(age_years)),
    avg_age = (mean(age_years))
  ) %>%
  print(n = 30)

# ages
age_plot <- ggplot(cats_df, aes(x = gender, y = age_years, colour = breed)) +
  geom_point()
# show entire data plot
age_plot +
  geom_jitter() +
  coord_flip()

# Breed facets
