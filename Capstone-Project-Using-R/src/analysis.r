## install packages required for analysis
# create a list of packages to install for script
required_packages <- c("dplyr","ggeasy","ggplot2", "ggrepel","here", "janitor", "lubridate","skimr",  "tidyverse", "waffle")
# create a vector of missing packages that need to be installed
# !(required_packages %in% installed.packages()[,"Package"]): checks to see which are not already installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
# length(new_packages): checks how many packages are missing, if > 0, then install missing packages
if (length(new_packages))
  install.packages(new_packages)

# ## load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# data paths
raw_data_dir <- here("data", "raw", "Fitabase_Data_4.12.16-5.12.16")
processed_data_dir <- here("data", "processed")

# import data sets
daily_activity <- read_csv(file.path(raw_data_dir, "dailyActivity_merged.csv"))
daily_steps <- read_csv(file.path(raw_data_dir, "dailySteps_merged.csv"))
hourly_calories <- read_csv(file.path(raw_data_dir, "hourlyCalories_merged.csv"))
hourly_intensities <- read_csv(file.path(raw_data_dir, "hourlyIntensities_merged.csv"))
hourly_steps <- read_csv(file.path(raw_data_dir, "hourlySteps_merged.csv"))

# create a list of datasets to apply functions to verify
datasets <- list(
  daily_activity = daily_activity,
  daily_steps = daily_steps,
  hourly_calories = hourly_calories,
  hourly_intensities = hourly_intensities,
  hourly_steps = hourly_steps
)

# verify data and check for duplicates
# create function for requently used calls
inspect_datasets <- function(data) {
  if (is.data.frame(data)) {
    # Handle a single data frame
    cat("\n--- Inspecting single dataset ---\n")
    glimpse(data)
    cat("Unique IDs:     ", n_distinct(data$Id), "\n")
    cat("Rows:           ", nrow(data), "\n")
    cat("Duplicates:     ", sum(duplicated(data)), "\n")
    cat("Missing Values: ", sum(is.na(data)), "\n")
  } else if (is.list(data) && all(sapply(data, is.data.frame))) {
    # Handle a named list of data frames
    for (name in names(data)) {
      cat("\n---", name, "---\n")
      glimpse(data[[name]])
      cat("Unique IDs:     ", n_distinct(data[[name]]$Id), "\n")
      cat("Rows:           ", nrow(data[[name]]), "\n")
      cat("Duplicates:     ", sum(duplicated(data[[name]])), "\n")
      cat("Missing Values: ", sum(is.na(data[[name]])), "\n")
    }
  } else {
    stop("Input must be a data frame or a named list of data frames.")
  }
}

inspect_datasets(datasets)

## Merge all hourly into one dataset
hourly_data_df <- hourly_steps %>%
  left_join(hourly_calories, by = c("Id", "ActivityHour")) %>%
  left_join(hourly_intensities, by = c("Id", "ActivityHour"))

inspect_datasets(hourly_data_df)

## Remove daily_steps and individual hourly data from environment
rm(daily_steps,
   hourly_calories,
   hourly_intensities,
   hourly_steps,
   datasets)

## change camel case column names
daily_activity <- clean_names(daily_activity)
hourly_data_df <- clean_names(hourly_data_df)

# Check date format, sample check
unique(substr(daily_activity$activity_date, 1, 10))

## Change datetime format
daily_activity$activity_date <-
  as.POSIXct(daily_activity$activity_date,
             format = "%m/%d/%Y",
             tz = Sys.timezone())

#Sys.timezone() varies by user system
hourly_data_df$activity_hour <-
  as.POSIXct(hourly_data_df$activity_hour,
             format = "%m/%d/%Y %I:%M:%S %p",
             tz = Sys.timezone())

daily_activity$activity_date <-
  as.Date(daily_activity$activity_date, format = "%Y-%m-%d")

## separate datetime column into two

hourly_data_df$date <- as.Date(hourly_data_df$activity_hour)

hourly_data_df$time <-
  format(as.POSIXct(hourly_data_df$activity_hour), format = "%H:%M:%S")

## verify format matches
class(daily_activity$activity_date)
class(hourly_data_df$date)

## add a colum for day of week
daily_activity$day_of_week <-
  wday(daily_activity$activity_date, label = TRUE)


hourly_data_df$day_of_week <-
  wday(hourly_data_df$date, label = TRUE)

glimpse(daily_activity)
glimpse(hourly_data_df)

## export hourly data to csv
write_csv(hourly_data_df, here(file.path(processed_data_dir),"hourly_data.csv"))

write_csv(daily_activity, here(file.path(processed_data_dir), "daily_activity.csv"))

## quick summary of statistics
summary(daily_activity)
summary(hourly_data_df)

## average by day of week
steps_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = round(mean(total_steps)))

## look at average steps in bar graph

ggplot(daily_activity, aes(x = day_of_week, y = total_steps)) +
  geom_bar(stat = "summary",
           fun = "mean",
           fill = "#00abff") +
  labs(title = "Average Steps By Day Of Week", x = "Day of Week", y = "Average Steps") +
  ggeasy::easy_center_title()

## look at correlation between steps and calories
ggplot(data = daily_activity) +
  geom_point(mapping = aes(x = total_steps, y = calories)) +
  geom_smooth(mapping = aes(x = total_steps, y = calories)) +
  labs(title = "Correlation Between Total Steps and Calories Burned", x = "Total Steps", y = "Calories Burned")

## Get correlation coefficient
cor(daily_activity$total_steps, daily_activity$calories)

## distance and calories
ggplot(data = daily_activity) +
  geom_point(mapping = aes(x = total_distance, y = calories)) +
  geom_smooth(mapping = aes(x = total_distance, y = calories)) +
  labs(title = "Correlation Between Total Distance and Calories Burned", x = "Distance(km)", y = "Calories Burned")

## Get correlation coefficient
cor(daily_activity$total_distance, daily_activity$calories)

## Device usage

## create a usage breakdown
dau_breakdown <- daily_activity %>%
  filter(total_steps > 500) %>%
  group_by(id) %>%
  summarize(activity_date = sum(n())) %>%
  mutate(
    device_usage = case_when(
      activity_date >= 1 & activity_date <= 14 ~ "Low Use - 1 to 14 days",
      activity_date >= 15 &
        activity_date <= 22 ~ "Moderate Use - 15 to 22 days",
      activity_date >= 23 &
        activity_date <= 31 ~ "High Use - 23 to 31 days"
    )
  ) %>%
  mutate(device_usage = factor(
    device_usage,
    level = c(
      "Low Use - 1 to 14 days",
      "Moderate Use - 15 to 22 days",
      "High Use - 23 to 31 days"
    )
  )) %>%
  rename(daysused = activity_date) %>%
  group_by(device_usage)

# summary
dau_use <- daily_activity %>%
  left_join(dau_breakdown, by = "id") %>%
  group_by(device_usage) %>%
  summarise(participants = n_distinct(id)) %>%
  mutate(perc = participants / sum(participants)) %>%
  arrange(perc) %>%
  mutate(perc = scales::percent(perc))
head(dau_use)

waffle(
  dau_use,
  rows = 3,
  size = 0.33,
  colors = c("#D55E00", "#0072B2", "#009E73"),
  legend_pos = "bottom"
) +
  labs(title = "Device Usage", subtitle = "1 square = 1 Participant", ) +
  ggeasy::easy_center_title()

# get totals from daily activity for each type
very_active_min <- sum(daily_activity$very_active_minutes)
fairly_active_min <- sum(daily_activity$fairly_active_minutes)
lightly_active_min <- sum(daily_activity$lightly_active_minutes)
sedentary_min <- sum(daily_activity$sedentary_minutes)

# create chart slices and labels
activity_vec <- c(very_active_min,
                  fairly_active_min,
                  lightly_active_min,
                  sedentary_min)

label_vec <-
  c("Very Active", "Fairly Active", "Lightly Active", "Sedentary")
piepercent <- round(100 * activity_vec / sum(activity_vec), 1)

pie_data <- data.frame(activity_vec, label_vec, piepercent)

## get positions
pie_pos <- pie_data %>%
  mutate(
    csum = rev(cumsum(rev(activity_vec))),
    pos = activity_vec / 2 + lead(csum, 1),
    pos = if_else(is.na(pos), activity_vec / 2, pos)
  )

## display chart

ggplot(pie_data, aes(
  x = "",
  y = activity_vec,
  fill = fct_inorder(label_vec)
)) +
  ggtitle("Percentage of Activity in Minutes") +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  geom_label_repel(
    data = pie_pos,
    aes(y = pos, label = paste0(piepercent, "%")),
    size = 4.5,
    nudge_x = 1,
    show.legend = FALSE
  ) +
  guides(fill = guide_legend(title = "Activity Type")) +
  theme_void() +
  ggeasy::easy_center_title()