library(dplyr)
library(ggeasy)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(janitor)
library(skimr)
library(tidyverse)

stream_activity_df <- read_csv("my_streaming_activity.csv")

glimpse(stream_activity_df)
n_distinct(stream_activity_df$SongID)

sum(stream_activity_df[duplicated(stream_activity_df),])

sum(is.na(stream_activity_df))
