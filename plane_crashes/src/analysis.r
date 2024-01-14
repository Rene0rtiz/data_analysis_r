library(chron)
library(dplyr)
library(ggeasy)
library(ggplot2)
library(ggrepel)
library(hms)
library(lubridate)
library(janitor)
library(skimr)
library(tidyverse)

crashes_fatalities <-
  read_csv("Airplane_Crashes_and_Fatalities_Since_1908_to_2023.csv")
glimpse(crashes_fatalities)

crashes_fatalities <-
  crashes_fatalities %>%
  rename("Aboard Passengers" = "Aboard Passangers",
         "Fatalities Passengers" = "Fatalities Passangers")
glimpse(crashes_fatalities)

crashes_fatalities <-
  clean_names(crashes_fatalities)
glimpse(crashes_fatalities)

#convert date from chr to date
crashes_fatalities$date <-
  mdy(crashes_fatalities$date)
glimpse(crashes_fatalities)

#convert time from chr to time
crashes_fatalities$new_time <-
  times(paste0(crashes_fatalities$time, ":00"))

#move column position
crashes_fatalities <- crashes_fatalities %>%
  relocate(new_time, .after = time)

#write_csv(crashes_fatalities, "~/Code/github/plane_crashes/data/processed/c&f.csv")
#convert Aboard, Fatalities, Fatalities Passengers, Fatalities Crew, Ground to int
crashes_fatalities$new_aboard <-
  as.integer(crashes_fatalities$aboard)
crashes_fatalities <-
  crashes_fatalities %>%
  relocate(new_aboard,
           .after = aboard)

crashes_fatalities$new_fatalities <-
  as.integer(crashes_fatalities$fatalities)
crashes_fatalities <-
  crashes_fatalities %>%
  relocate(new_fatalities,
           .after = fatalities)

crashes_fatalities$new_fatalities_passengers <-
  as.integer(crashes_fatalities$fatalities_passengers)
crashes_fatalities <-
  crashes_fatalities %>%
  relocate(new_fatalities_passengers,
           .after = fatalities_passengers)

crashes_fatalities$new_fatalities_crew <-
  as.integer(crashes_fatalities$fatalities_crew)
crashes_fatalities <-
  crashes_fatalities %>%
  relocate(new_fatalities_crew,
           .after = fatalities_crew)

crashes_fatalities$new_ground <-
  as.integer(crashes_fatalities$ground)
crashes_fatalities <-
  crashes_fatalities %>%
  relocate(new_ground,
           .after = ground)

glimpse(crashes_fatalities)
