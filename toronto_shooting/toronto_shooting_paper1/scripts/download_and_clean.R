### Preamble####
# title: download and clean data
# editor: Yimeng Li
# date: format(Sys.time(),%d %B $Y)
# abstract: This is my downloading data and cleaning data


#READ DATA AND CLEAN DATA FROM OPENDATATORONTO


#### Set up workplace ####
#install.packages("opendatatoronto")
#install.packages("lubridate")
#install.packages("knitr")


library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(vctrs)
library(dplyr, warn.conflicts = FALSE)


#### Acquire ####
# Get the raw data and generate table#
toronto_shootings <-
  # Each package is associated with a unique id  found in the "For 
  # Developers" tab of the relevant page from Open Data Toronto
  # https://open.toronto.ca/dataset/shootings-firearm-discharges/
  list_package_resources("4bc5511d-0ecf-487a-9214-7b2359ad8f61") |>
  # Within that package, we are interested in the shootings-firearm-discharges file.
  filter(name == 
           "shootings-firearm-discharges") |>
  # Having reduced the dataset to one row we can get the resource
  get_resource()


# Generate raw data table#
write_csv(toronto_shootings, "inputs/toronto_shootings.csv")


#### Clean data ####
# Take only the rows we want #
cleaned_toronto_shootings <-
  clean_names(toronto_shootings) |>
  #focus on within 10 years' data
  filter(occurrence_year %in% c("2012","2013","2014","2015","2016","2017", 
                                "2018","2019","2020","2021"))|>
  select(occurrence_year,time_range,death,injuries,neighbourhood) |>
  # If NA occurs in both death and injuries, counted as a false alarm. Then, delete it.
  filter(!vec_equal_na(across(c(death,injuries)))) |>
  # rename the column
  rename(
    year = occurrence_year,
    time = time_range)

head(cleaned_toronto_shootings)

# Replace NA to 0
cleaned_toronto_shootings$injuries [is.na(cleaned_toronto_shootings$injuries )] <- 0
cleaned_toronto_shootings$death [is.na(cleaned_toronto_shootings$death )] <- 0

# Add a new column to categorize whether the shooting caused injuries or death
cleaned_toronto_shootings <-
  cleaned_toronto_shootings |>
  # both represents injuries and death at same time, otherwise, shows "other"
  mutate(injury_level = if_else(injuries > 0 & death > 0, "both", "other"))

#condition 1- only death
cleaned_toronto_shootings$injury_level [
  cleaned_toronto_shootings$injury_level == "other" &
    cleaned_toronto_shootings$death >0 ] <- "death"

#condition 2- only injuries
cleaned_toronto_shootings$injury_level [
  cleaned_toronto_shootings$injury_level == "other" &
    cleaned_toronto_shootings$injuries >0 ] <- "injuries"


# Save data#
write_csv(cleaned_toronto_shootings, "inputs/cleaned_toronto_shooting.csv")




