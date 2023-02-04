
#### data ####
test_cleaned_data <- 
  read_csv("/Users/myrali/Desktop/toronto_shooting/toronto_shooting_paper1/inputs/cleaned_toronto_shooting.csv", 
           show_col_types = FALSE)
head(test_cleaned_data)


#The shooting_level is exclusively one of these three: “injuries”, “death”, or "both".

#The year is between 2012 and 2021
test_cleaned_data$year |>
  min() == 2012
test_cleaned_data$year |>
  max() == 2021

#The shooting_level is exclusively one of these three: “injuries”, “death”, or "both".
test_cleaned_data$injury_level |>
  unique() == c(
    "death",
    "injuries",
    "both"
  )

#The time is exclusively one of these four: “Night”, “Evening”,"Morning", or "Afternoon".
test_cleaned_data$time |>
  unique() == c(
    "Night",
    "Evening",
    "Afternoon",
    "Morning"
  )

#The injuries is greater or equal to zero for testing cleaning NA
test_cleaned_data$injuries |>
  min() == 0


#The death is greater or equal to zero for testing cleaning NA
test_cleaned_data$death |>
  min() == 0

