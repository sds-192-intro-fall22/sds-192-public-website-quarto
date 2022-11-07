library(tidyverse)
library(tidycensus)
library(readxl)

pit_url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-State.xlsx"
temp <- tempfile()
download.file(pit_url, temp)

get_pit <- function(pit_year){
  print(paste("getting", pit_year, "data"))
  read_excel(temp, sheet = pit_year) |> 
    select(State, starts_with("Sheltered Total Homeless -"), starts_with("Unsheltered Homeless -")) |>
    select(-contains("Age 18"), -contains("Under 18"), -contains("24"), -contains("Hispanic"))
}

pit <- map(c("2015", "2016", "2017", "2018", "2019", "2020"), get_pit) |>
  reduce(left_join, by = "State")

pit_colnames <- data.frame(pit_names = colnames(pit))

get_race <- function(cen_year){
  get_acs(geography = "state", 
          variables = c("C02003_003", 
                        "C02003_004", 
                        "C02003_005", 
                        "C02003_006", 
                        "C02003_007", 
                        "C02003_009"),
          year = cen_year) |>
    rename_with(.fn = ~paste0(., "_", cen_year), .cols = estimate:moe)
}

get_gender <- function(cen_year){
  get_acs(geography = "state", 
          variables = c("B01001_026", 
                        "B01001_002"),
          year = cen_year) |>
    rename_with(.fn = ~paste0(., "_", cen_year), .cols = estimate:moe)
}

race <- map(c(2015:2020), get_race) |> 
  reduce(left_join, by = c("GEOID", "NAME", "variable"))
gender <- map(c(2015:2020), get_gender) |> 
  reduce(left_join, by = c("GEOID", "NAME", "variable"))

race$variable <-
  recode(race$variable,
         C02003_003 = "White",
         C02003_004 = "Black or African American",
         C02003_005 = "American Indian or Alaska Native",
         C02003_006 = "Asian",
         C02003_007 = "Native Hawaiian or Other Pacific Islander",
         C02003_009 = "Multiple Races")

gender$variable <-
  recode(gender$variable,
         B01001_026 = "Female",
         B01001_002 = "Male")

write_csv(pit, "website/data/pit_2015_2020.csv")
write_csv(gender, "website/data/gender_state_2015_2020.csv")
write_csv(race, "website/data/race_state_2015_2020.csv")