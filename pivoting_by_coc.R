library(tidyverse)
library(tidycensus)
#https://github.com/tomhbyrne/HUD-CoC-Geography-Crosswalk

race <- get_decennial(geography = "state", 
                      variables = c("C02003_003", 
                                    "C02003_004", 
                                    "C02003_005", 
                                    "C02003_006", 
                                    "C02003_007", 
                                    "C02003_009"),
                      year = 2020)

race$variable <-
  recode(race$variable,
         C02003_003 = "White",
         C02003_004 = "Black or African American",
         C02003_005 = "American Indian or Alaska Native",
         C02003_006 = "Asian",
         C02003_007 = "Native Hawaiian or Other Pacific Islander",
         C02003_009 = "Multiple Races")

race_pops_per_coc <- 
  coc_to_tract |>
  inner_join(race, by = c("tract_fips" = "GEOID")) |> 
  group_by(coc_number, variable) |>
  summarize(total = sum(estimate))