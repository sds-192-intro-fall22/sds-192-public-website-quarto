# Load Packages

library(googlesheets4)
library(googledrive)
library(tidyverse)

# Google Creds

google_app <- httr::oauth_app(
  "Survey",
  key = Sys.getenv("GOOGLE_CLIENT_ID"),
  secret = Sys.getenv("GOOGLE_SECRET")
)
drive_auth_configure(app = google_app)


# Import Survey Data

survey1 <- read_sheet("1pSOCQWXDn_Q4mLL4UyzXKx3Vs6plhXPbfQIcrwIVGsI", 
                      .name_repair = make.names)
survey2 <- read_sheet("1cFzlO4pqzhZA8RQbZuAReJYYcJmkPmOl5IzSCQ4WM1Y", 
                      .name_repair = make.names)


# Join Data

joined_mosaic_surveys <-
  survey1 %>%
  left_join(survey2, 
            by = c("Neighborhood.at.Smith", "Year.Started.at.Smith", "Birth.Month")) %>%
  select(Full.Name, Favorite.Color, Favorite.Movie)

# Calculate Percent ID'd

survey1_rows <- nrow(survey1)

joined_mosaic_surveys %>%
  group_by(Full.Name) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  filter(Count == 1) %>% 
  nrow() / survey1_rows
