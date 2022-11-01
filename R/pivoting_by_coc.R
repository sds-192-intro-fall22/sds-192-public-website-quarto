library(tidyverse)
library(tidycensus)
library(sf)
library(readxl)
#https://github.com/tomhbyrne/HUD-CoC-Geography-Crosswalk

pit_url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-CoC.xlsx"
temp <- tempfile()
download.file(pit_url, temp)


get_pit <- function(pit_year){
  read_excel(temp, sheet = pit_year) |> 
    select(1:3, starts_with("Sheltered Total Homeless -"), starts_with("Unsheltered Homeless -")) |>
    select(-contains("18"), -contains("24"), -contains("Hispanic"), -contains("Female"), -contains("Male"), -contains("ender"))
}

pit <- map(c("2017"), get_pit) |>
  reduce(left_join, by = `CoC Number`)

ca_coc_500 <- st_read("R/data/California/CA_500/CA_500.shp")
ca_coc_501 <- st_read("R/data/California/CA_501/CA_501.shp")
ca_coc_502 <- st_read("R/data/California/CA_502/CA_502.shp")
ca_coc_503 <- st_read("R/data/California/CA_503/CA_503.shp")
ca_coc_504 <- st_read("R/data/California/CA_504/CA_504.shp")
ca_coc_505 <- st_read("R/data/California/CA_505/CA_505.shp")
ca_coc_506 <- st_read("R/data/California/CA_506/CA_506.shp")
ca_coc_507 <- st_read("R/data/California/CA_507/CA_507.shp")
ca_coc_508 <- st_read("R/data/California/CA_508/CA_508.shp")
ca_coc_509 <- st_read("R/data/California/CA_509/CA_509.shp")
ca_coc_510 <- st_read("R/data/California/CA_510/CA_510.shp")
ca_coc_511 <- st_read("R/data/California/CA_511/CA_511.shp")
ca_coc_512 <- st_read("R/data/California/CA_512/CA_512.shp")
ca_coc_513 <- st_read("R/data/California/CA_513/CA_513.shp")
ca_coc_514 <- st_read("R/data/California/CA_514/CA_514.shp")
ca_coc_515 <- st_read("R/data/California/CA_515/CA_515.shp")
ca_coc_516 <- st_read("R/data/California/CA_516/CA_516.shp")
ca_coc_517 <- st_read("R/data/California/CA_517/CA_517.shp")
ca_coc_518 <- st_read("R/data/California/CA_518/CA_518.shp")
ca_coc_519 <- st_read("R/data/California/CA_519/CA_519.shp")
ca_coc_520 <- st_read("R/data/California/CA_520/CA_520.shp")
ca_coc_521 <- st_read("R/data/California/CA_521/CA_521.shp")
ca_coc_522 <- st_read("R/data/California/CA_522/CA_522.shp")
ca_coc_523 <- st_read("R/data/California/CA_523/CA_523.shp")
ca_coc_524 <- st_read("R/data/California/CA_524/CA_524.shp")
ca_coc_525 <- st_read("R/data/California/CA_525/CA_525.shp")
ca_coc_526 <- st_read("R/data/California/CA_526/CA_526.shp")
ca_coc_527 <- st_read("R/data/California/CA_527/CA_527.shp")
#ca_coc_530 <- st_read("R/data/California/CA_530/CA_320.shp")
ca_coc_600 <- st_read("R/data/California/CA_600/CA_600.shp")
ca_coc_601 <- st_read("R/data/California/CA_601/CA_601.shp")
ca_coc_602 <- st_read("R/data/California/CA_602/CA_602.shp")
ca_coc_603 <- st_read("R/data/California/CA_603/CA_603.shp")
ca_coc_604 <- st_read("R/data/California/CA_604/CA_604.shp")
ca_coc_606 <- st_read("R/data/California/CA_606/CA_606.shp")
ca_coc_607 <- st_read("R/data/California/CA_607/CA_607.shp")
ca_coc_608 <- st_read("R/data/California/CA_608/CA_608.shp")
ca_coc_609 <- st_read("R/data/California/CA_609/CA_609.shp")
ca_coc_611 <- st_read("R/data/California/CA_611/CA_611.shp")
ca_coc_612 <- st_read("R/data/California/CA_612/CA_612.shp")
ca_coc_613 <- st_read("R/data/California/CA_613/CA_613.shp")
ca_coc_614 <- st_read("R/data/California/CA_614/CA_614.shp")

ca_coc <- bind_rows(ca_coc_500, ca_coc_501, ca_coc_502, ca_coc_503, ca_coc_504, ca_coc_505,
          ca_coc_506, ca_coc_507, ca_coc_508, ca_coc_509, ca_coc_510, 
          ca_coc_511, ca_coc_512, ca_coc_513, ca_coc_514, ca_coc_515,
          ca_coc_516, ca_coc_517, ca_coc_518, ca_coc_519, ca_coc_520, 
          ca_coc_521, ca_coc_522, ca_coc_523, ca_coc_524, ca_coc_525,
          ca_coc_526, ca_coc_527, ca_coc_600, ca_coc_601, ca_coc_602, 
          ca_coc_603, ca_coc_604, ca_coc_606, ca_coc_607, ca_coc_608, ca_coc_609,
          ca_coc_611, ca_coc_612, ca_coc_613, ca_coc_614)

rm(ca_coc_500, ca_coc_501, ca_coc_502, ca_coc_503, ca_coc_504, ca_coc_505,
          ca_coc_506, ca_coc_507, ca_coc_508, ca_coc_509, ca_coc_510, 
          ca_coc_511, ca_coc_512, ca_coc_513, ca_coc_514, ca_coc_515,
          ca_coc_516, ca_coc_517, ca_coc_518, ca_coc_519, ca_coc_520, 
          ca_coc_521, ca_coc_522, ca_coc_523, ca_coc_524, ca_coc_525,
          ca_coc_526, ca_coc_527, ca_coc_600, ca_coc_601, ca_coc_602, 
          ca_coc_603, ca_coc_604, ca_coc_606, ca_coc_607, ca_coc_608, ca_coc_609,
          ca_coc_611, ca_coc_612, ca_coc_613, ca_coc_614)

coc_to_tract <- read.csv("https://raw.githubusercontent.com/tomhbyrne/HUD-CoC-Geography-Crosswalk/master/output/tract_coc_match.csv")

pop <- get_acs(geography = "tract", 
                variables = c("B01001_001"),
                state = "CA",
                year = 2017)

pop <-
  pop |>
  mutate(tract_fips = as.double(GEOID))

pops_per_coc <- 
  coc_to_tract |>
  inner_join(pop, by = "tract_fips") |> 
  group_by(coc_number, variable) |>
  summarize(total = sum(estimate))

ca_coc_joined <-
  ca_coc |>
  inner_join(pops_per_coc, by = c("COCNUM"= "coc_number"))

ca_coc_joined <-
  ca_coc_joined |>
  inner_join(pit, by = c("COCNUM" = "CoC Number")) |>
  mutate(homeless_rate = (`Overall Homeless, 2017`/total) * 10000) |>
  rename(total_homeless = `Overall Homeless, 2017`,
         total_population = total) |>
  select(ST, COCNUM, COCNAME, total_homeless, total_population, homeless_rate, geometry)

st_write(ca_coc_joined, "ca_homeless/geoddata.shp")


# Code for Race

race <- get_acs(geography = "tract", 
                      variables = c("C02003_003", 
                                    "C02003_004", 
                                    "C02003_005", 
                                    "C02003_006", 
                                    "C02003_007", 
                                    "C02003_009"),
                      state = "CA",
                      year = 2017)

race$variable <-
  recode(race$variable,
         C02003_003 = "White",
         C02003_004 = "Black or African American",
         C02003_005 = "American Indian or Alaska Native",
         C02003_006 = "Asian",
         C02003_007 = "Native Hawaiian or Other Pacific Islander",
         C02003_009 = "Multiple Races")




