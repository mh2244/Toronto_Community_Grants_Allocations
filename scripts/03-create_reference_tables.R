#### Preamble ####
# Purpose: Downloads Community Grant Allocations data and saves the data from Open Data Toronto.
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run script 02-download_data first. 
# Any other information needed? N/A

#### Workspace setup ####
library(stringr)
library(tidyverse)
library(dplyr)

# Functions

# As data was inputted by hand, many datasets include extra information at the bottom of the table
# This function will find the first row in which this information appears
keep_orgs_only <- function(df, id = "id"){
  split_index <- min(which(is.na(df[[id]]))) - 1
  return(split_index)
}

# Reference Tables

# Some grants from earlier years are no longer funded and so are difficult to find on the City of Toronto site
# I created a table to identify these older grants and their full names
other_funds_ref <- tibble(
  Funding.Program = c("APCIP", "CCC", "CFSE", "DPCIP", "ENF","FSIP", "HER","StrArts", "StART", "MNR", "MJR", "GameOn", "SNOW"),
  Full.Name = c("AIDS Prevention Community Investment Program", "Competitiveness, Creativity and Collaboration", 
                "Community Festivals & Special Events", "Drug Prevention Community Investment Program", 
                "Emerging Neighbourhood Fund","Food Security Investment Program", "Home Efficiency Rebate","StreetARToronto", 
                "StreetARToronto", "Minor Recreation Funding", "Major Recreation Funding", "GameOn", "Snow Shovelling and Lawn Care")
)

# Upload ward reference table
wards_ref <- read.csv(file = "data/01-raw_data/wards_ref.csv", header = TRUE)

# Upload raw data from all years
raw_2022_to_2025 <- read.csv(file="data/01-raw_data/raw_data_2022_to_2025.csv", header = TRUE)
raw_2021 <- read.csv(file="data/01-raw_data/raw_data_2021.csv", header = TRUE)
raw_2020 <- read.csv(file="data/01-raw_data/raw_data_2020.csv", header = TRUE)
raw_2020_addon <- read.csv(file="data/01-raw_data/raw_data_2020_addon.csv", header = TRUE)
raw_2019 <- read.csv(file="data/01-raw_data/raw_data_2019.csv", header = TRUE)
raw_2018 <- read.csv(file="data/01-raw_data/raw_data_2018.csv", header = TRUE)
raw_2017 <- read.csv(file="data/01-raw_data/raw_data_2017.csv", header = TRUE)
raw_2016 <- read.csv(file="data/01-raw_data/raw_data_2016.csv", header = TRUE)
raw_2015 <- read.csv(file="data/01-raw_data/raw_data_2015.csv", header = TRUE)
raw_2014 <- read.csv(file="data/01-raw_data/raw_data_2014.csv", header = TRUE)
raw_2013 <- read.csv(file="data/01-raw_data/raw_data_2013.csv", header = TRUE)
raw_2012 <- read.csv(file="data/01-raw_data/raw_data_2012.csv", header = TRUE)
raw_2011 <- read.csv(file="data/01-raw_data/raw_data_2011.csv", header = TRUE)
raw_2010 <- read.csv(file="data/01-raw_data/raw_data_2010.csv", header = TRUE)

# Upload legends and other extra information
raw_fund_names <- read.csv(file="data/01-raw_data/raw_allocations_since_2022.csv", header = TRUE)
raw_readme <- read.csv(file = "data/01-raw_data/raw_readme.csv", header = TRUE)
legend_2021 <- read.csv(file="data/01-raw_data/raw_legend_2021.csv", header = TRUE)
legend_2020 <- read.csv(file="data/01-raw_data/raw_legend_2020.csv", header = TRUE)
legend_2019 <- read.csv(file="data/01-raw_data/raw_legend_2019.csv", header = TRUE)
legend_2018 <- read.csv(file="data/01-raw_data/raw_legend_2018.csv", header = TRUE)
all_legends_18_21 <- rbind(legend_2021, legend_2020, legend_2019, legend_2018) |> select(-c(Comments...Examples)) |> 
  distinct() |> rename(Funding.Program = Field.Name...Item...Column.name, Full.Name = Description...Definition) |>
  filter((!is.na(Funding.Program)) & !grepl("Total|Local|City|Neighbourhood|Other|Service|Organization|Ward|NOTE|#", Funding.Program))

#### Create master table for organizations ####

# Wide datasets

# Some datasets' Ward entries have asterisks to indicate differences in mailing vs. service locations
# However, this is not an issue as the service location is listed. Thus, use parse_number to extract the ward number
# Remove duplicates and NA values with keep_orgs_only function
unique_orgs_2010 <- raw_2010 |> select(Organization, Ward, Service.Area, year) |> distinct() |> 
  slice(1:keep_orgs_only(raw_2010, id = "...1")) |> mutate(Ward = parse_number(Ward))
unique_orgs_2011 <- raw_2011 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2011, id = "X.")) |> mutate(Ward = parse_number(Ward))
unique_orgs_2012 <- raw_2012 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2012, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
unique_orgs_2013 <- raw_2013 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2013, id = "X.")) |> mutate(Ward = as.numeric(Ward))
unique_orgs_2014 <- raw_2014 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2014, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
unique_orgs_2015 <- raw_2015 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2015, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
unique_orgs_2016 <- raw_2016 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  slice(1:keep_orgs_only(raw_2016, id = "X.")) |> mutate(Ward = as.numeric(Ward))
# Some organizations have multiple locations, so their entry under "Ward" is a series of numbers 
# Separate them into rows, then convert the column into a numeric one
unique_orgs_2017 <- raw_2017 |> select(Organization, Ward, Service.Area, year) |>
  separate_rows(Ward, sep=",") |> mutate(Ward = as.numeric(Ward)) |> distinct() 
# long formats
unique_orgs_2018 <- raw_2018 |> select(Organization, Ward, Service.Area, year) |> distinct() 

unique_orgs_2019 <- raw_2019 |> select(Organization, Ward, Service.Area, year) |> distinct() |> 
  separate_rows(Ward, sep=",") |> mutate(Ward = as.numeric(Ward)) # multiple wards

unique_orgs_2020 <- raw_2020 |> select(Organization, Ward, Service.Area, year) |> distinct() |> 
  separate_rows(Ward, sep=",") |> mutate(Ward = as.numeric(Ward)) # multiple wards

unique_orgs_2020_addon <- raw_2020_addon |> select(Organization, Ward, Service.Area, year) |> distinct()

unique_orgs_2021 <- raw_2021 |> select(Organization, Ward, Service.Area, year) |> distinct() |>
  separate_rows(Ward, sep=",") |> mutate(Ward = as.numeric(Ward)) # multiple wards

unique_orgs_2022_25 <- raw_2022_to_2025 |> select(Organization, Ward, Service.Area, date_from_filename) |> 
  rename(year = date_from_filename) |> distinct() |> separate_rows(Ward, sep=",") |>
  mutate(Ward = str_trim(Ward)) 

# It is difficult to match ward numbers and ward names due to various typos and punctuation marks
# Strip each entry of punctuation marks and spaces before matching
unique_orgs_2022_25$Ward <- gsub("[[:punct:] ]+", "", unique_orgs_2022_25$Ward)
wards_ref$Ward.Name <- gsub("[[:punct:] ]+", "", wards_ref$Ward.Name)
unique_orgs_2022_25 <- unique_orgs_2022_25 |> left_join(wards_ref, by=c("Ward"="Ward.Name")) |>
  select(Organization, Ward.Number, Service.Area, year, Ward) |> rename(Ward.Name = Ward) |>
  rename(Ward = Ward.Number)

# Combine multiple years
combined_orgs_p1 <- bind_rows(unique_orgs_2010, unique_orgs_2011, unique_orgs_2012,
                           unique_orgs_2013, unique_orgs_2014, unique_orgs_2015,
                           unique_orgs_2016, unique_orgs_2017, unique_orgs_2018,
                           unique_orgs_2019, unique_orgs_2020, unique_orgs_2020_addon,
                           unique_orgs_2021) |> left_join(wards_ref, by=c("Ward"="Ward.Number"))

# By arranging it in descending order by organization, I can see each organization's yearly submissions from 2010-2025
# After 2017, I will be able to see what ward the non-profit was rezoned to and can update the master table accordingly
all_orgs <- bind_rows(combined_orgs_p1, unique_orgs_2022_25) |> arrange(desc(Organization)) |>
  filter(!is.na(Ward.Name)) |> filter(!is.na(Ward)) |> mutate(service.area = tolower(str_remove_all(Service.Area, " "))) |> 
  mutate(
    service.area = gsub("citywide|city-wide|ciry-wide", "city", service.area),
    service.area = gsub("areaservices|area-services|etobicokeandscarborough|areaservice", "local",service.area),
    service.area = gsub("neighborhood", "neighbourhood", service.area)) |>
  select(-c(year, Service.Area)) |> distinct()

#### Create master table for grant programs and funders ####

# 2022-25 is unique since an additional "Division" cateogry is provided
# Get the grant programs from each year
unique_funds_2022_25 <- raw_2022_to_2025 |> select(Funding.Program, Division, date_from_filename) |> 
  distinct() |> rename(year = date_from_filename)
unique_funds_2021 <- raw_2021 |> select(Funder..Funding.Program, year) |> distinct()
unique_funds_2020 <- raw_2020 |> select(Funder..Funding.Program, year) |> distinct()
unique_funds_2020_addon <- raw_2020_addon |> select(Funder..Funding.Program, year) |> distinct()
unique_funds_2019 <- raw_2019 |> select(Funder..Funding.Program, year) |> distinct()
unique_funds_2018 <- raw_2018 |> select(Funder, year) |> distinct()

# Wide format tables, which need to be converted into long format
# Tables 2010-2017 give each grant its own column
# However, I want a single column which lists all the grants a nonprofit applied to
unique_funds_2010 <- colnames(raw_2010 |> select(-(1:match("Total.Allocations", names(raw_2010))))|> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2010)

# figure out how to get only col names?
unique_funds_2011 <- colnames(raw_2011 |> select(-(1:match("Total.Allocations", names(raw_2011)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2011)
unique_funds_2012 <- colnames(raw_2012 |> select(-(1:match("Total.Allocations", names(raw_2012)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2012)
unique_funds_2013 <- colnames(raw_2013 |> select(-(1:match("Total.Allocations", names(raw_2013)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2013)
unique_funds_2014 <- colnames(raw_2014 |> select(-(1:match("Total.Allocations", names(raw_2014)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2014)
unique_funds_2015 <- colnames(raw_2015 |> select(-(1:match("Total.Allocations", names(raw_2015)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2015)
unique_funds_2016 <- colnames(raw_2016 |> select(-(1:match("Total.Allocations", names(raw_2016)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2016)
unique_funds_2017 <- colnames(raw_2017 |> select(-(1:match("Total.Funding.Amount", names(raw_2017)))) |> select(-"year")) |>
  tibble() |> rename("Funding.Program" = "colnames(...)") |> mutate(Full.Name = NA, year = 2017)

# Bind grant program data together for years 2010-2017
unique_funds_10_17 <- rbind(unique_funds_2010, unique_funds_2011, unique_funds_2012, unique_funds_2013,
                            unique_funds_2014, unique_funds_2015, unique_funds_2016, unique_funds_2017) |> 
  distinct()

# Add grant program data from 2018-2021. While I did research and tried to match any missing grant program acronyms with their full equivalents, 
  # this was not always possible, and so any program I was not able to verify was removed.
all_funds_10_21 <- rbind(unique_funds_10_17, all_legends_18_21) |> arrange(desc(Funding.Program)) |> 
  left_join(other_funds_ref, by = "Funding.Program") |> 
  mutate(Full.Name.Final = coalesce(Full.Name.x, Full.Name.y)) |> select(-c(Full.Name.x, Full.Name.y, year)) |>
  distinct() |> filter(!is.na(Full.Name.Final))

diffs <- (setdiff(trimws(unique_funds_2022_25$Funding.Program), trimws(all_funds_10_21$Funding.Program)))
diffs_22_25 <- tibble(Funding.Program = diffs) |> mutate(Full.Name.Final = NA)

# Match remaining grant acronyms
all_funds <- rbind(all_funds_10_21, diffs_22_25) |> arrange(desc(Funding.Program))
all_funds <- all_funds |> mutate(Full.Name.Final = case_when(
  grepl("TAC", Funding.Program) ~ "Toronto Arts Council",
  grepl("StARTPP", Funding.Program) ~ "StreetARToronto Partnership Program",
  grepl("Local Arts Service Organizations", Funding.Program)~ "Local Arts Service Organizations",
  grepl("Youth Cultural Incubators Stabilization Initiative", Funding.Program) ~"Youth Cultural Incubators Stabilization Initiative",
  grepl("Other", Funding.Program) ~ "Other",
  grepl("None", Funding.Program, ignore.case = TRUE) ~ "None", 
  grepl("CSP-OneTime", Funding.Program) ~ "Community Services Partnerships",
  grepl("ICAG", Funding.Program) ~ "Indigenous Climate Action Grants",
  grepl("ILG", Funding.Program) ~ "Local Leadership Grant",
  grepl("IFF", Funding.Program) ~ "Indigenous Funding Framework",
  grepl("IAO", Funding.Program) ~ "Indigenous Affairs Office",
  grepl("COOP", Funding.Program) ~ "Cultural Organization Operating Partnership",
  grepl("CFFP", Funding.Program) ~ "Cultural Festivals Funding Program",
  grepl("CFIF", Funding.Program) ~ "Circular Food Innovators Fund",
  grepl("CPP", Funding.Program) ~ "Community Partnership Program",
  grepl("CHS|CH", Funding.Program) ~ "Uncategorized",
  grepl("DON|CIU|CDU", Funding.Program) ~ "Uncategorized",
  grepl("IACPF", Funding.Program) ~ "Indigenous Arts and Culture Partnerships Fund",
  grepl("YCAG", Funding.Program) ~ "Youth Climate Action Grants",
  grepl("SSRF", Funding.Program) ~ "Social Services Relief Fund",
  grepl("TTSP", Funding.Program) ~ "Toronto Tenant Support Program",
  grepl("TSEIP", Funding.Program) ~ "Toronto Significant Event Investment Program",
  grepl("RPSDP", Funding.Program) ~ "Regent Park Social Development Plan",
  grepl("HPP", Funding.Program) ~ "Homelessness Prevention Program",
  grepl("CSWBU", Funding.Program) ~ "Community Safety and Wellbeing Unit",
  grepl("CABR", Funding.Program) ~ "Confronting Anti-Black Racism",
  grepl("BMFF", Funding.Program) ~ "Black-Mandated Funding Framework",
  grepl(" ", Funding.Program) ~ Funding.Program,
  TRUE ~ Full.Name.Final)) |> filter(!is.na(Full.Name.Final))

all_funds <- all_funds %>% distinct(Funding.Program, .keep_all = TRUE)

#### Save master files ####
write.csv(all_orgs, file = "data/02-analysis_data/master_organizations.csv", row.names = FALSE)
write.csv(all_funds, file = "data/02-analysis_data/master_funds.csv", row.names = FALSE)
write.csv(wards_ref, file = "data/02-analysis_data/master_wards.csv", row.names=FALSE)
