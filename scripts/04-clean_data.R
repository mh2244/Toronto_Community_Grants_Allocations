#### Preamble ####
# Purpose: Cleans data which was downloaded from '02-download_data.R'.
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download data and create reference tables.
# Any other information needed? N/A

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(readr)
library(priceR) 

split_index <- NA
split_index_check <- NA

# This function converts a row in a wide-format table into long format
wide_to_long <- function(start_col, row_num, raw_data){
  i <- row_num
  grants_won_index <- which(!is.na(raw_data[i,(start_col+1):(ncol(raw_data)-1)])) + start_col
  store_grant_names <- names(raw_data)[grants_won_index] 
  store_grant_amount <- unlist(raw_data[i, grants_won_index], use.names = FALSE) # Originally a dataframe, use unlist()
  # Order is preserved so set use.names to false
  entry_length <- length(grants_won_index)
                      
  entry <- tibble(
    year = rep(raw_data$year[i], entry_length),
    organization = rep(raw_data$Organization[i], entry_length),
    service_area = rep(raw_data$Service.Area[i], entry_length),
    ward_number = rep(raw_data$Ward[i], entry_length),
    grant_amount = store_grant_amount,
    grant_code = store_grant_names
  )
  return(entry)
}

# This function identifies the first row where extraneous information is listed
# Used for datasets 2010-2017
split_df <- function(df, id = "id"){
  split_index <- min(which(is.na(df[[id]]))) - 1
  return(split_index)
}

# This function creates dataframes to store cleaned data
init_clean_df <- function(){
  df <- tibble(year = numeric(), organization = character(), service_area = character(), ward_number = numeric(), grant_amount = numeric(),
               grant_code = character())
  return(df)
}

#### Load datasets ####

# Read in reference tables
master_orgs_ref <- read.csv(file="data/02-analysis_data/master_organizations.csv", header = TRUE)
master_funds_ref <- read.csv(file="data/02-analysis_data/master_funds.csv", header = TRUE)
master_wards_ref <- read.csv(file="data/02-analysis_data/master_wards.csv", header=TRUE)

# Read in raw data
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

# Create dataframes for cleaned data
clean_2010 <- init_clean_df()
clean_2011 <- init_clean_df()
clean_2012 <- init_clean_df()
clean_2013 <- init_clean_df()
clean_2014 <- init_clean_df()
clean_2015 <- init_clean_df()
clean_2016 <- init_clean_df()
clean_2017 <- init_clean_df()
clean_2018 <- init_clean_df()
clean_2019 <- init_clean_df()
clean_2020 <- init_clean_df()
clean_2021 <- init_clean_df()
clean_2022_2025 <- init_clean_df()

# Reference simulated data if I need to
raw_sim <- read.csv(file="data/01-raw_data/raw_sim_data.csv", header = TRUE)

#### Data wrangling ####

# Cut off rows containing extra information. Reference column varies. Used whichever one was most consistent.
keep_orgs_2010 <- raw_2010 |> slice(1:split_df(raw_2010, id = "...1")) |> mutate(Ward = parse_number(Ward))
keep_orgs_2011 <- raw_2011 |>  slice(1:split_df(raw_2011, id = "X.")) |> mutate(Ward = parse_number(Ward))
keep_orgs_2012 <- raw_2012 |>  slice(1:split_df(raw_2012, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
keep_orgs_2013 <- raw_2013 |> slice(1:split_df(raw_2013, id = "X.")) |> mutate(Ward = as.numeric(Ward))
keep_orgs_2014 <- raw_2014 |> slice(1:split_df(raw_2014, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
keep_orgs_2015 <- raw_2015 |> slice(1:split_df(raw_2015, id = "Ward")) |> mutate(Ward = as.numeric(Ward))
keep_orgs_2016 <- raw_2016 |> slice(1:split_df(raw_2016, id = "X..of.Grants")) |> mutate(Ward = as.numeric(Ward))
keep_orgs_2017 <- raw_2017 |> separate_rows(Ward, sep=",") |> mutate(Ward = as.numeric(Ward)) # 2017 dataset OK

# Now, retrieve the index numbers in each keep_org table where the actual grants begin
index_2010 <- grep("total.allocations", tolower(names(keep_orgs_2010))) 
index_2011 <- grep("total.allocations", tolower(names(keep_orgs_2011)))
index_2012 <- grep("total.allocations", tolower(names(keep_orgs_2012)))
index_2013 <- grep("total.allocations", tolower(names(keep_orgs_2013)))
index_2014 <- grep("total.allocations", tolower(names(keep_orgs_2014)))
index_2015 <- grep("total.allocations", tolower(names(keep_orgs_2015)))
index_2016 <- grep("total.allocations", tolower(names(keep_orgs_2016)))
index_2017 <- grep("total.funding.amount", tolower(names(keep_orgs_2017)))

index_10_17 <- c(index_2010, index_2011, index_2012, index_2013, index_2014, index_2015,
                 index_2016, index_2017)

# Turn all wide formatted tables into long formatted tables.
for (row_num in 1:nrow(keep_orgs_2010)){
  entry <- wide_to_long(index_2010, row_num, keep_orgs_2010)
  clean_2010 <- rbind(clean_2010, entry)
}

for (row_num in 1:nrow(keep_orgs_2011)){
  entry <- wide_to_long(index_2011, row_num, keep_orgs_2011)
  clean_2011 <- rbind(clean_2011, entry)
}

for (row_num in 1:nrow(keep_orgs_2012)){
  entry <- wide_to_long(index_2012, row_num, keep_orgs_2012)
  clean_2012 <- rbind(clean_2012, entry)
}

for (row_num in 1:nrow(keep_orgs_2013)){
  entry <- wide_to_long(index_2013, row_num, keep_orgs_2013)
  clean_2013 <- rbind(clean_2013, entry)
}

for (row_num in 1:nrow(keep_orgs_2014)){
  entry <- wide_to_long(index_2014, row_num, keep_orgs_2014)
  clean_2014 <- rbind(clean_2014, entry)
}

for (row_num in 1:nrow(keep_orgs_2015)){
  entry <- wide_to_long(index_2015, row_num, keep_orgs_2015)
  clean_2015 <- rbind(clean_2015, entry)
}

for (row_num in 1:nrow(keep_orgs_2016)){
  entry <- wide_to_long(index_2016, row_num, keep_orgs_2016)
  clean_2016 <- rbind(clean_2016, entry)
}

for (row_num in 1:nrow(keep_orgs_2017)){
  entry <- wide_to_long(index_2017, row_num, keep_orgs_2017)
  clean_2017 <- rbind(clean_2017, entry)
}

# Long datasets
join_2018 <- raw_2018 |> select(year, Organization, Service.Area, Ward, Total.Funding.Amount, Funder) |> 
  rename(organization = Organization, service_area = Service.Area, ward_number = Ward,
                                          grant_amount = Total.Funding.Amount, grant_code = Funder) |> 
  mutate(ward_number = as.numeric(ward_number))
# Tibble initialized with correct variable types, also acts as a check.
clean_2018 <- rbind(clean_2018, join_2018)

# Datasets 2019-2025 list multiple ward numbers under the Ward column
# This is problematic as it is a character() col instead of numeric
# I won't be able to map the Ward name easily unless it is numeric
# I can separate by comma but then grant amount is copied as new rows are created
# When aggregating total grant funding this means grant funding will be inflated
# So I will need to divide by number of ward numbers in the ward number column
count_wards <- function(df, col="Ward"){
  count <- str_count(col, "\\d+")
  return(count)
}

# Make a function that counts how many #s in ward then divides by number of wards, so total stays consistent
join_2019 <- raw_2019 |> select(year, Organization, Service.Area, Ward, Total.Funding.Amount, Funder..Funding.Program) |> 
  mutate(Total.Funding.Amount = (Total.Funding.Amount/str_count(Ward, "\\d+"))) |> 
  rename(organization = Organization, service_area = Service.Area, ward_number = Ward,
                                          grant_amount = Total.Funding.Amount, grant_code = Funder..Funding.Program) |>
  separate_rows(ward_number, sep=",") |> mutate(ward_number = as.numeric(ward_number))
clean_2019 <- rbind(clean_2019, join_2019)

join_2020 <- raw_2020 |> select(year, Organization, Service.Area, Ward, Total.Funding.Amount, Funder..Funding.Program) |> 
  mutate(Total.Funding.Amount = (Total.Funding.Amount/str_count(Ward, "\\d+"))) |> rename(organization = Organization, service_area = Service.Area, ward_number = Ward,
                                          grant_amount = Total.Funding.Amount, grant_code = Funder..Funding.Program) |>
  separate_rows(ward_number, sep=",") |> mutate(ward_number = as.numeric(ward_number))
clean_2020 <- rbind(clean_2020, join_2020)

join_2020_addon <- raw_2020_addon |> select(year, Organization, Service.Area, Ward, Total.Funding.Amount, Funder..Funding.Program) |> 
  rename(organization = Organization, service_area = Service.Area, ward_number = Ward,
                                          grant_amount = Total.Funding.Amount, grant_code = Funder..Funding.Program)
clean_2020 <- rbind(clean_2020, join_2020_addon)

join_2021 <- raw_2021 |> select(year, Organization, Service.Area, Ward, Total.Funding.Amount, Funder..Funding.Program) |> 
  mutate(Total.Funding.Amount = (Total.Funding.Amount/str_count(Ward, "\\d+"))) |> rename(organization = Organization, service_area = Service.Area, ward_number = Ward,
                                          grant_amount = Total.Funding.Amount, grant_code = Funder..Funding.Program) |> 
  separate_rows(ward_number, sep=",") |> mutate(ward_number = as.numeric(ward_number))
clean_2021 <- rbind(clean_2021, join_2021)

# Get rid of "None" entries in grant amounts
join_22_25 <- raw_2022_to_2025 |> select(date_from_filename, Organization, Service.Area, Ward, Total.Funding.Amount, Funding.Program) |> 
  rename(year= date_from_filename, organization = Organization, service_area = Service.Area, grant_amount = Total.Funding.Amount, grant_code = Funding.Program, ward_name = Ward) |>
  mutate(grant_amount = as.numeric(str_replace_all(grant_amount, "[^0-9.]", ""))) |>
  mutate (grant_amount = grant_amount/(str_count(ward_name, ",") + 1)) |>
  separate_rows(ward_name, sep=",") |> 
  mutate(ward_name = str_replace_all(ward_name, "[[:punct:] ]+", "")) |> 
  left_join(master_wards_ref, by=c("ward_name"="Ward.Name")) |> select(-ward_name) |>
  rename("ward_number" = "Ward.Number") |>
  select(year, organization, service_area, ward_number,grant_amount, grant_code)

clean_2022_2025 <- rbind(clean_2022_2025, join_22_25)

# Clean up the service areas, and the organizations.
# Use reference tables and match entries.
clean_all_years <- init_clean_df()
clean_all_years <- rbind(clean_all_years, clean_2010, clean_2011,
                         clean_2012, clean_2013, clean_2014, clean_2015,
                         clean_2016,clean_2017, clean_2018, clean_2019,
                         clean_2020, clean_2021, clean_2022_2025) %>%
  filter(ward_number <= 25)

clean_all_years_serv_area <- clean_all_years |> mutate(service_area = tolower(str_remove_all(service_area, " "))) |> 
  mutate(service_area = gsub("citywide|city-wide|ciry-wide", "city", service_area),
         service_area = gsub("areaservices|area-services|etobicokeandscarborough|areaservice", "local",service_area),
         service_area = gsub("neighborhood", "neighbourhood", service_area))

clean_all_years_merge_funds <- left_join(clean_all_years_serv_area, master_funds_ref, by=c("grant_code"="Funding.Program")) |>
  mutate(Full.Name.Final = if_else(is.na(Full.Name.Final), "Uncategorized", Full.Name.Final))

clean_all_years_merge_funds_orgs <- left_join(clean_all_years_merge_funds, master_orgs_ref, 
                                              by=c("organization" = "Organization", "ward_number" = "Ward", "service_area" = "service.area")) |>
  mutate(infl_adj_funding = adjust_for_inflation(grant_amount, from_date = year, country="CA", to_date=2025)) |>
  mutate(id = row_number()) |>
  filter(!is.na(grant_code))

uncat_funds <- setdiff(clean_all_years_merge_funds_orgs$grant_code, master_funds_ref$Funding.Program)

#### Export cleaned data and uncategorized grants for checking ####
write.csv(uncat_funds, file ="data/02-analysis_data/uncat_funds.csv", row.names = FALSE)
write.csv(clean_all_years, file = "data/02-analysis_data/unmerged_clean_data.csv", row.names = FALSE)
write.csv(clean_all_years_merge_funds_orgs, file="data/02-analysis_data/clean_data.csv", row.names = FALSE)
