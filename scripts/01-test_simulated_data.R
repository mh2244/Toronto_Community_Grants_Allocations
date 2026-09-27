#### Preamble ####
# Purpose: Tests simulated data
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run simulation script, 00-simulate_data.R

#### Workspace Set-up ####
library(validate)

#### Retrieve simulated data  and reference tables ####
data <- read.csv(file = "data/01-raw_data/raw_sim_data.csv")
org_ref_tbl <- read.csv(file = "data/01-raw_data/sim_org_data.csv")
funder_ref_tbl <- read.csv(file = "data/01-raw_data/sim_funder_data.csv")

#### Test simulated data ####

# Test for null values in any of the numerical columns
null_rules <- validator(all_complete(id, year, funder_code, funder_fullname,
                                     funder_category, grant_amount, winner_id,
                                     winner_full_name, winner_service_area,
                                     winner_ward_number, factor))
null_test <- confront(data, null_rules)
null_test

# Check for uniqueness
duplicate_rules <- validator(
  is_unique(id, year, funder_code, funder_fullname,
            funder_category, grant_amount, winner_id,
            winner_full_name, winner_service_area,
            winner_ward_number, factor)
)
duplicate_check <- confront(data, duplicate_rules)
duplicate_check

# Ensure that they follow reasonable ranges
# Use the reference tables to check variables like organization and fund names for convenience
check_value_rules <- validator(
  grant_amount >= 0,
  winner_ward_number > 0 & winner_ward_number <= 25,
  factor >= 1.00 & factor <= 1.15,
  funder_code %in% funder_ref_tbl$funder_code,
  year >= 2010 & year <= 2025,
  winner_id %in% org_ref_tbl$org_code,
  winner_service_area %in% c("city", "local", "neighbourhood"),
  winner_full_name %in% org_ref_tbl$name,
  funder_fullname %in% funder_ref_tbl$funder_full_name,
  funder_category %in% funder_ref_tbl$category
)
check_value_validate <- confront(data, check_value_rules)
check_value_validate
