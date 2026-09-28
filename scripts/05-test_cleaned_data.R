#### Preamble ####
# Purpose: Tests cleaned data
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download, generate reference tables, and clean data

#### Workspace Set-up ####
library(validate)

#### Retrieve simulated data  and reference tables ####
data <- read.csv(file = "data/02-analysis_data/clean_data.csv", header = TRUE)
unmerged_data <- read.csv(file="data/02-analysis_data/unmerged_clean_data.csv", header=TRUE)
org_ref_tbl <- read.csv(file = "data/02-analysis_data/master_organizations.csv", header=TRUE)
funder_ref_tbl <- read.csv(file = "data/02-analysis_data/master_funds.csv", header=TRUE)
uncat_funds <- read.csv(file = "data/02-analysis_data/uncat_funds.csv", header=TRUE)

#### Test simulated data ####

# Test for null values in any of the numerical columns
null_rules <- validator(all_complete(X, year, organization, service_area,
                                     ward_number, grant_amount, grant_code,
                                     Full.Name.Final, Ward.Name, infl_adj_funding))
null_test <- confront(data, null_rules)
null_test

# Check for uniqueness
duplicate_rules <- validator(
  is_unique(X, year, organization, service_area,
            ward_number, grant_amount, grant_code,
            Full.Name.Final, Ward.Name, infl_adj_funding)
)
duplicate_check <- confront(data, duplicate_rules)
duplicate_check

# Ensure that they follow reasonable ranges
# Use the reference tables to check variables like organization and fund names for convenience
check_value_rules <- validator(
  id >= 1,
  grant_amount >= 0,
  ward_number > 0 & ward_number <= 25,
  infl_adj_funding >= 1.00,
  year >= 2010 & year <= 2025,
  organization %in% org_ref_tbl$Organization,
  grant_code %in% c(funder_ref_tbl$Funding.Program, uncat_funds$x),
  service_area %in% c("city", "local", "neighbourhood", "multiple"),
  Full.Name.Final %in% c(funder_ref_tbl$Full.Name.Final, "Uncategorized")
)
check_value_validate <- confront(data, check_value_rules)
check_value_validate

# Check that sum of grant amounts adjusted for inflation is larger than sum of unadjusted grant column
infl_rules <- validator(
  sum(infl_adj_funding) > sum(grant_amount)
)
infl_validate <- confront(data, infl_rules)
infl_validate

# Check that grant totals did not change when merging dataset with reference tables
grants_both_tbls <- tibble(
  merged_ver = data$grant_amount,
  unmerged_ver = unmerged_data$grant_amount)

same_total_rules <- validator(
  sum(merged_ver) == sum(unmerged_ver)
)
same_total_validate <- confront(grants_both_tbls, same_total_rules)
same_total_validate
