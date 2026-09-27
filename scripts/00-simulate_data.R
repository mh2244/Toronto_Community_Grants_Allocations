#### Preamble ####
# Purpose: Simulates dataset pertaining to grant allocations by ward and program type
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: None.

#### Workspace setup ####
library(tidyverse)
library(bit64)
library(dplyr)
library(knitr)

set.seed(123) # Set seed for reproducibility

#### Simulation ####

# Define variables
start_year <- 2010
end_year <- 2025
multiple_installments_check <- FALSE

# Define function to select winner of funding
# Non-profits are not always eligible to apply for every single grant.
  # To simulate an eligible organization winning, create a function that takes the organization ID and grant program ID
  # If the IDs are mod 0, then label that as a win.
select_winner <- function(index, max){
  winner <- FALSE
  
  while (winner == FALSE){
    # Select organization  
    selection <- runif64(n = 1, min = 1, max = max) # Max will be length(organizations$name), which is 14 in this case
    # Determine if winner or not
    ifelse(test = pmax(selection, index)%%pmin(selection, index)==0,yes= winner<-TRUE, no=winner<-FALSE) 
  }
  return(as.numeric(selection))
}

# Create table of inflation factors with 2025 as the reference year
sim_inflation_factors <- tibble(
  year = seq(from=start_year, to=end_year, by=1),
  factor = seq(from=1.15, to=1.00, by=-0.01)
)

# Generate simulation data. Include their ID/org_code, full names, service area, and ward number
organizations <- tibble(
  org_code = seq(from = 1, to = 14, by = 1),
  name = c("Organization_A", "Organization_B", "Organization_C", "Organization_D", "Organization_E",
           "Organization_F", "Organization_G", "Organization_H", "Organization_I", "Organization_J",
           "Organization_K", "Organization_L", "Organization_M", "Organization_N"),
  service_area = c("city", "local", "neighbourhood",
                   "city", "local", "neighbourhood",
                   "city", "local", "neighbourhood",
                   "city", "local", "neighbourhood",
                   "city", "local"),
  ward_number = c(2, 4, 5, 6, 7, 8, 14, 11, 12, 13, 9, 12, 14, 1)
)

# Create simulated data of grant programs.
# While funding can change year over year, we assume a constant amount in simulated data for convenience.
funders <- tibble(
  funder_code = c(1, 2, 3, 4, 5, 6, 7),
  funder_full_name = c("Funder_1", "Funder_2", "Funder_3", "Funder_4", "Funder_5", "Funder_6", "Funder_7"),
  total_grant_amount = c(1000, 2000, 1000, 5000, 15000, 3000, 10000),
  category = c("Environment", "Health", "Arts", "Indigenous", "Housing", "Heritage", "Community safety"),
  num_app_periods = c(1, 3, 2, 1, 4, 1, 1), # Some grants have multiple application cycles
  num_installments = c(1, 1, 1, 1, 1, 1, 2), # Some grants give out funding in installments
  funding_per_installment = total_grant_amount/num_installments,
)

# Assume that all funding programs are applied to. Calculate the total number of entries
num_entries <- sum(funders$num_app_periods * funders$num_installments)

# Set up the simulation data table
sim_data <-
  tibble(
    id = seq(from = 1, to = (num_entries*(end_year-start_year+1)), by = 1),
    year = rep(x = start_year:end_year, each = num_entries),
    funder_code = rep(rep(x = funders$funder_code, times = (funders$num_app_periods * funders$num_installments)), times = end_year-start_year+1),
    funder_fullname = rep(rep(x = funders$funder_full_name, times = (funders$num_app_periods * funders$num_installments)), times = end_year-start_year+1),
    funder_category = rep(rep(x = funders$category, times = (funders$num_app_periods * funders$num_installments)), times = end_year-start_year+1),
    grant_amount = rep(rep(x = funders$funding_per_installment, times = (funders$num_app_periods * funders$num_installments)), times = end_year-start_year+1)
  )

# For each grant, select winner(s)
org_winners <- rep(NA, length(sim_data$funder_code))

for (i in 1:length(org_winners)){
  # Get the funding program code from simulated data
  row_index <- which(funders$funder_code==sim_data$funder_code[i])
  
  # Check if grant money is given out in multiple installments
  if (funders$num_installments[row_index] > 1){
    multiple_installments_check <- TRUE
  }
  # If there are multiple installments within the same year and i>1, then take the previous entry
  if(i > 1 && sim_data$funder_code[i] == sim_data$funder_code[i-1] && sim_data$year[i] == sim_data$year[i-1] && multiple_installments_check==TRUE){
    org_winners[i] <- org_winners[i-1] 
    multiple_installments_check <- FALSE
  } else { # Otherwise, if false, select a new winner.
    org_winners[i] <- select_winner(row_index, length(organizations$name))
  }
}

full_info_winners <- tibble(
  id = seq(from = 1, to = (num_entries*(end_year-start_year+1)), by = 1),
  winner_id = org_winners,
  winner_full_name = organizations$name[org_winners],
  winner_service_area = organizations$service_area[org_winners],
  winner_ward_number = organizations$ward_number[org_winners]
)

# Join organization and grant funding information together
sim_data_all_years <- left_join(sim_data, full_info_winners, by="id")
# Add inflation factors
sim_data_final <- sim_data_all_years |> left_join(sim_inflation_factors, by = "year")

#### Export Simulated Data ####
write.csv(sim_data_final, file = "data/01-raw_data/raw_sim_data.csv", row.names = FALSE)
write.csv(organizations, file = "data/01-raw_data/sim_org_data.csv", row.names = FALSE)
write.csv(funders, file = "data/01-raw_data/sim_funder_data.csv", row.names = FALSE)
