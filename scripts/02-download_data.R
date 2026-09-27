#### Preamble ####
# Purpose: Downloads Community Grant Allocations data and saves the data from Open Data Toronto.
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run 00-simulate_data.R and 01-test_simulated_data.R first
# Any other information needed? N/A

#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)

#### Download data ####

# Download reference table of Toronto's 25-ward system
# From 2010-2017, datasets used either the 44- or 47-ward system
# From 2018 onwards, the 25-ward system was used
# This reference table will be used to update and streamline ward numbers and names from older datasets
ward_sys_search <- search_packages("ward profiles")
ward_sys_id <- "6678e1a6-d25f-4dff-b2b7-aa8f042bc2eb"
resources_ward <- list_package_resources(ward_sys_id)
wards_ref <- get_resource("ea4cc466-bd4d-40c6-a616-7abfa9d7398f")

# Download community grant allocation data
keyword_search <- search_packages("community grant allocations")
dataset_id <-"6f20e59a-1dd9-4a1d-ae9b-ba56a126c7f5" 
resources <- list_package_resources(dataset_id)

# Community grant allocation data is saved in separate files by year
# Some files have multiple sheets; I went through the raw files and saved the appropriate sheet, if necessary
  # In multisheet files, the first contains the desired data, while the second contains legends or extra information explaining codes/acronyms used.
# 'Year' columns were added for organization purposes
# In addition, datasets 2018-2025 are in long format. Years 2022-2025 are saved in the same dataset
# Datasets 2010-2017 are in wide format
cga2021 <- get_resource("bb31a820-f516-4abb-8ac2-e24f9f300a9a")[[1]] |>
  mutate(year = "2021")
cga2021_legend <- get_resource("bb31a820-f516-4abb-8ac2-e24f9f300a9a")[[2]] |>
  mutate(year = "2021")

cga2020_addon <- get_resource("7533f834-b38c-49ab-a6d7-37b333b76baa") |>
  mutate(year = "2020")
cga2020 <- get_resource("0e9c7d33-eed8-489a-abc8-1145f9698542")[[1]] |>
  mutate(year = "2020")
cga2020_legend <- get_resource("0e9c7d33-eed8-489a-abc8-1145f9698542")[[2]] |>
  mutate(year = "2020")

cga2019 <- get_resource("9b319faa-5364-4695-b139-fcfd118d0353")[[1]] |>
  mutate(year = "2019")
cga2019_legend <- get_resource("9b319faa-5364-4695-b139-fcfd118d0353")[[2]] |>
  mutate(year = "2019")

cga2018 <- get_resource("68a3716d-600b-4013-8f2d-76811c95ce1f")[[1]] |>
  mutate(year = "2018")
cga2018_legend <- get_resource("68a3716d-600b-4013-8f2d-76811c95ce1f")[[2]] |>
  mutate(year = "2018")

cga2017 <- get_resource("dd23b1ae-49af-433b-a175-807fa05f91cc") |>
  mutate(year = "2017")
cga2016 <- get_resource("dc7a58ef-6f97-4366-b351-0ab60e18cada") |>
  mutate(year = "2016")
cga2015 <- get_resource("5b636b08-7554-4cfc-906d-c3adf928710b") |>
  mutate(year="2015")
cga2014 <- get_resource("8cb7edd4-57fd-4a98-95b1-f0363be83e6d") |>
  mutate(year="2014")
cga2013 <- get_resource("3313bed4-7d36-4ad0-abc2-81dbb75e8968") |>
  mutate(year="2013")
cga2012 <- get_resource("f4170149-155a-4538-a8f5-4276022ea87a") |>
  mutate(year="2012")
cga2011 <- get_resource("d5a45d76-f684-4d57-b4e0-c0bf06e0e97a") |>
  mutate(year="2011")
cga2010 <- get_resource("9a28c0bd-f206-4f4b-b002-0ff272750101") |>
  mutate(year="2010")
readme <- get_resource("f13c55c1-fe1c-46fd-9f53-266fa208d125")
grants_since_2022 <- get_resource("e8ae0064-43f8-4cb4-988f-41336f2f3c06")
cga2022_to_2025 <- get_resource("866f95c3-49ac-496e-91ee-1cbffd662cdb")

#### Save the datasets into the folder ####
write.csv(wards_ref, file = "data/01-raw_data/wards_ref.csv", row.names = FALSE)
write.csv(cga2021, file = "data/01-raw_data/raw_data_2021.csv", row.names = FALSE) # Only save the first sheet
write.csv(cga2020, file = "data/01-raw_data/raw_data_2020.csv", row.names = FALSE)
write.csv(cga2020_addon, file = "data/01-raw_data/raw_data_2020_addon.csv", row.names = FALSE)
write.csv(cga2019, file = "data/01-raw_data/raw_data_2019.csv", row.names = FALSE)
write.csv(cga2018, file = "data/01-raw_data/raw_data_2018.csv", row.names = FALSE)
write.csv(cga2017, file = "data/01-raw_data/raw_data_2017.csv", row.names = FALSE)
write.csv(cga2016, file = "data/01-raw_data/raw_data_2016.csv", row.names = FALSE)
write.csv(cga2015, file = "data/01-raw_data/raw_data_2015.csv", row.names = FALSE)
write.csv(cga2014, file = "data/01-raw_data/raw_data_2014.csv", row.names = FALSE)
write.csv(cga2013, file = "data/01-raw_data/raw_data_2013.csv", row.names = FALSE)
write.csv(cga2012, file = "data/01-raw_data/raw_data_2012.csv", row.names = FALSE)
write.csv(cga2011, file = "data/01-raw_data/raw_data_2011.csv", row.names = FALSE)
write.csv(cga2010, file = "data/01-raw_data/raw_data_2010.csv", row.names = FALSE)
write.csv(cga2022_to_2025, file = "data/01-raw_data/raw_data_2022_to_2025.csv", row.names = FALSE)
write.csv(grants_since_2022, file = "data/01-raw_data/raw_allocations_since_2022.csv", row.names = FALSE)
write.csv(readme, file = "data/01-raw_data/raw_readme.csv", row.names = FALSE)

write.csv(cga2021_legend, file = "data/01-raw_data/raw_legend_2021.csv", row.names = FALSE)
write.csv(cga2020_legend, file = "data/01-raw_data/raw_legend_2020.csv", row.names = FALSE)
write.csv(cga2019_legend, file = "data/01-raw_data/raw_legend_2019.csv", row.names = FALSE)
write.csv(cga2018_legend, file = "data/01-raw_data/raw_legend_2018.csv", row.names = FALSE)

