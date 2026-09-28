#### Preamble ####
# Purpose: Generates plots from cleaned dataset of Toronto's grant allocations.
# Author: Maggie Huang
# Date: 28 September 2026
# Contact: maggieh.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download, generate reference tables, clean, and validate data

#### Workspace setup ####
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(dplyr)
library(scales)

data <- read.csv("data/02-analysis_data/clean_data.csv")

#### Reference Tables ####
pc_low_income_ref <- data.frame(
  ward = c(13, 18, 11, 7, 5, 22, 24, 16, 17, 10, 20, 21, 1, 15, 12, 
    4, 19, 23, 6, 14, 3, 9, 8, 2, 25),
  pc = c(22.20, 17.80, 15.30, 15.10, 14.70, 14.70, 14.70, 14.40, 14.40, 14.10, 
    13.90, 13.30, 12.90, 12.60, 12.50, 12.00, 11.90, 11.80, 11.50, 11.50, 
    11.00, 10.80, 10.10, 7.90, 7.80))
length(pc_low_income_ref$pc)

#### Chart 1: Examining total disbursements year over year ####

# Since the data is already in long format, get the yearly totals by summarizing the data
yearly_totals <- data |>
  group_by(year) |>
  summarise(Original = sum(grant_amount),
            Adjusted = sum(infl_adj_funding)
  )

ggplot(yearly_totals, aes(x=year)) +
  geom_line(aes(y=Original, color = "Nominal Amount"), linewidth = 1) +
  geom_line(aes(y=Adjusted, color = "Real Amount, 2025 Dollars"), linewidth = 1) +
  geom_point(aes(y=Original, color = "Nominal Amount")) +
  geom_point(aes(y=Adjusted, color = "Real Amount, 2025 Dollars"))+
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Total Grant Disbursements to Toronto Nonprofits by Year",
       x = "Year",
       y = "Grant funding disbursed (CAD)",
       color = "") +
  theme_minimal()

#ggsave("other/charts/total_by_year.jpg")

#### Chart 2: Total Grant Disbursments by Ward ####

# Add back space in ward names for labels in charts
data <- data |> mutate(Ward.Name = str_replace_all(Ward.Name, "([a-z])([A-Z])", "\\1 \\2"))

# Create summary table of total disbursements in nominal dollars by ward
ward_top10_funds <- data |>
  mutate(Full.Name.Final = fct_lump_n(Full.Name.Final, n = 10, w = infl_adj_funding, other_level = "Other Funds")) |>
  group_by(Ward.Name, Full.Name.Final) |>
  summarise(Total_Real = sum(infl_adj_funding, na.rm = TRUE), .groups = "drop")

# Create stacked bar chart
ggplot(ward_top10_funds, aes(x = Ward.Name, y = Total_Real, fill = Full.Name.Final)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set3") + 
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Top 10 Grant Programs by Ward (Real Total, 2010-2025)",
    x = "Ward",
    y = "Amount (CAD)",
    fill = "Grant Program"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#ggsave("other/charts/grant_disbursement_by_ward.jpg")

#### Chart 3: Grant Disbursements to Top 5 low-income wards vs. high-income ####
ward_low <- data |> 
  left_join(pc_low_income_ref, by = c("ward_number" = "ward")) |>
  group_by(year, ward_number, Ward.Name, pc) |>
  summarise(Nominal = sum(grant_amount),
            Real = sum(infl_adj_funding),
            .groups = "drop") |>
  filter(ward_number %in% pc_low_income_ref$ward[c(1:5, 20:24)]) |>
  mutate(group = if_else(ward_number %in% pc_low_income_ref$ward[1:5], "Top 5 Highest % of Low-Income Residents", 
                         "Top 5 Lowest % of Low-Income Residents"))

ggplot(ward_low, aes(x = year, group = group, color = group)) +
  stat_summary(aes(y = Nominal), fun = mean, geom = "line", size = 1) +
  stat_summary(aes(y = Nominal), fun = mean, geom = "point", size = 2) +
  stat_summary(aes(y = Real), fun = mean, geom = "line", size = 1, linetype = "dashed") +
  stat_summary(aes(y = Real), fun = mean, geom = "point", size = 2, shape = 17) +
  scale_y_continuous(labels = scales::label_comma()) +
  # labels and formatting
  labs(
    title = "Real (Dashed) vs. Nominal (Solid) Total Grant Disbursements by Year",
    subtitle = "Grouped by Top 5 Wards with Highest vs. Lowest % of Low-Income Residents",
    x = "Year",
    y = "Amount (CAD)",
    color = "Group"
  ) +
  theme_minimal()

#ggsave("other/charts/grant_disbursement_top5.jpg")

#### Chart 4: Examining disbursements by top 10 nonprofits ####
org_totals <- data |> group_by(organization) |>
  summarise(real_total = sum(infl_adj_funding)) |> arrange(desc(real_total)) |>
  slice(1:10)

ggplot(org_totals, aes(x=organization, y = real_total)) +
  geom_col(fill = "royalblue") +
  scale_y_continuous(labels = scales::label_comma()) +
  # labels and formatting
  labs(
    title = "Top 10 Total Grant Disbursements by Nonprofit in Toronto adjusted for Inflation, Base Year 2025",
    x = "Nonprofit Organization",
    y = "Amount (CAD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust = 1)
  )

#ggsave("other/charts/top_10_nonprofits.jpg")

#### Chart 5: Examining proportion of funding top 10 programs occupy per year ####
# There are many grant programs. Focus on the top 10 and group other funds into "Other Funds" category
year_fund_prop <- data |>
  mutate(Full.Name.Final = fct_lump_n(Full.Name.Final, n = 10, 
                                      w = infl_adj_funding, other_level = "Other Funds")) |>
  group_by(year, Full.Name.Final) |>
  summarise(Total_Real = sum(infl_adj_funding, na.rm = TRUE), .groups = "drop")

# Create the stacked bar chart.
ggplot(year_fund_prop, aes(x = factor(year), y = Total_Real, fill = Full.Name.Final)) +
  geom_col(position = "fill") + # normalize to 100%
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) + # turn y-axis into %
  labs(
    title = "Proportional Composition of Grant Programs by Year",
    x = "Year",
    y = "Percentage of Funding (%)",
    fill = "Grant Program"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#ggsave("other/charts/prop_composition.jpg")


#### Chart 5: Examining proportion of funding top 10 programs occupy per year ####
# There are many grant programs. Focus on the top 10 and group other funds into "Other Funds" category
ward_fund_prop <- data |>
  mutate(Ward.Name = fct_lump_n(Ward.Name, n = 10, 
                                      w = infl_adj_funding, other_level = "Other Wards")) |>
  group_by(year, Ward.Name) |>
  summarise(Total_Real = sum(infl_adj_funding, na.rm = TRUE), .groups = "drop")

# Create the stacked bar chart.
ggplot(ward_fund_prop, aes(x = factor(year), y = Total_Real, fill = Ward.Name)) +
  geom_col(position = "fill") + # normalize to 100%
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) + # turn y-axis into %
  labs(
    title = "Proportional Composition of Annual Grant Disbursement by Ward",
    x = "Year",
    y = "Percentage of Funding (%)",
    fill = "Ward"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#ggsave("other/charts/prop_composition_ward.jpg")
