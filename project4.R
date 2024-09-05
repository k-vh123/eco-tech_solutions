# Wildlife Data Analysis Project Setup
# Eco-Tech Solutions: Analyzing Wildlife Conservation Efforts and Their Economic Impact

# Install required packages if not already installed
packages <- c("tidyverse", "lubridate", "leaflet", "plotly")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)

# Set seed for reproducibility
set.seed(123)

# Create sample wildlife population data
create_wildlife_data <- function(n_years = 10, n_species = 10) {
  data.frame(
    date = seq(as.Date("2015-01-01"), by = "year", length.out = n_years),
    matrix(
      cumsum(matrix(rnorm(n_years * n_species, mean = 30, sd = 5), 
                    nrow = n_years, ncol = n_species)),
      nrow = n_years,
      dimnames = list(NULL, paste0("species", 1:n_species))
    )
  )
}

wildlife_data <- create_wildlife_data()

# Create sample economic data
create_economic_data <- function(n_years = 10) {
  data.frame(
    date = seq(as.Date("2015-01-01"), by = "year", length.out = n_years),
    gdp_growth = cumsum(rnorm(n_years, mean = 2, sd = 0.5)),
    unemployment_rate = 5 + cumsum(rnorm(n_years, mean = -0.2, sd = 0.1)),
    tourism_revenue = cumsum(rnorm(n_years, mean = 5000000, sd = 1000000)),
    conservation_budget = cumsum(rnorm(n_years, mean = 1000000, sd = 200000)),
    local_business_count = cumsum(rpois(n_years, lambda = 5)),
    eco_friendly_initiatives = cumsum(rpois(n_years, lambda = 2)),
    green_job_growth = cumsum(rnorm(n_years, mean = 1.5, sd = 0.3)),
    renewable_energy_adoption = cumsum(rbeta(n_years, shape1 = 2, shape2 = 5)),
    water_quality_index = 70 + cumsum(rnorm(n_years, mean = 0.5, sd = 0.1)),
    air_quality_index = 80 + cumsum(rnorm(n_years, mean = 0.3, sd = 0.1))
  )
}

economic_data <- create_economic_data()

# Create sample conservation technology data
create_tech_data <- function(n_techs = 10, n_years = 10) {
  expand.grid(
    technology = paste0("Tech", 1:n_techs),
    year = 2015:(2015 + n_years - 1)
  ) %>%
    mutate(
      effectiveness = runif(n(), 0.6, 0.9),
      cost = runif(n(), 100000, 500000),
      adoption_rate = runif(n(), 0.1, 0.8),
      energy_efficiency = runif(n(), 0.7, 0.95),
      data_accuracy = runif(n(), 0.8, 0.99),
      maintenance_required = sample(c("Low", "Medium", "High"), n(), replace = TRUE, prob = c(0.3, 0.5, 0.2)),
      user_satisfaction = runif(n(), 3, 5),
      environmental_impact = runif(n(), -0.2, 0.1)
    )
}

tech_data <- create_tech_data()

# Basic visualization of wildlife population trends
ggplot(wildlife_data %>% pivot_longer(cols = starts_with("species"), names_to = "species", values_to = "population")) +
  geom_line(aes(x = date, y = population, color = species)) +
  labs(title = "Wildlife Population Trends", x = "Year", y = "Population") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Basic visualization of economic trends
economic_data_long <- economic_data %>% 
  pivot_longer(cols = -date, names_to = "indicator", values_to = "value")

ggplot(economic_data_long, aes(x = date, y = value, color = indicator)) +
  geom_line() +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  labs(title = "Economic Indicators Over Time", x = "Year", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Function to perform basic analysis
analyze_wildlife_economy <- function(wildlife_data, economic_data) {
  # Combine datasets
  combined_data <- wildlife_data %>%
    left_join(economic_data, by = "date")
  
  # Calculate correlations
  cor_matrix <- cor(combined_data[, -1])
  
  # Print correlations
  print("Correlation matrix:")
  print(cor_matrix)
}

# Run basic analysis
analyze_wildlife_economy(wildlife_data, economic_data)


