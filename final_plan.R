# PLAN 372 Final

# Set workspace
setwd("/Users/danielleowens/Downloads")
# Read the CSV file
debt_data <- read.csv("debts_counties.csv", header = TRUE)

# Libraries
library(usmap)
library(tidyverse)

# Making vector of GEOIDs
geoids <- debt_data$GEOID

# In order to make FIPS codes for the usmap package, have to transform GEOIDs
# Code from: http://students.washington.edu/ayandm/tutfiles/FIPSConversion.pdf
# Extract state and county codes from GEOIDs
state_codes <- substr(geoids, 1, 2)
county_codes <- substr(geoids, 3, 5)

# Link state and county codes to form FIPS codes
fips <- paste(state_codes, county_codes, sep = "")

# Add to data
debt_data$fips <- fips

# Plot the data on a US map using the county column

# Code and documentation used: https://usmap.dev/
plot_usmap(data = debt_data, values = "debt") +
  scale_fill_continuous(name = "Debt", 
                        label = scales::comma, 
                        low = "white", high = "darkblue") +
  ggtitle("Debt in America by County")

# Plot the data on a US map using the state column
library(dplyr)

debt_data <- debt_data %>% rename(state = state_name)

debt_data_by_state <- aggregate(debt ~ state, data = debt_data, mean)

plot_usmap(data = debt_data_by_state, values = "debt") +
  scale_fill_continuous(name = "Debt", 
                        label = scales::comma, 
                        low = "white", high = "darkblue") +
  ggtitle("Debt in America by State")


# ANOVA analysis of any debt between regions

library(dplyr)
# Add regions
# create new column with region names
debt_data <- debt_data %>%
  mutate(region = case_when(state_name %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont") ~ "Northeast",
                            state_name %in% c("Arizona", "New Mexico", "Oklahoma", "Texas") ~ "Southwest",
                            state_name %in% c("Alaska", "California", "Colorado", "Hawaii", "Idaho", "Montana", "Nevada", "Oregon", "Utah", "Washington", "Wyoming") ~ "West",
                            state_name %in% c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Virginia", "West Virginia") ~ "Southeast",
                            state_name %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin") ~ "Midwest",
                            TRUE ~ NA_character_))

# Create bar chart of average debt by region

# Group data by state and region
states <- group_by(debt_data, state_name) %>%
  summarise(mean_value = mean(debt))
  # Add regions 
states$region <- c("Southeast", "West", "West", "Southeast", "West", "West", "Northeast", "Northeast", "Southeast", "Southeast", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "Southeast", "Southeast", "Northeast", "Northeast", "Northeast", "Midwest", "Midwest", "Southeast", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "Southeast", "Midwest", "Southwest", "West", "Northeast", "Northeast", "Southeast", "Midwest", "Northeast", "Southeast", "Midwest", "Southeast", "Southwest", "West", "Northeast", "West", "Southeast", "Midwest", "West")
grouped_regions <- group_by(states, region) %>%
  summarise(mean_debt = mean(debt))

# Plot of region average
ggplot(states, aes(x = region, y = mean_value, fill = "pink")) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Average Debt by Region") +
  labs(x = "Region", y = "Mean Debt") +
  theme(plot.title = element_text(hjust = 0.5))

# Perform the ANOVA analysis by region
debt_model <- aov(debt ~ region, data = debt_data)
summary(debt_model)
# Post-hoc tukey test
region_tukey <- TukeyHSD(debt_model)
region_tukey

# Linear regression with other debt variables 
# Independent:any_debt, medical_debt, student_debt, auto_retail_debt, credit_card_debt, state_name
# Dependent: Median debt
# Can the proportion of debt tell us anything about the median debt?

# Load data in
full_data <- read.csv("full_county.csv", header = TRUE)

# Create linear regression model
debt_model <- lm(median_debt ~ any_debt+medical_debt+student_debt+auto_retail_debt+credit_card_debt+state_name , data = full_data)
summary(debt_model)
anova(debt_model)

# select the variables you want to plot
vars <- c("median_debt", "any_debt", "medical_debt", "student_debt", "auto_retail_debt", "credit_card_debt")

# create a scatter plot matrix
pairs(full_data[vars])

