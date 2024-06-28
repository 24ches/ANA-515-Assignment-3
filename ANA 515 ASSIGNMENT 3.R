# check working directory
getwd()

# Loading and calling the necessary libraries for reading csv
library(readr)
library(tidyverse)
library(dplyr)

# Define the file name
file_name <- "StormEvents_details-ftp_v1.0_d1998_c20220425.csv"

# read the data from the csv file.
storm_data <- read.csv(file_name)

# Display the first few rows of the dataframe
head(storm_data, 6)

# Limit the dataframe to specific columns
storm_data <- storm_data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

# Arrange the data by state name (STATE)
storm_data <- storm_data %>%
  arrange(STATE)

# Change state and county names to title case
storm_data <- storm_data %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))

# Limit to events listed by county FIPS (CZ_TYPE of "C") and then remove the CZ_TYPE column
storm_data <- storm_data %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

# Pad the state and county FIPS with a “0” at the beginning and unite the two columns
storm_data <- storm_data %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 3, pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0"),
         FIPS = str_c(STATE_FIPS, CZ_FIPS, sep = "")) %>%
  select(-STATE_FIPS, -CZ_FIPS)
# Change all the column names to lower case
storm_data <- storm_data %>%
  rename_all(tolower)
# Load base R state data
data("state")

# Create a dataframe with state name, area, and region
state_info <- data.frame(state_name = state.name,
                         area = state.area,
                         region = state.region)

# Create a dataframe with the number of events per state
events_per_state <- storm_data %>%
  group_by(state) %>%
  summarise(events = n())

# Merge with the state information dataframe
final_data <- merge(events_per_state, state_info, by.x = "state", by.y = "state_name")

# Remove any states that are not in the state information dataframe
final_data <- final_data %>%
  filter(!is.na(area) & !is.na(region))

# Display the first few rows of the dataframe to verify
head(final_data, 6)

# Load ggplot2 for plotting
library(ggplot2)

# Create the scatter plot
ggplot(final_data, aes(x = area, y = events, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "# of Storm Events in 1998",
       x = "Land area (square miles)",
       y = "# of storm events") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
