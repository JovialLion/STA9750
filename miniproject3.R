library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(knitr)
library(tidyverse)
library(ggplot2)





### TASK 1: Download Congressional Shapefiles 1976-2012 ###

# Base URL for downloading congressional shapefiles
base_url <- "https://github.com/JeffreyBLewis/congressional-district-boundaries"

# Define the range of congresses (from 1976 to 2012)
congresses <- 94:113

# Define the directory where the shapefiles will be saved
download_dir <- "congress_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# Function to download the shapefile if not already downloaded
download_shapefile <- function(congress, base_url, download_dir) {
  # Format the Congress number ("94th", "95th", etc.)
  congress_str <- paste0(congress, "th")
  # Construct the URL for the specific shapefile (adjust naming convention as needed)
  file_url <- paste0(base_url, congress_str, "/shapefile.zip")
  # Define the local filename for the zip file
  zip_file <- file.path(download_dir, paste0(congress_str, "_shapefile.zip"))
  # Check if the file already exists
  if (!file.exists(zip_file)) {
    message(paste("Downloading:", congress_str))
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
  } else {
    message(paste("File for Congress", congress_str, "already exists. Skipping download."))
  }
}

# Loop through all Congresses from 94th to 113th
for (congress in congresses) {
  download_shapefile(congress, base_url, download_dir)
}

# Optional: After download, unzip all shapefiles if necessary
unzip_shapefiles <- function(download_dir) {
  zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)
  for (zip_file in zip_files) {
    # Define the target folder for extraction
    unzip_dir <- gsub("\\.zip$", "", zip_file)
    if (!dir.exists(unzip_dir)) dir.create(unzip_dir)
    # Unzip the file
    unzip(zip_file, exdir = unzip_dir)
  }
}

# Optional: After downloading, uncomment the following line to unzip files
# unzip_shapefiles(download_dir)

message("Download process completed!")





### TASK 2: Download Congressional Shapefiles 2014-2022 ###

# Base URL for the U.S. Census Bureau Congressional Shapefiles
base_url <- "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html"

# Define the range of congresses (113th to 117th Congresses correspond to years 2014-2022)
congresses <- 113:117  # From 113th Congress (2013-2015) to 117th Congress (2021-2023)

# Define the directory where the shapefiles will be saved
download_dir <- "congress_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# Function to download the shapefile if not already downloaded
download_shapefile <- function(congress, base_url, download_dir) {
  # Format the Congress number (e.g., "113th", "114th", etc.)
  congress_str <- paste0(congress, "th")
  # Construct the URL for the specific shapefile (adjust naming convention as needed)
  file_url <- paste0(base_url, "congressional", congress_str, "/shapefile.zip")
  # Define the local filename for the zip file
  zip_file <- file.path(download_dir, paste0("congress_", congress_str, "_shapefile.zip"))
  # Check if the file already exists
  if (!file.exists(zip_file)) {
    message(paste("Downloading:", congress_str))
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
  } else {
    message(paste("File for Congress", congress_str, "already exists. Skipping download."))
  }
  return(zip_file)
}

# Function to unzip the downloaded shapefiles with error handling
unzip_shapefiles <- function(zip_file) {
  if (file.exists(zip_file)) {
    unzip_dir <- gsub("\\.zip$", "", zip_file)  # Remove .zip extension for folder name
    # Check if the directory already exists
    if (!dir.exists(unzip_dir)) {
      dir.create(unzip_dir)  # Create the directory for extraction
    }
    # Try unzipping the file, and catch any errors
    tryCatch({
      message(paste("Unzipping:", zip_file))
      unzip(zip_file, exdir = unzip_dir)
      message(paste("Unzipped successfully:", zip_file))
    }, error = function(e) {
      warning(paste("Failed to unzip:", zip_file, "\nError message:", e$message))
    })
  } else {
    warning(paste("The file does not exist:", zip_file))
  }
}

# Loop through all Congresses from 113th to 117th
for (congress in congresses) {
  zip_file <- download_shapefile(congress, base_url, download_dir)
  unzip_shapefiles(zip_file)
}

message("Download and unzip process completed!")





### TASK 3: Exploration of Vote Count Data ###

library(readr)
housevotes1976to2022 <- read_csv("dataverse_files/1976-2022-house.csv")
library(readr)
presidentvotes1976to2020 <- read_csv("dataverse_files2/1976-2020-president.csv")

### States that have gained and lost the most seats in the US House of Representatives between 1976 and 2022. ###

statedistricts_1976to2022 <- housevotes1976to2022 %>%
  group_by(year, state) %>%
  summarise(num_districts = n_distinct(district))
statedistricts_change <- statedistricts_1976to2022 %>%
  spread(key = year, value = num_districts) %>%
  rename(districts_1976 = `1976`, districts_2022 = `2022`) %>%
  mutate(districts_change = districts_2022 - districts_1976) %>%
  arrange(desc(districts_change))
kable(statedistricts_change)

### Elections in our data where the election would have had a different outcome if the “fusion” system was not used and candidates only received the votes from their “major party line” (Democrat or Republican). ###

newyorkvotes_1976to2022 <- housevotes1976to2022 %>%
  group_by(year, state) %>%
  filter(state == "NEW YORK")

nycandidatevotes_total <- newyorkvotes_1976to2022 %>%
  group_by(candidate, year) %>%
  mutate(candidatetotal = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup()
nycandidatevotes_winner <- nycandidatevotes_total %>%
  group_by(year, district) %>%
  slice_max(candidatetotal, n = 1) %>%
  select(year, candidate, candidatetotal) %>%
  distinct(candidate, .keep_all = TRUE)
altnycandidatevotes_total <- newyorkvotes_1976to2022 %>%
  group_by(candidate, year) %>%
  mutate(candidatetotal = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN")
altnycandidatevotes_winner <- nycandidatevotes_total %>%
  group_by(year, district) %>%
  slice_max(candidatevotes, n = 1) %>%
  select(year, candidate, candidatevotes)

if (any(nycandidatevotes_winner$candidate != altnycandidatevotes_winner$candidate)) {
  print("There are elections that would have had a different outcome.")
} else {
  print("There are not elections that would have had a different outcome.")
}

### Do presidential candidates run ahead or run behind congressional candidates? ###

congressionalpartyvotes <- housevotes1976to2022 %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  group_by(year, party) %>%
  summarize(total_votes_congressional = sum(candidatevotes), .groups = 'drop')
presidentialpartyvotes <- presidentvotes1976to2020 %>%
  rename("party" = "party_detailed") %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  group_by(year, party) %>%
  summarize(total_votes_presidential = sum(candidatevotes), .groups = 'drop')
congressionalandpresidential <- merge(congressionalpartyvotes, presidentialpartyvotes, by = c("year", "party"), all.x = TRUE)
congressionalandpresidential$iscongresshigher <- congressionalandpresidential$total_votes_congressional > congressionalandpresidential$total_votes_presidential
kable(congressionalandpresidential)





### TASK 4: Automate Zip File Extraction ###

library(ggplot2)
library(sf)

if(!file.exists("nyc_borough_boundaries.zip")){
  download.file("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile", 
                destfile="nyc_borough_boundaries.zip")
}

# Define the function to read .shp file from a zip archive
read_shp_from_zip <- function(zip_file) {
  # Create a temporary directory
  td <- tempdir(); 
  # Extract the contents of the zip file
  zip_contents <- unzip("nyc_borough_boundaries.zip", 
                        exdir = td)
  # Identify the .shp file among the extracted contents
  fname_shp <- zip_contents[grepl("shp$", zip_contents)]
  # Read the shapefile into R using st_read
  nyc_sf <- st_read(fname_shp)
  return(nyc_sf)
}

nyc_sf <- read_shp_from_zip("nyc_borough_boundaries.zip")

ggplot(nyc_sf, 
       aes(geometry=geometry,
           fill = shape_area)) + 
  geom_sf()





### TASK 5: Chloropleth Visualization of the 2000 Presidential Election Electoral College Results ###

library(ggplot2)
library(sf)

if(!file.exists("state_boundaries.zip")){
  download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip", 
                destfile="state_boundaries.zip")
}

# Define the function to read .shp file from a zip archive
read_shp_from_zip <- function(zip_file) {
  # Create a temporary directory
  td <- tempdir(); 
  # Extract the contents of the zip file
  zip_contents <- unzip("state_boundaries.zip", 
                        exdir = td)
  # Identify the .shp file among the extracted contents
  fname_shp <- zip_contents[grepl("shp$", zip_contents)]
  # Read the shapefile into R using st_read
  states_sf <- st_read(fname_shp)
  return(states_sf)
}

states_sf <- read_shp_from_zip("state_boundaries.zip")

# Reshape the wholistic dataset with an epmhasis on the 2000 election
presidentvotes2000_summary <- presidentvotes1976to2020 %>%
  filter(year == 2000, party_detailed %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(state, party_detailed) %>%
  summarise(candidatevotes = sum(candidatevotes), .groups = "drop") %>%
  pivot_wider(names_from = party_detailed, values_from = candidatevotes, values_fill = list(candidatevotes = 0)) %>%
  rename(Democratic_votes = DEMOCRAT, Republican_votes = REPUBLICAN) %>%
  left_join(
    presidentvotes1976to2020 %>%
      filter(year == 2000) %>%
      select(state, totalvotes) %>%
      distinct(),
    by = "state"
  ) %>%
  mutate(
    Winner = case_when(
      Democratic_votes > Republican_votes ~ "DEMOCRAT",
      Republican_votes > Democratic_votes ~ "REPUBLICAN",
      TRUE ~ "TIE"
    )
  ) %>%
  select(state, Democratic_votes, Republican_votes, Total_votes = totalvotes, Winner)

# Merge the shapefiles and presidential election data
states_sf <- states_sf %>%
  rename("state" = "NAME")
states_sf$state <- toupper(states_sf$state)
states_sf <- states_sf %>%
  left_join(presidentvotes2000_summary, by = c("state"))

# Plot the presidential election data and make the map window larger
ggplot(data = states_sf) +
  geom_sf(aes(fill = Winner)) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red")) +
  theme_minimal() +
  labs(title = "US Election Results by State (2000)", fill = "Party") +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE)





### TASK 7: Evaluating Fairness of ECV Allocation Schemes ###

# Modify the code to display the 2016 election
presidentvotes2016_summary <- presidentvotes1976to2020 %>%
  filter(year == 2016, party_detailed %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(state, party_detailed) %>%
  summarise(candidatevotes = sum(candidatevotes), .groups = "drop") %>%
  pivot_wider(names_from = party_detailed, values_from = candidatevotes, values_fill = list(candidatevotes = 0)) %>%
  rename(Democratic_votes = DEMOCRAT, Republican_votes = REPUBLICAN) %>%
  left_join(
    presidentvotes1976to2020 %>%
      filter(year == 2016) %>%
      select(state, totalvotes) %>%
      distinct(),
    by = "state"
  ) %>%
  mutate(
    Winner = case_when(
      Democratic_votes > Republican_votes ~ "DEMOCRAT",
      Republican_votes > Democratic_votes ~ "REPUBLICAN",
      TRUE ~ "TIE"
    )
  ) %>%
  select(state, Democratic_votes, Republican_votes, Total_votes = totalvotes, Winner)
kable(presidentvotes2016_summary)
