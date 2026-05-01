# =========================
# PACKAGES
# =========================

required_packages <- c("tidyverse", "readxl", "readr", "DT")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) install.packages(pkg)
}
library(tidyverse)
library(readxl)
library(readr)
library(DT)



# =========================
# FARE REVENUE
# =========================

if (!file.exists("2022_fare_revenue.xlsx")) {
  download.file(
    "http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx",
    destfile = "2022_fare_revenue.xlsx",
    quiet = FALSE
  )
}

FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(
    -`State/Parent NTD ID`,
    -`Reporter Type`,
    -`Reporting Module`,
    -`TOS`,
    -`Passenger Paid Fares`,
    -`Organization Paid Fares`
  ) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`, `Agency Name`, `Mode`) |>
  summarize(`Total Fares` = sum(`Total Fares`), .groups = "drop")

# Aggregates fare revenue by agency and mode
# Collapses direct-operated and subcontracted services
# Removes unnecessary reporting metadata



# =========================
# EXPENSES
# =========================

if (!file.exists("2022_expenses.csv")) {
  download.file(
    "https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true",
    destfile = "2022_expenses.csv",
    quiet = FALSE
  )
}

EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, `Agency`, `Total`, `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses), .groups = "drop")

# Standardizes NTD ID format for joins
# Aggregates expenses by agency mode combination



# =========================
# COMBINE FINANCIALS
# =========================

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

# Merges fare revenue and expense datasets
# Uses NTD ID and Mode as the join keys



# =========================
# MONTHLY TRANSIT DATA
# =========================

if (!file.exists("ridership.xlsx")) {
  download.file(
    "https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx",
    destfile = "ridership.xlsx",
    quiet = FALSE
  )
}



# =========================
# TRIPS (UPT)
# =========================

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet = "UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(
    -`Legacy NTD ID`,
    -`Reporter Type`,
    -`Mode/Type of Service Status`,
    -`UACE CD`,
    -`TOS`
  ) |>
  pivot_longer(
    -c(`NTD ID`:`3 Mode`),
    names_to = "month",
    values_to = "UPT"
  ) |>
  drop_na() |>
  mutate(month = my(month))

# Filters active transit services only
# Reshapes wide monthly data into long format
# Converts month strings into date format



# =========================
# VEHICLE REVENUE MILES (VRM)
# =========================

MILES <- readxl::read_xlsx("ridership.xlsx", sheet = "VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(
    -`Legacy NTD ID`,
    -`Reporter Type`,
    -`Mode/Type of Service Status`,
    -`UACE CD`,
    -`TOS`
  ) |>
  pivot_longer(
    -c(`NTD ID`:`3 Mode`),
    names_to = "month",
    values_to = "VRM"
  ) |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM), .groups = "drop") |>
  mutate(month = my(month))

# Standardizes structure across agencies and modes
# Aggregates mileage data across reporting dimensions
# Converts monthly fields into usable time format



# =========================
# COMBINE USAGE DATA
# =========================

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

# Merges ridership and mileage datasets
# Ensures consistent ID formatting for downstream joins



# =========================
# SAMPLE OUTPUT
# =========================

sample_n(USAGE, 1000) |>
  mutate(month = as.character(month)) |>
  DT::datatable()

# Displays random sample of usage data
# Formats month column for better table readability





# =========================
# TASK 1 - Creating Syntactic Names
# =========================

USAGE <- USAGE |>
  rename(metro_area = "UZA Name")

# Renaming the column "UZA Name" to "metro_area" for 
# easier and syntactically valid referencing in R.





# =========================
# TASK 2 - Recoding the Mode Column
# =========================

USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "DR" ~ "Demand Response",   
    Mode == "FB" ~ "Ferryboat",   
    Mode == "MB" ~ "Bus",   
    Mode == "SR" ~ "Streetcar Rail",   
    Mode == "TB" ~ "Trolleybus",   
    Mode == "VP" ~ "Vanpool",   
    Mode == "CB" ~ "Commuter Bus",   
    Mode == "RB" ~ "Bus Rapid Transit",   
    Mode == "LR" ~ "Light Rail",   
    Mode == "YR" ~ "Hybrid Rail",   
    Mode == "MG" ~ "Monorail Automated Guideway",   
    Mode == "CR" ~ "Commuter Rail",   
    Mode == "AR" ~ "Alaska Railroad",   
    Mode == "TR" ~ "Aerial Tramway",   
    Mode == "HR" ~ "Heavy Rail",   
    Mode == "IP" ~ "Inclined Plane",   
    Mode == "PB" ~ "Publico",   
    Mode == "CC" ~ "Cable Car",   
    TRUE ~ "Unknown"))

if(!require("DT")) install.packages("DT")
library(DT)
library(dplyr)

sample_n(USAGE |> select(-"NTD ID", -"3 Mode") |> rename(trips = "UPT", miles = "VRM"), 1000) |> 
  mutate(month = as.character(month)) |> 
  DT::datatable()

# Recodes the shorthand Mode column into full, descriptive transit mode names.
# Removes NTD ID and 3 Mode columns, renames UPT to trips and VRM to miles.
# Samples 1000 rows, converts month to character datatype, and displays the data 
# interactively in a datatable.





# =========================
# TASK 3 - Answering Questions with DPLYR
# =========================

#What transit agency had the most VRM?
USAGE %>% group_by(Agency) %>%
  summarize(max(VRM)) %>%
  arrange(desc(`max(VRM)`))
#After grouping the data by agency, calculating the max VRM values, and arranging
#the VRM values in descending order, the transit agency with the most VRM is MTA
#New York City Transit. 

#What transit mode had the most total VRM in our data set?
USAGE %>% group_by(Mode) %>%
  summarize(max(VRM)) %>%
  arrange(desc(`max(VRM)`))
#After grouping the data by the mode of transportation, calculating the max VRM values of each mode, 
#and arranging the VRM values in descending order, the transit mode with the most VRM in the dataset 
#is Heavy Rail; or more colloquially known as mass rapid transit. 

#How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
USAGE %>% filter(Agency == "MTA New York City Transit", month == "2024-05-01", Mode == "Heavy Rail")
#To determine how many Heavy Rail trips were taken on the NYC subway in May 2024, the filter function
#with three parameters is used: the first parameter ensures the data displayed is solely from the MTA NYC Transit,
#the second parameter ensures that of the MTA NYC Transit data, only datapoints from the month of May are displayed,
#the third parameter filters out only the transit trips from May that are completed via heavy rail.  

#What mode of transport had the longest average trip in May 2024?
#N/A 

#How much did NYC subway ridership fall between April 2019 and April 2020?
Ridership_April19 <- USAGE %>%
  filter(month == "2019-04-01", Agency == "MTA New York City Transit", Mode == "Heavy Rail") %>%
  summarize(Ridership_April19 = sum(UPT, na.rm = TRUE))
Ridership_April20 <- USAGE %>%
  filter(month == "2020-04-01", Agency == "MTA New York City Transit", Mode == "Heavy Rail") %>%
  summarize(Ridership_May24 = sum(UPT, na.rm = TRUE))
Ridership_Change <- (Ridership_April19 - Ridership_April20)
Ridership_Change
#To determine how much subway ridership in NYC fell between April 2019 and April 2020, we first use the
#filter function to find the requested values and store them in two variables. We then subtract those two variables and
#store their result in a new variable, which we then run to receive a value of 211,969,660.


