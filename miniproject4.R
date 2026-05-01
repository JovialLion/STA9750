library(dplyr)
library(httr)
library(knitr)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(lubridate)





### TASK 1: Register for Alpha Vantage API Key ###





### TASK 2: Register for FRED API Key ###





### TASK 3: Data Acquisition ###
## LOAD REQUIRED LIBRARIES ##
library(httr)
library(jsonlite)



## WAGE GROWTH ##
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "NYWTOT"

# Build the URL to get the data for a specific series
url <- paste0(base_url, "series/observations")

# Define the query parameters
params <- list(
  series_id = series_id,        # The series ID
  api_key = api_key,            # The API key
  file_type = "json",           # Specify the file type as JSON
  frequency = "q",              # Data frequency
  units = "lin",                # Linear units
  start_date = "2014-04-01",    # Start date
  end_date = "2024-04-01"       # End date
)

# Make the GET request to the FRED API
response <- GET(url, query = params)

# Check if the request was successful (status code 200 means OK)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  # Extract the observations
  observations <- data$observations
  # Convert to a data frame
  data_wagegrowth <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_wagegrowth)
  # Save the data to a CSV file
  write.csv(data_wagegrowth, "fred_data_wagegrowth.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_wagegrowth.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}



## INFLATION ##
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "CPALTT01USQ657N"

# Build the URL to get the data for a specific series
url <- paste0(base_url, "series/observations")

# Define the query parameters
params <- list(
  series_id = series_id,        # The series ID
  api_key = api_key,            # The API key
  file_type = "json",           # Specify the file type as JSON
  frequency = "q",              # Data frequency
  units = "lin",                # Linear units
  start_date = "2014-04-01",    # Start date
  end_date = "2024-04-01"       # End date
)

# Make the GET request to the FRED API
response <- GET(url, query = params)

# Check if the request was successful (status code 200 means OK)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  # Extract the observations
  observations <- data$observations
  # Convert to a data frame
  data_inflation <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_inflation)
  # Save the data to a CSV file
  write.csv(data_inflation, "fred_data_inflation.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_inflation.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}



## US EQUITY MARKET TOTAL RETURNS ##
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "SPY"  # S&P 500 ETF
function_type <- "TIME_SERIES_MONTHLY"  # Monthly historical data
output_size <- "full"  # Full time series

# Construct the API request URL
url <- paste0("https://www.alphavantage.co/query?function=", function_type,
              "&symbol=", symbol, 
              "&apikey=", api_key, 
              "&outputsize=", output_size)

# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content of the response
  data <- content(response, "parsed", type = "application/json")
  # Extract the historical time series data (if present)
  if (function_type == "TIME_SERIES_MONTHLY") {
    time_series <- data[["Monthly Time Series"]]
    # Convert the time series into a data frame
    if (!is.null(time_series)) {
      # Convert the list to a data frame where each date is a row
      data_sap500 <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_sap500) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_sap500$date <- rownames(data_sap500)
      rownames(data_sap500) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_sap500$date <- as.Date(paste0(data_sap500$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_sap500))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}



## INTL EQUITY MARKET TOTAL RETURNS ##
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "EFA"  # MSCI Developed Markets ETF
function_type <- "TIME_SERIES_MONTHLY"  # Monthly historical data
output_size <- "full"  # Full time series

# Construct the API request URL
url <- paste0("https://www.alphavantage.co/query?function=", function_type,
              "&symbol=", symbol, 
              "&apikey=", api_key, 
              "&outputsize=", output_size)

# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content of the response
  data <- content(response, "parsed", type = "application/json")
  # Extract the historical time series data (if present)
  if (function_type == "TIME_SERIES_MONTHLY") {
    time_series <- data[["Monthly Time Series"]]
    # Convert the time series into a data frame
    if (!is.null(time_series)) {
      # Convert the list to a data frame where each date is a row
      data_msci <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_msci) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_msci$date <- rownames(data_msci)
      rownames(data_msci) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_msci$date <- as.Date(paste0(data_msci$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_msci))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}



## BOND MARKET TOTAL RETURNS ##
# Define the Alpha Vantage API key and base URL
api_key <- "13OR5QPKL3DGT9U6"  # Alpha Vantage API key
symbol <- "AGG"  # Bond Markets ETF
function_type <- "TIME_SERIES_MONTHLY"  # Monthly historical data
output_size <- "full"  # Full time series

# Construct the API request URL
url <- paste0("https://www.alphavantage.co/query?function=", function_type,
              "&symbol=", symbol, 
              "&apikey=", api_key, 
              "&outputsize=", output_size)

# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content of the response
  data <- content(response, "parsed", type = "application/json")
  # Extract the historical time series data (if present)
  if (function_type == "TIME_SERIES_MONTHLY") {
    time_series <- data[["Monthly Time Series"]]
    # Convert the time series into a data frame
    if (!is.null(time_series)) {
      # Convert the list to a data frame where each date is a row
      data_bondreturns <- as.data.frame(do.call(rbind, lapply(time_series, unlist)))
      # Set column names for open, high, low, close, volume
      colnames(data_bondreturns) <- c("open", "high", "low", "close", "volume")
      # Convert the row names to a date column
      data_bondreturns$date <- rownames(data_bondreturns)
      rownames(data_bondreturns) <- NULL  # Remove row names
      # Ensure the date column is in Date format (using the first of each month)
      data_bondreturns$date <- as.Date(paste0(data_bondreturns$date, "-01"))
      # Print out the first few rows of the data frame
      print(head(data_bondreturns))
    } else {
      print("Error: No time series data found.")
    }
  }
} else {
  # If the request was unsuccessful, print the error message
  print(paste("Request failed with status:", status_code(response)))
  print(content(response, "text"))
}



## SHORT-TERM DEBT RETURNS ##
# Define the FRED API endpoint and the FRED API key
api_key <- "ffc31969aa3d7177ca28636e5b65c963"
base_url <- "https://api.stlouisfed.org/fred/"

# Define the FRED series ID for the data you want to download
series_id <- "QFRD304INFUSNO"

# Build the URL to get the data for a specific series
url <- paste0(base_url, "series/observations")

# Define the query parameters
params <- list(
  series_id = series_id,        # The series ID
  api_key = api_key,            # The API key
  file_type = "json",           # Specify the file type as JSON
  frequency = "q",              # Data frequency
  units = "lin",                # Linear units
  start_date = "2014-04-01",    # Start date
  end_date = "2024-04-01"       # End date
)

# Make the GET request to the FRED API
response <- GET(url, query = params)

# Check if the request was successful (status code 200 means OK)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
  # Extract the observations
  observations <- data$observations
  # Convert to a data frame
  data_shortdebts <- as.data.frame(observations)
  # Display the first few rows of the data
  head(data_shortdebts)
  # Save the data to a CSV file
  write.csv(data_shortdebts, "fred_data_shortdebts.csv", row.names = FALSE)
  # If the request succeeds, print a download message
  cat("Data has been successfully downloaded and saved as 'fred_data_shortdebts.csv'.\n")
} else {
  # If the request fails, print an error message
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}





### TASK 4: Initial Analysis ###
# Make sure that the datatypes are in correct format
data_inflation$date <- as.Date(data_inflation$date)
data_inflation$value <- as.numeric(data_inflation$value)
data_wagegrowth$date <- as.Date(data_wagegrowth$date)
data_wagegrowth$value <- as.numeric(data_wagegrowth$value)

# Merge both datasets by the 'date' column, rescale the wage growth data, omit n/a values
data_inflationandwages <- merge(data_inflation, data_wagegrowth, by = "date", suffixes = c("_inflation", "_wage_growth"))
data_inflationandwages$scaled_wage_growth <- data_inflationandwages$value_wage_growth / 100000000
data_inflationandwages <- na.omit(data_inflationandwages)

# Plot the data using ggplot
ggplot(data_inflationandwages, aes(x = date)) +
  geom_line(aes(y = value_inflation, color = "Inflation"), size = 1) +
  geom_line(aes(y = scaled_wage_growth, color = "Wage Growth"), size = 1) +
  scale_y_continuous(
    name = "Inflation (percent)",
    sec.axis = sec_axis(~ ., name = "Wage Growth (hundreds of millions)")
  ) +
  labs(
    title = "Inflation vs Wage Growth",
    x = "Date",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Inflation" = "red", "Wage Growth" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )



# Convert SAP500 columns to numeric datatypes, create a year_quarter column, group "year_quarter" to calculate average open and close prices
data_sap500quarterlyavg <- data_sap500 |> 
  mutate(
    open = as.numeric(open),
    close = as.numeric(close),
    high = as.numeric(high),
    low = as.numeric(low),
    volume = as.numeric(volume),
    date = as.Date(date),
    year_quarter = paste(year(date), "-Q", quarter(date), sep = "")
  ) |> 
  group_by(year_quarter) |> 
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(
    `Year and Quarter` = year_quarter,
    `Average Open` = avg_open,
    `Average Close` = avg_close
  )

# Display the SAP500 results as a table
data_sap500quarterlyavg |> kable()

# Convert MSCI columns to numeric datatypes, create a year_quarter column, group "year_quarter" to calculate average open and close prices
data_msciquarterlyavg <- data_msci |> 
  mutate(
    open = as.numeric(open),
    close = as.numeric(close),
    high = as.numeric(high),
    low = as.numeric(low),
    volume = as.numeric(volume),
    date = as.Date(date),
    year_quarter = paste(year(date), "-Q", quarter(date), sep = "")
  ) |> 
  group_by(year_quarter) |> 
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(
    `Year and Quarter` = year_quarter,
    `Average Open` = avg_open,
    `Average Close` = avg_close
  )

# Display the MSCI results as a table
data_msciquarterlyavg |> kable()





### TASK 5: Historical Comparison ###
# Change the datatypes in the stock market data
data_sap500$open <- as.numeric(data_sap500$open)
data_sap500$close <- as.numeric(data_sap500$close)
data_msci$open <- as.numeric(data_msci$open)
data_msci$close <- as.numeric(data_msci$close)
data_shortdebts$date <- as.Date(data_shortdebts$date)
data_shortdebts$value <- as.numeric(data_shortdebts$value)



# TRS Retirement Calculation
calculate_trs_retirement_benefit <- function(salary_data, years_served, data_inflation, retirement_date) {
  # salary_data: A numeric vector of the employee's salary for the last 3 years
  # years_served: The number of years the employee has worked
  # data_inflation: A data frame containing 'date' and 'value' columns with CPI data
  # retirement_date: The employee's retirement date (as a Date object)
  
  # 1. Calculate Final Average Salary (FAS) based on the last 3 years' salary
  FAS <- mean(salary_data)
  
  # 2. Calculate the base retirement benefit
  if (years_served <= 20) {
    base_benefit <- 0.0167 * FAS * years_served
  } else if (years_served == 20) {
    base_benefit <- 0.0175 * FAS * years_served
  } else {
    base_benefit <- (0.35 + 0.02 * years_served) * FAS
  }
  
  # 3. Convert 'retirement_date' to Date class if not already
  retirement_date <- as.Date(retirement_date)
  
  # 4. Get CPI data for the period from January 2010 to the month of retirement
  data_inflation$date <- as.Date(data_inflation$date)  # Ensure 'date' is Date class
  
  # Filter CPI data from January 2010 to the month of retirement
  cpi_period <- data_inflation[data_inflation$date >= "2010-01-01" & data_inflation$date <= retirement_date, ]
  
  # 5. Initialize inflation adjustment
  inflation_adjustment <- 0  # Start with no inflation adjustment
  
  # Loop through each month in the CPI period and calculate inflation adjustment
  for (i in 1:nrow(cpi_period)) {
    avg_cpi <- cpi_period$value[i]  # CPI for the current month
    
    # Calculate the inflation adjustment for the current month
    monthly_inflation <- round(0.5 * avg_cpi, 1)  # 50% of the CPI, rounded to nearest 0.1%
    monthly_inflation <- pmin(pmax(monthly_inflation, 1), 3)  # cap between 1% and 3%
    
    # Apply the monthly inflation adjustment to the base benefit
    base_benefit <- base_benefit * (1 + monthly_inflation / 100)
  }
  
  # 6. Return the final adjusted retirement benefit
  return(base_benefit)
}



# ORP Retirement Calculation 
calculate_orp <- function(age, salary, start_date, end_date, data_sap500, data_msci, data_shortdebts) {
  # Set asset allocation based on age range
  if (age >= 25 && age <= 49) {
    allocation <- c(US = 0.54, International = 0.36, Bonds = 0.10, ShortDebt = 0)
  } else if (age >= 50 && age <= 59) {
    allocation <- c(US = 0.47, International = 0.32, Bonds = 0.21, ShortDebt = 0)
  } else if (age >= 60 && age <= 74) {
    allocation <- c(US = 0.34, International = 0.23, Bonds = 0.43, ShortDebt = 0)
  } else if (age >= 75) {
    allocation <- c(US = 0.19, International = 0.13, Bonds = 0.62, ShortDebt = 0.06)
  }
  
  # Calculate market returns for each asset class
  us_returns <- (data_sap500$close - data_sap500$open) / data_sap500$open
  international_returns <- (data_msci$close - data_msci$open) / data_msci$open
  shortdebt_returns <- diff(log(data_shortdebts$value)) # assuming logarithmic return
  bond_returns <- rep(0, length(data_sap500$date)) # assuming bonds return 0 if not specified
  
  # Salary-based contribution percentages
  if (salary <= 45000) {
    emp_contrib_rate <- 0.03
  } else if (salary <= 55000) {
    emp_contrib_rate <- 0.035
  } else if (salary <= 75000) {
    emp_contrib_rate <- 0.045
  } else if (salary <= 100000) {
    emp_contrib_rate <- 0.0575
  } else {
    emp_contrib_rate <- 0.06
  }
  
  # Employer contribution rate (8% for first 7 years, 10% thereafter)
  employer_contrib_rate <- ifelse(age <= 31, 0.08, 0.10)  # assuming employment starts at age 25
  
  # Initial account balance
  account_balance <- 0  # Starting with no balance
  
  # Monthly contributions (employee + employer)
  emp_monthly_contrib <- (emp_contrib_rate * salary) / 12
  employer_monthly_contrib <- (employer_contrib_rate * salary) / 12
  
  # Withdrawals (4% annually, divided monthly)
  annual_withdrawal_rate <- 0.04
  monthly_withdrawal <- (annual_withdrawal_rate * salary) / 12
  
  # Date range for simulation
  date_range <- seq.Date(as.Date(start_date), as.Date(end_date), by = "month")
  
  # Loop over each month
  for (i in 1:length(date_range)) {
    # Monthly contributions
    account_balance <- account_balance + emp_monthly_contrib + employer_monthly_contrib
    
    # Apply asset allocation returns
    us_growth <- allocation["US"] * us_returns[i %% length(us_returns) + 1]  # Wrap around if less than the number of months
    international_growth <- allocation["International"] * international_returns[i %% length(international_returns) + 1]
    bond_growth <- allocation["Bonds"] * bond_returns[i %% length(bond_returns) + 1]
    shortdebt_growth <- allocation["ShortDebt"] * shortdebt_returns[i %% length(shortdebt_returns) + 1]
    
    # Update account balance with growth
    account_balance <- account_balance * (1 + us_growth + international_growth + bond_growth + shortdebt_growth)
    
    # Monthly withdrawal
    account_balance <- account_balance - monthly_withdrawal
  }
  
  return(account_balance)
}



# TRS Retirement Example
salary_data <- c(70000, 75000, 80000)
years_served <- 14
retirement_date <- "2024-02-09"

adjusted_benefit <- calculate_trs_retirement_benefit(salary_data, years_served, data_inflation, retirement_date)
cat("The TRS Retirement balance in month 1 is", adjusted_benefit, "\n")



# ORP Example
age <- 75
salary <- 80000
start_date <- "2010-01-01"
end_date <- "2024-02-09"

final_balance <- calculate_orp(age, salary, start_date, end_date, data_sap500, data_msci, data_shortdebts)
cat("The ORP balance in month 1 is", round(final_balance, 2), "\n")





### TASK 6: Fixed-Rate Analysis ###
calculate_trs_retirement_benefit <- function(salary_data, years_served, data_inflation, retirement_date, life_expectancy_years) {
  # salary_data: A numeric vector of the employee's salary for the last 3 years
  # years_served: The number of years the employee has worked
  # data_inflation: A data frame containing 'date' and 'value' columns with CPI data
  # retirement_date: The employee's retirement date (as a Date object)
  # life_expectancy_years: The expected years the employee will live post-retirement (default = 30)
  
  # 1. Calculate Final Average Salary (FAS) based on the last 3 years' salary
  FAS <- mean(salary_data)
  
  # 2. Calculate the base retirement benefit
  if (years_served <= 20) {
    base_benefit <- 0.0167 * FAS * years_served
  } else if (years_served == 20) {
    base_benefit <- 0.0175 * FAS * years_served
  } else {
    base_benefit <- (0.35 + 0.02 * years_served) * FAS
  }
  
  # 3. Convert 'retirement_date' to Date class if not already
  retirement_date <- as.Date(retirement_date)
  
  # 4. Get CPI data for the period from January 2010 to the year of retirement
  data_inflation$date <- as.Date(data_inflation$date)  # Ensure 'date' is Date class
  
  # Filter CPI data from January 2010 to the year of retirement
  cpi_period <- data_inflation[data_inflation$date >= "2010-01-01" & data_inflation$date <= retirement_date, ]
  
  # 5. Calculate the average inflation over the last 3 years
  start_date <- as.Date(format(retirement_date, "%Y-01-01")) - 365 * 3  # Calculate the date 3 years before retirement
  inflation_period <- data_inflation[data_inflation$date >= start_date, ]
  avg_inflation <- mean(inflation_period$value) / 100  # Convert CPI to decimal (e.g., 2.5% -> 0.025)
  
  # 6. Apply inflation adjustments annually and project pension benefit until death
  projected_benefit <- numeric(life_expectancy_years)
  projected_benefit[1] <- base_benefit  # Initial benefit at retirement
  
  # Project the benefit over the years of retirement
  for (year in 2:life_expectancy_years) {
    # Apply COLA adjustment annually
    adjusted_benefit <- projected_benefit[year - 1] * (1 + avg_inflation)
    projected_benefit[year] <- adjusted_benefit
  }
  
  # 7. Calculate the total projected pension benefit over the retirement period
  total_projected_benefit <- sum(projected_benefit)
  
  # 8. Return the projected pension benefit over the retirement period and the total sum
  list(
    projected_benefit = projected_benefit,  # Return the projected benefits for each year
    total_projected_benefit = total_projected_benefit  # Return the total sum of projected benefits
  )
}



calculate_orp <- function(age, salary, start_date, data_sap500, data_msci, data_shortdebts, life_expectancy) {
  
  # Set asset allocation based on age range
  if (age >= 25 && age <= 49) {
    allocation <- c(US = 0.54, International = 0.36, Bonds = 0.10, ShortDebt = 0)
  } else if (age >= 50 && age <= 59) {
    allocation <- c(US = 0.47, International = 0.32, Bonds = 0.21, ShortDebt = 0)
  } else if (age >= 60 && age <= 74) {
    allocation <- c(US = 0.34, International = 0.23, Bonds = 0.43, ShortDebt = 0)
  } else if (age >= 75) {
    allocation <- c(US = 0.19, International = 0.13, Bonds = 0.62, ShortDebt = 0.06)
  }
  
  # Calculate long-run average market returns for each asset class (e.g., last 10 years)
  us_avg_return <- mean((data_sap500$close - data_sap500$open) / data_sap500$open, na.rm = TRUE)
  international_avg_return <- mean((data_msci$close - data_msci$open) / data_msci$open, na.rm = TRUE)
  shortdebt_avg_return <- mean(diff(log(data_shortdebts$value)), na.rm = TRUE)
  bond_avg_return <- 0  # assuming bonds return 0 if not specified
  
  # Salary-based contribution percentages
  if (salary <= 45000) {
    emp_contrib_rate <- 0.03
  } else if (salary <= 55000) {
    emp_contrib_rate <- 0.035
  } else if (salary <= 75000) {
    emp_contrib_rate <- 0.045
  } else if (salary <= 100000) {
    emp_contrib_rate <- 0.0575
  } else {
    emp_contrib_rate <- 0.06
  }
  
  # Employer contribution rate (8% for first 7 years, 10% thereafter)
  employer_contrib_rate <- ifelse(age <= 31, 0.08, 0.10)  # assuming employment starts at age 25
  
  # Initial account balance
  account_balance <- 0  # Starting with no balance
  
  # Monthly contributions (employee + employer)
  emp_monthly_contrib <- (emp_contrib_rate * salary) / 12
  employer_monthly_contrib <- (employer_contrib_rate * salary) / 12
  
  # Withdrawals (4% annually, divided monthly)
  annual_withdrawal_rate <- 0.04
  monthly_withdrawal <- (annual_withdrawal_rate * salary) / 12
  
  # Estimate number of months until the employee's death
  months_until_death <- (life_expectancy - age) * 12
  date_range <- seq(1, months_until_death)  # We simulate each month
  
  # Loop over each month
  for (i in 1:length(date_range)) {
    # Monthly contributions
    account_balance <- account_balance + emp_monthly_contrib + employer_monthly_contrib
    
    # Apply asset allocation returns (using long-run average returns)
    us_growth <- allocation["US"] * us_avg_return
    international_growth <- allocation["International"] * international_avg_return
    bond_growth <- allocation["Bonds"] * bond_avg_return
    shortdebt_growth <- allocation["ShortDebt"] * shortdebt_avg_return
    
    # Update account balance with growth
    account_balance <- account_balance * (1 + us_growth + international_growth + bond_growth + shortdebt_growth)
    
    # Monthly withdrawal
    account_balance <- account_balance - monthly_withdrawal
    
    # Ensure account balance does not go negative (you can't withdraw more than available)
    account_balance <- max(account_balance, 0)
  }
  
  return(account_balance)
}



# Projected total TRS pension
salary_data <- c(70000, 75000, 80000)
years_served <- 14
retirement_date <- "2010-02-09"
life_expectancy_years = 80

result <- calculate_trs_retirement_benefit(salary_data, years_served, data_inflation, retirement_date, life_expectancy_years)
cat("The total projected TRS pension benefit is", result$total_projected_benefit, "\n")



# Projected total ORP amount
age <- 66
salary <- 80000
start_date <- "2010-02-09"
life_expectancy = 80

final_balance <- calculate_orp(age, salary, start_date, data_sap500, data_msci, data_shortdebts, life_expectancy)
cat("The total projected ORP withdrawl amount is", final_balance, "\n")





### TASK 7: Monte Carlo Analysis ###
# TRS Bootstrap Analysis
set.seed(123)  # Set seed for reproducibility

# Function to generate synthetic salary data for an employee
generate_salary_data <- function() {
  # Simulating last 3 years of salary data (e.g., between $40,000 and $80,000)
  sample(seq(40000, 80000, by = 500), 3)
}

# Function to generate synthetic life expectancy data
generate_life_expectancy <- function() {
  # Assume life expectancy between 25 and 35 years post-retirement
  sample(25:35, 1)
}

# Monte Carlo Simulation to generate 200 bootstrap histories
n_simulations <- 200
bootstrap_histories <- list()

for (i in 1:n_simulations) {
  # Generate synthetic data
  salary_data <- generate_salary_data()
  years_served <- sample(10:35, 1)  # Random years of service between 10 and 35
  retirement_date <- as.Date("2024-12-01") - sample(1:365, 1)  # Random retirement date in 2024
  
  # Simulate life expectancy
  life_expectancy_years <- generate_life_expectancy()
  
  # Run the retirement benefit calculation function (using second function with life expectancy)
  result <- calculate_trs_retirement_benefit(salary_data, years_served, data_inflation, retirement_date, life_expectancy_years)
  
  # Store the result
  bootstrap_histories[[i]] <- result
}

# Printing some summary statistics of the total projected benefits
total_benefits <- sapply(bootstrap_histories, function(x) x$total_projected_benefit)
summary(total_benefits)



# Extract the total projected benefits from all simulations
total_benefits <- sapply(bootstrap_histories, function(x) x$total_projected_benefit)

# Convert to a data frame for ggplot2
benefits_df <- data.frame(total_benefit = total_benefits)

# Create a histogram of the total projected benefits
ggplot(benefits_df, aes(x = total_benefit)) +
  geom_histogram(binwidth = 50000, color = "black", fill = "black", alpha = 0.7) +
  labs(
    title = "Distribution of TRS Total Projected Retirement Benefits (200 Simulations)",
    x = "Total Projected Benefit ($)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  # Add summary statistics (mean and 95% CI)
  geom_vline(aes(xintercept = mean(total_benefits)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(total_benefits)), color = "green", linetype = "dashed", size = 1) +
  annotate("text", x = mean(total_benefits), y = 5, label = paste("Mean:", round(mean(total_benefits), 2)), color = "red", hjust = 1) +
  annotate("text", x = median(total_benefits), y = 5, label = paste("Median:", round(median(total_benefits), 2)), color = "green", hjust = 0)





# ORP Bootstrap Analysis
set.seed(123)  # Set a seed for reproducibility

# Function to calculate final balance from ORP (with asset returns)
calculate_orp <- function(age, salary, start_date, data_sap500, data_msci, data_shortdebts, life_expectancy) {
  
  # Set asset allocation based on age range
  if (age >= 25 && age <= 49) {
    allocation <- c(US = 0.54, International = 0.36, Bonds = 0.10, ShortDebt = 0)
  } else if (age >= 50 && age <= 59) {
    allocation <- c(US = 0.47, International = 0.32, Bonds = 0.21, ShortDebt = 0)
  } else if (age >= 60 && age <= 74) {
    allocation <- c(US = 0.34, International = 0.23, Bonds = 0.43, ShortDebt = 0)
  } else if (age >= 75) {
    allocation <- c(US = 0.19, International = 0.13, Bonds = 0.62, ShortDebt = 0.06)
  }
  
  # Calculate monthly returns for each asset class
  us_returns <- (data_sap500$close - data_sap500$open) / data_sap500$open
  international_returns <- (data_msci$close - data_msci$open) / data_msci$open
  shortdebt_returns <- diff(log(data_shortdebts$value))  # Logarithmic return for short-term debt
  
  # Salary-based contribution percentages
  if (salary <= 45000) {
    emp_contrib_rate <- 0.03
  } else if (salary <= 55000) {
    emp_contrib_rate <- 0.035
  } else if (salary <= 75000) {
    emp_contrib_rate <- 0.045
  } else if (salary <= 100000) {
    emp_contrib_rate <- 0.0575
  } else {
    emp_contrib_rate <- 0.06
  }
  
  # Employer contribution rate (8% for first 7 years, 10% thereafter)
  employer_contrib_rate <- ifelse(age <= 31, 0.08, 0.10)  # assuming employment starts at age 25
  
  # Initial account balance
  account_balance <- 0  # Starting with no balance
  
  # Monthly contributions (employee + employer)
  emp_monthly_contrib <- (emp_contrib_rate * salary) / 12
  employer_monthly_contrib <- (employer_contrib_rate * salary) / 12
  
  # Withdrawals (4% annually, divided monthly)
  annual_withdrawal_rate <- 0.04
  monthly_withdrawal <- (annual_withdrawal_rate * salary) / 12
  
  # Estimate number of months until the employee's death
  months_until_death <- (life_expectancy - age) * 12
  date_range <- seq(1, months_until_death)  # We simulate each month
  
  # Loop over each month
  for (i in 1:length(date_range)) {
    # Monthly contributions
    account_balance <- account_balance + emp_monthly_contrib + employer_monthly_contrib
    
    # Apply asset allocation returns (monthly returns)
    us_growth <- allocation["US"] * us_returns[i %% length(us_returns) + 1]  # Wrap around if less than the number of months
    international_growth <- allocation["International"] * international_returns[i %% length(international_returns) + 1]
    bond_growth <- 0
    shortdebt_growth <- allocation["ShortDebt"] * shortdebt_returns[i %% length(shortdebt_returns) + 1]
    
    # Update account balance with growth
    account_balance <- account_balance * (1 + us_growth + international_growth + bond_growth + shortdebt_growth)
    
    # Monthly withdrawal
    account_balance <- account_balance - monthly_withdrawal
    
    # Ensure account balance does not go negative (you can't withdraw more than available)
    account_balance <- max(account_balance, 0)
  }
  
  return(account_balance)
}

# Function to generate bootstrap simulations
generate_bootstrap_histories <- function(n_simulations, data_sap500, data_msci, data_shortdebts) {
  
  bootstrap_histories <- list()  # To store the results of all simulations
  
  # Loop to generate multiple simulations (n_simulations)
  for (i in 1:n_simulations) {
    
    # Randomly sample parameters for each simulation
    age <- sample(25:75, 1)  # Random age between 25 and 75
    salary <- sample(30000:150000, 1)  # Random salary between $30,000 and $150,000
    start_date <- sample(seq.Date(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "years"), 1)  # Random start date
    life_expectancy <- sample(75:100, 1)  # Random life expectancy
    
    # Call the ORP calculation function (second version)
    final_balance <- calculate_orp(age, salary, start_date, data_sap500, data_msci, data_shortdebts, life_expectancy)
    
    # Store the result for this simulation
    bootstrap_histories[[i]] <- final_balance
  }
  
  # Return the bootstrap histories
  return(bootstrap_histories)
}

# Number of bootstrap simulations to run (e.g., 200)
n_simulations <- 200

# Call the function to generate the bootstrap histories
bootstrap_histories <- generate_bootstrap_histories(n_simulations, data_sap500, data_msci, data_shortdebts)

# Convert the bootstrap histories to a data frame
bootstrap_results <- data.frame(final_balance = unlist(bootstrap_histories))

# Printing some summary statistics of the total projected balances
summary(bootstrap_results)



# Calculate the mean and median of the final balances
mean_balance <- mean(bootstrap_results$final_balance)
median_balance <- median(bootstrap_results$final_balance)

# Create a histogram or density plot with mean and median lines
ggplot(bootstrap_results, aes(x = final_balance)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "skyblue", alpha = 0.7) +
  # Add vertical lines for mean and median
  geom_vline(aes(xintercept = mean_balance), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_balance), color = "green", linetype = "dashed", size = 1) +
  # Add labels for the mean and median lines
  annotate("text", x = mean_balance + 20000, y = 10, label = paste("Mean:", round(mean_balance, 2)), color = "red", size = 4, hjust = 0) +
  annotate("text", x = median_balance + 20000, y = 10, label = paste("Median:", round(median_balance, 2)), color = "green", size = 4, hjust = 1) +
  labs(
    title = "Distribution of ORP Final Account Balances (200 Simulations)",
    x = "Final Account Balance ($)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

