---
  output: 
  html_document:
  number_sections=TRUE
  pagetitle: Assignment_7
  ---
    
    
    
    ```{r setup, include=FALSE}
  knitr::opts_chunk$set(echo = TRUE)
  ```
  
  # **Assignment 7**
  
  In this assignment, you will use R (within R-Studio) to:
    
    + Take a real life data set and wrangle it into shape
  + Use a data dictionary to clean and merge two related data sets
  
  **All file paths should be relative, starting from *your* Assignment_7 directory!!**
    
    **This means that you need to create a new R-Project named "Assignment_7.Rproj" in your Assignment_7 directory, and work from scripts within that.**
    
    
    ## **For credit...**
    
    1.  Push a completed version of your Rproj and R-script (details at end of this assignment) to GitHub
  2.  Your score will also depend on whether any files generated in this workflow are found in your repository
  
  
  ____________
  
  # Your tasks:
  
  + Import the 4 related datasets found in the Data_Course/Data/flights/ directory. There should be: 
    - airlines.csv
  - airports.csv
  - jan_flights.csv
  - Jan_snowfall.csv
  + Combine the data sets appropriately to investigate whether departure delay was correlated with snowfall amount
  + You will need to think carefully about column names
  + Plot average departure delays by state over time
  + Plot average departure delays by airline over time
  + Plot effect of snowfall on departure *and* arrival delays
  _____________
  
  #Assignment 7: Data Wrangling and Flight Delays
  # Author: Salma Centeno
  # Date: April 2026
  
  library(tidyverse)
  library(lubridate)
  
  airlines <- read_csv("airlines.csv")
  airports <- read_csv("airports.csv")
  flights  <- read_csv("jan_flights.csv")
  snowfall <- read_csv("Jan_snowfall.csv")
  
  glimpse(flights)
  glimpse(airports)
  glimpse(snowfall)
  
  
  
  flights <- flights %>%
    mutate(date = as.Date(paste(YEAR, MONTH, DAY, sep = "-")))
  
  
  snowfall <- snowfall %>%
    mutate(date = as.Date(Date, format = "%m/%d/%Y"))
  
  flights_with_states <- flights %>%
    left_join(airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE"))
  
  data_combined <- flights_with_states %>%
    left_join(snowfall, by = c("ORIGIN_AIRPORT" = "iata", "date" = "date"))
  
  flights_with_states %>%
    group_by(STATE, date) %>%
    summarize(avg_delay = mean(DEPARTURE_DELAY, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = avg_delay, color = STATE)) +
    geom_line() +
    labs(title = "Average Departure Delay by State Over Time")
  
  
  flights %>%
    group_by(AIRLINE, date) %>%
    summarize(avg_delay = mean(DEPARTURE_DELAY, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = avg_delay, color = AIRLINE)) +
    geom_line() +
    labs(title = "Average Departure Delay by Airline Over Time")
  
  data_combined %>%
    ggplot(aes(x = snow_precip_cm, y = DEPARTURE_DELAY)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    labs(title = "Snowfall vs Departure Delay",
         x = "Snowfall (cm)",
         y = "Departure Delay")
  
  
  data_combined %>%
    ggplot(aes(x = snow_precip_cm, y = ARRIVAL_DELAY)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    labs(title = "Snowfall vs Arrival Delay",
         x = "Snowfall (cm)",
         y = "Arrival Delay")
  
  cor(data_combined$snow_precip_cm,
      data_combined$DEPARTURE_DELAY,
      use = "complete.obs")
  
  cor(data_combined$snow_precip_cm,
      data_combined$ARRIVAL_DELAY,
      use = "complete.obs")
# Snowfall seems to cause a modest increase in departure delays
# Airlines exhibit varying patterns of delays over time
  # Delays are consistently higher in some states than in others.