preprocess_temp_data <- function(path = "/Users/c.cardenasvelasquez/Documents/git/COVID19-global-forecasting/data/Climate change earth surface temperature data - Kaggle/GlobalLandTemperaturesByCountry.csv"
){
  
  require(tidyverse)
  require(lubridate)
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  df$dt <- as.Date(df$dt)
  
  df <- df %>% 
    filter(!is.na(AverageTemperature)) %>% 
    mutate(month = month(dt)) %>% 
    group_by(Country, month) %>% 
    arrange(Country, month, desc(dt)) %>%
    top_n(10, wt = dt) %>% 
    summarise(AvgTemp = mean(AverageTemperature))
  
  df$Country[which(df$Country == "Congo (Democratic Republic Of The)")] <- "Congo (Kinshasa)"
  df$Country[which(df$Country == "Congo")] <- "Congo (Brazzaville)"
  df$Country[which(df$Country == "Antigua And Barbuda")] <- "Antigua and Barbuda"
  df$Country[which(df$Country == "Bosnia And Herzegovina")] <- "Bosnia and Herzegovina"
  df$Country[which(df$Country == "Cape Verde")] <- "Cabo Verde"
  df$Country[which(df$Country == "Côte D'Ivoire")] <- "Cote d'Ivoire"
  df$Country[which(df$Country == "Czech Republic")] <- "Czechia"
  df$Country[which(df$Country == "Swaziland")] <- "Eswatini"
  df$Country[which(df$Country == "Guinea Bissau")] <- "Guinea-Bissau"
  df$Country[which(df$Country == "South Korea")] <- "Korea, South"
  df$Country[which(df$Country == "Falkland Islands (Islas Malvinas)")] <- "Maldives" 
  df$Country[which(df$Country == "Macedonia")] <- "North Macedonia"
  df$Country[which(df$Country == "Saint Vincent And The Grenadines")] <- "Saint Vincent and the Grenadines" 
  df$Country[which(df$Country == "Sao Tome And Principe")] <- "Sao Tome and Principe"
  df$Country[which(df$Country == "Sudan")] <- "South Sudan"
  df$Country[which(df$Country == "Timor Leste")] <- "Timor-Leste"
  df$Country[which(df$Country == "Trinidad And Tobago")] <- "Trinidad and Tobago"
  df$Country[which(df$Country == "United States")] <- "US"
  df$Country[which(df$Country == "Timor Leste")] <- "Timor-Leste"
  df$Country[which(df$Country == "Taiwan")] <- "Taiwan*"

  df <- rename(df, Country_Region = Country)
  
  names(df)[which(!(names(df) %in% c("Country_Region", "month")))] <- paste0("Kaggle__",names(df)[which(!(names(df) %in% c("Country_Region", "month")))])
  
  return(df)
}