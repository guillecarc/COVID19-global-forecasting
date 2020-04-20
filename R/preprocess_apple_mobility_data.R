preprocess_apple_mobility <- function(path = "./data/Apple Mobility Trends/applemobilitytrends-2020-04-15.csv"){
  
  require(tidyverse)
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  df <- filter(df, geo_type == "country/region")
 
  df <- df %>% 
    pivot_longer(cols = contains("X"),names_to = "date", values_to = "value")
  
  df <- df %>% 
    mutate(date = parse_date(date, format = "X%Y.%m.%d"))
  
  df <- df %>% 
    pivot_wider(names_from = transportation_type,
                values_from = value)
  
  df <- df %>% mutate_at(vars("driving", "walking", "transit"), replace_na, 100)
  
  df <- rename(df, Country_Region = region)
  
  df$Country_Region[which(df$Country_Region == "Czech Republic")] <- "Czechia"
  df$Country_Region[which(df$Country_Region == "Republic of Korea")] <- "Korea, South"
  df$Country_Region[which(df$Country_Region == "UK")] <- "United Kingdom"
  df$Country_Region[which(df$Country_Region == "United States")] <- "US"
  df$Country_Region[which(df$Country_Region == "Taiwan")] <- "Taiwan*"

  df <- select(df, -geo_type)
  
  names(df)[which(!(names(df) %in% c("Country_Region", "date")))] <- paste0("Apple__",names(df)[which(!(names(df) %in% c("Country_Region", "date")))])
  
  return(df)
}