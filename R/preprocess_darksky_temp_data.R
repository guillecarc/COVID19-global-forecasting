preprocess_darksky_temp <- function(path = "./data/Dark Sky Weather/weather_covid19.csv"){
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  df <- df %>% select(-X:-Long, -icon, -summary, -Province.State)
  
  df <- df %>% 
    group_by(Country.Region, time) %>% 
    summarise_if(is.numeric, mean,na.rm = TRUE) %>% 
    ungroup()
  
  df <- df %>% 
    rename(Country_Region = Country.Region,
           Date = time)
  
  df$Country_Region[which(df$Country_Region == "The Bahamas")] <- "Bahamas"
  df$Country_Region[which(df$Country_Region == "The Gambia")] <- "Gambia"
  
  names(df)[which(!(names(df) %in% c("Country_Region", "Date")))] <- paste0("DarkSky__",names(df)[which(!(names(df) %in% c("Country_Region", "Date")))])
  
  df$Date <- as.Date(df$Date)
  
  # The following variables are remove from the dataset as they have several
  # missing values, and there is not a straight-foward reliable approach without
  # falling into investigation
  var2rm <- df %>% 
    select_if(is.numeric) %>% 
    select_at(vars(names(df[, colSums(is.na(df)) > 0]))) %>% 
    names
  
  df <- df %>% select_at(vars(-var2rm))
  
  return(df)
}