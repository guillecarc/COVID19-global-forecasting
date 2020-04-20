preprocess_google_mobility <- function(path = "./data/Google Mobility Trends/Global_Mobility_Report.csv"){
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  df <- filter(df, sub_region_1 == "")
  
  df <- rename(df, Country_Region = country_region, Date = date)
  
  df <- select(df, 2,5:11)
  
  names(df)[which(!(names(df) %in% c("Country_Region", "Date")))] <- paste0("Google__",
                                                                            str_remove_all(names(df)[which(!(names(df) %in% c("Country_Region", "Date")))],
                                                                                           pattern = "_percent_change_from_baseline")) 
  
  df$Country_Region[which(df$Country_Region == "The Bahamas")] <- "Bahamas"
  df$Country_Region[which(df$Country_Region == "Côte d'Ivoire")] <- "Cote d'Ivoire"
  df$Country_Region[which(df$Country_Region == "Cape Verde")] <- "Cabo Verde"
  df$Country_Region[which(df$Country_Region == "South Korea")] <- "Korea, South"
  df$Country_Region[which(df$Country_Region == "Myanmar (Burma)")] <- "Burma"
  df$Country_Region[which(df$Country_Region == "Taiwan")] <- "Taiwan*"
  df$Country_Region[which(df$Country_Region == "United States")] <- "US"
  
  df$Date <- as.Date(df$Date)
  
  return(df)
}
