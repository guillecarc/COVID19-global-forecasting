preprocess_gob_data <- function(path = "./data/Government measures/20200414_acaps_covid-19_goverment_measures_dataset_v7.csv"){
  
  require(tidyverse)
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  df <- filter(df, 
               CATEGORY != "",
               DATE_IMPLEMENTED != "")
  
  df$COUNTRY <- str_squish(str_to_lower(df$COUNTRY))
  
  df <- df %>% 
    group_by(COUNTRY, LOG_TYPE, CATEGORY, DATE_IMPLEMENTED) %>% 
    tally() %>% #To use more specific values the data must be inspected in deep
    ungroup()
  
  df <- rename(df,
               Country_Region = COUNTRY,
               Date = DATE_IMPLEMENTED)
  
  df <- df %>% 
    mutate(n = 1,
           ENTRY_MEASURE = case_when(LOG_TYPE == "Introduction / extension of measures" ~ 1,
                                     TRUE ~ 0)) %>% 
    pivot_wider(names_from = CATEGORY,
                values_from = n)
  
  # There are some inconsistencies that require more investigation, for now, when
  # there are 2 types of measure under the same country and date, only
  # introduction measures will persist
  df <- df %>% 
    arrange(Country_Region, Date, LOG_TYPE) %>%
    distinct(Country_Region, Date, .keep_all = TRUE)
  
  df <- select(df, -LOG_TYPE)
  
  names(df)[which(!(names(df) %in% c("Country_Region", "Date")))] <- paste0("Gob__",names(df)[which(!(names(df) %in% c("Country_Region", "Date")))])
  
  df$Country_Region[which(df$Country_Region == "brunei darussalam")] <- "brunei"
  df$Country_Region[which(df$Country_Region == "c�te d'ivoire")] <- "cote d'ivoire"
  df$Country_Region[which(df$Country_Region == "myanmar")] <- "burma"
  df$Country_Region[which(df$Country_Region == "congo")] <- "congo (brazzaville)"
  df$Country_Region[which(df$Country_Region == "congo dr")] <- "congo (kinshasa)"
  df$Country_Region[which(df$Country_Region == "czech republic")] <- "czechia"
  df$Country_Region[which(df$Country_Region == "korea republic of")] <- "korea, south"
  df$Country_Region[which(df$Country_Region == "lao pdr")] <- "laos"
  df$Country_Region[which(df$Country_Region == "moldova republic of")] <- "moldova"
  df$Country_Region[which(df$Country_Region == "north macedonia republic of")] <- "north macedonia"
  df$Country_Region[which(df$Country_Region == "russian federation")] <- "russia"
  df$Country_Region[which(df$Country_Region == "united states of america")] <- "us"
  df$Country_Region[which(df$Country_Region == "viet nam")] <- "vietnam"
  
  df$Date <- as.Date(df$Date, format = "%d-%m-%Y")
  
  df <- df %>% mutate_if(is.numeric, replace_na, 0)
  
  return(df)
  
}

