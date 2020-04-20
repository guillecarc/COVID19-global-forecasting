get_data <- function(train_test_path = "./data/train_test/week4/",
                     UNPop_path = "./data/World Population Prospects 2019 - UN/",
                     KaggleTemp_path = "./data/Climate change earth surface temperature data - Kaggle/GlobalLandTemperaturesByCountry.csv",
                     AppleMob_path = "./data/Apple Mobility Trends/applemobilitytrends-2020-04-15.csv",
                     GoogleMob_path = "./data/Google Mobility Trends/Global_Mobility_Report.csv",
                     Wiki_path = "./data/Wikipedia metadata/region_metadata.csv",
                     GovMsrs_path = "./data/Government measures/20200414_acaps_covid-19_goverment_measures_dataset_v7.csv",
                     DarkSkyTemp_path = "./data/Dark Sky Weather/weather_covid19.csv",
                     LegatumPI_path = "./data/Prosperity Index - The Legatum Institute Foundation/PI_2019_Data.xlsx",
                     complete.cases = TRUE){
  
  source("./R/preprocess_un_pop_data.R")
  source("./R/preprocess_train_test_data.R")
  source("./R/preprocess_kaggle_temp_data.R")
  source("./R/preprocess_apple_mobility_data.R")
  source("./R/preprocess_google_mobility_data.R")
  source("./R/preprocess_wiki_pop_data.R")
  source("./R/preprocess_government_measures_data.R")
  source("./R/preprocess_darksky_temp_data.R")
  source("./R/preprocess_legatum_prosperity_index_data.R")
  
  message("Reading sources")
  train_test_df <- preprocess_traintest(train_test_path)
  UNPop_df <- preprocess_pop_data(UNPop_path)
  KaggleTemp_df <- preprocess_temp_data(KaggleTemp_path)
  AppleMob_df <- preprocess_apple_mobility(AppleMob_path)
  GoogleMob_df <- preprocess_google_mobility(GoogleMob_path)
  WikiPop_df <- preprocess_wiki_pop_data(Wiki_path)
  GovMsrs_df <- preprocess_gob_data(GovMsrs_path)
  DarkSkyTemp_df <- preprocess_darksky_temp(DarkSkyTemp_path)
  LegatumPI_df <- preprocess_legatum_prosperity_index_data(LegatumPI_path)
  
  # if complete cases is true, then only existing countries in all data sets
  # will be selected
  if (complete.cases) {
    sources <- list(
      train_test_df = train_test_df,
      UNPop_df = UNPop_df,
      KaggleTemp_df = KaggleTemp_df,
      AppleMob_df = AppleMob_df,
      GoogleMob_df = GoogleMob_df,
      WikiPop_df = WikiPop_df,
      GovMsrs_df = GovMsrs_df,
      DarkSkyTemp_df = DarkSkyTemp_df,
      LegatumPI_df = LegatumPI_df
    )
    
    sources <- map(sources, ~{enframe(as.character(unique(.x %>% ungroup %>% .$Country_Region)),
                                      value = "Country_Region")})
    
    country_check <- data.frame()
    for (s in 1:length(sources)){
      if (names(sources[s]) == "train_test_df") {
        mapping <- sources[[s]]
        mapping$lower <- str_to_lower(mapping$Country_Region)
        mapping <- rename(mapping, original = Country_Region)
      }
      sources[[s]]$Country_Region <- str_to_lower(sources[[s]]$Country_Region)
      if (is_empty(country_check)){
        country_check <- sources[[s]]
      } else {
        country_check <- inner_join(country_check,
                                    sources[[s]],
                                    by = "Country_Region")
      }
    }
    country_check <- left_join(mapping, country_check,
                               by = c("lower"="Country_Region"))
    country_check <- country_check$original[which(complete.cases(country_check))]
    message(length(country_check), " countries were left after checking for complete.cases")
  }
  
  message("Joining sources")
  # Join population data ----------------------------------------------------
  df <- left_join(train_test_df,
                  UNPop_df,
                  by = c("Country_Region"))
  
  # Join temperature data ---------------------------------------------------
  df <- left_join(df %>% mutate(month = month(Date)),
                  KaggleTemp_df,
                  by = c("Country_Region", "month"))
  
  df <- select(df, -month)
  
  # Join Apple mobility data ------------------------------------------------
  df <- left_join(df,
                  AppleMob_df,
                  by = c("Country_Region", "Date" = "date"))
  
  # Join Google mobility data -----------------------------------------------
  df <- left_join(df,
                  GoogleMob_df,
                  by = c("Country_Region", "Date"))
  
  google_names <- names(df)
  google_names <- google_names[which(str_detect(google_names, "^Google__"))]
  df <- df %>% 
    mutate_at(vars(google_names), replace_na, 0)
  message("Google Mobility data does not contain data before 2020-02-14, it will be imputed with 0")
  # Join Wikipedia medatadata -----------------------------------------------
  df <- left_join(df, WikiPop_df,
                  by = c("Country_Region"))
  
  # Join Government measures data -------------------------------------------
  df <- left_join(df %>% mutate(lower = str_to_lower(Country_Region)),
                  GovMsrs_df,
                  by = c("lower"="Country_Region", "Date"))
  
  df <- select(df, -lower)
  
  gob_names <- names(df)[which(str_detect(names(df), pattern = "Gob__"))]
  df <- df %>% 
    mutate_at(vars(gob_names), replace_na, 0)
  
  # Join DarkSky temperatures -----------------------------------------------
  df <- left_join(df,
                  DarkSkyTemp_df,
                  by = c("Country_Region", "Date"))
  
  # Join Legatum Prosperity Indey data --------------------------------------
  df <- left_join(df,
                  LegatumPI_df,
                  by = c("Country_Region"))
  
  if (complete.cases){
    message("Leaving only complete cases")
    df <- df[which(df$Country_Region %in% country_check),]
    
    # Test data to merge later
    test_df <- df %>% 
      filter(data_type == "test")
    
    # Get the maximun date from the train data to check for missings
    max_train_date <- df %>% 
      filter(data_type == "train") %>% 
      select(Date) %>% 
      pull %>% 
      max
    message("The maximum date for training data is ", max_train_date)
    
    names <- names(df)
    sources <- unique(str_extract(names, pattern = "^[:alpha:]+(?=__)"))
    sources <- sources[which(!is.na(sources))]
    
    vars2rm <- vector()
    df <- df %>% 
      filter(data_type == "train")
    for (s in sources){
      s_names <- names[str_detect(names, pattern = paste(s, collapse = "|"))]
      missing_vars <- colSums(is.na(df %>% select_at(vars(s_names))))
      if (sum(missing_vars) > 0) {
        # How many data points
        message(sum(missing_vars), " data points are missing for ", s, " source") 
        
        # Which variables
        missing_vars <- names(df %>% select_at(vars(s_names)))[which(colSums(is.na(df %>% select_at(vars(s_names)))) > 0)]
        vars2rm <- c(vars2rm, missing_vars)
        text <- paste(c("The missing variables are:",missing_vars), collapse = "\n")
        message(text)
      }
    }
    
    df <- bind_rows(df, test_df)
    df <- df %>% select_at(vars(-missing_vars))
    message("missing variables detected were removed")
  }
  return(df)
}
