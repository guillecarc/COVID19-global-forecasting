preprocess_pop_data <- function(path = "./data/World Population Prospects 2019 - UN/"){
  
  # Libraries and parameters ------------------------------------------------
  require(openxlsx)
  require(tidyverse)
  
  # Load files --------------------------------------------------------------
  
  files <- list.files(path = path, 
                      full.names = TRUE,
                      pattern = ".xlsx$")
  
  files_df_list <- map(.x = files, read.xlsx,
                       sheet = 1,
                       colNames = FALSE, 
                       rowNames = FALSE,
                       check.names = FALSE)
  
  files_df_list <- set_names(files_df_list,
                             nm = basename(files))
  
  
  # Process data ------------------------------------------------------------
  for (df in 1:length(files_df_list)){
    
    # File with median age
    if (grepl("MEDIAN", names(files_df_list[df]))) {
      files_df_list[[df]] <- files_df_list[[df]][11:nrow(files_df_list[[df]]),]
      names(files_df_list[[df]]) <- files_df_list[[df]][1,]
      files_df_list[[df]] <- files_df_list[[df]][-1,]
      files_df_list[[df]] <- files_df_list[[df]][which(files_df_list[[df]]$Type == "Country/Area"),]
      files_df_list[[df]] <- select(files_df_list[[df]], `Region, subregion, country or area *`, `2020`)
      names(files_df_list[[df]]) <- c("Country_Region", "median_age")
      files_df_list[[df]]$median_age <- as.numeric(files_df_list[[df]]$median_age)
    }
    
    # File with population density
    if (grepl("DENSITY", names(files_df_list[df]))) {
      files_df_list[[df]] <- files_df_list[[df]][11:nrow(files_df_list[[df]]),]
      names(files_df_list[[df]]) <- files_df_list[[df]][1,]
      files_df_list[[df]] <- files_df_list[[df]][-1,]
      files_df_list[[df]] <- files_df_list[[df]][which(files_df_list[[df]]$Type == "Country/Area"),]
      files_df_list[[df]] <- select(files_df_list[[df]], `Region, subregion, country or area *`, `2020`)
      names(files_df_list[[df]]) <- c("Country_Region", "density")
      files_df_list[[df]]$density <- as.numeric(files_df_list[[df]]$density)
    }
    
    # File with population by age group
    if (grepl("BY_AGE", names(files_df_list[df]))) {
      files_df_list[[df]] <- files_df_list[[df]][11:nrow(files_df_list[[df]]),]
      names(files_df_list[[df]]) <- files_df_list[[df]][1,]
      files_df_list[[df]] <- files_df_list[[df]][-1,]
      files_df_list[[df]] <- files_df_list[[df]][which(files_df_list[[df]]$Type == "Country/Area"),]
      files_df_list[[df]] <- files_df_list[[df]][which(files_df_list[[df]]$`Reference date (as of 1 July)` == "2020"),]
      files_df_list[[df]] <- select(files_df_list[[df]], `Region, subregion, country or area *`, 9:29)
      names(files_df_list[[df]])[1] <- c("Country_Region")
      files_df_list[[df]] <- mutate_at(files_df_list[[df]], vars(2:22), as.numeric)
    }
  }
  
  # Join dataframes ---------------------------------------------------------
  merged_df <- data.frame(stringsAsFactors = FALSE)
  for (df in 1:length(files_df_list)){
    if (is_empty(merged_df)){
      merged_df <- files_df_list[[df]]
    } 
    else {
      merged_df <- left_join(merged_df, files_df_list[[df]],  
                             by = "Country_Region")
    }
  }
  
  male_gender <- which(grepl("_MALE", names(files_df_list)))
  female_gender <- which(grepl("_FEMALE", names(files_df_list)))
  
  if (male_gender < female_gender){
    x <- ".m"
    y <- ".f"
  } else {
    x <- ".f"
    y <- ".m"
  }
  
  names(merged_df) <- str_replace_all(names(merged_df), "\\.x", x)
  names(merged_df) <- str_replace_all(names(merged_df), "\\.y", y)
  
  # There are some countries that could definitely not be matched
  merged_df$Country_Region[which(merged_df$Country_Region == "Myanmar")] <- "Burma" 
  merged_df$Country_Region[which(merged_df$Country_Region == "Congo")] <- "Congo (Brazzaville)"
  merged_df$Country_Region[which(merged_df$Country_Region == "Democratic Republic of the Congo")] <- "Congo (Kinshasa)"
  merged_df$Country_Region[which(merged_df$Country_Region == "Côte d'Ivoire")] <- "Cote d'Ivoire"
  merged_df$Country_Region[which(merged_df$Country_Region == "Republic of Korea")] <- "Korea, South"
  merged_df$Country_Region[which(merged_df$Country_Region == "Lao People's Democratic Republic")] <- "Laos"
  merged_df$Country_Region[which(merged_df$Country_Region == "United States of America")] <- "US"
  merged_df$Country_Region[which(merged_df$Country_Region == "Viet Nam")] <- "Vietnam"
  merged_df$Country_Region[which(merged_df$Country_Region == "Bolivia (Plurinational State of)")] <- "Bolivia"
  merged_df$Country_Region[which(merged_df$Country_Region == "Brunei Darussalam")] <- "Brunei"
  merged_df$Country_Region[which(merged_df$Country_Region == "Iran (Islamic Republic of)")] <- "Iran"
  merged_df$Country_Region[which(merged_df$Country_Region == "Republic of Moldova")] <- "Moldova"
  merged_df$Country_Region[which(merged_df$Country_Region == "Russian Federation")] <- "Russia"
  merged_df$Country_Region[which(merged_df$Country_Region == "Syrian Arab Republic")] <- "Syria"
  merged_df$Country_Region[which(merged_df$Country_Region == "China, Taiwan Province of China")] <- "Taiwan*"
  merged_df$Country_Region[which(merged_df$Country_Region == "United Republic of Tanzania")] <- "Tanzania"
  merged_df$Country_Region[which(merged_df$Country_Region == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  
  names(merged_df)[which(names(merged_df) != "Country_Region")] <- paste0("UN__",names(merged_df)[which(names(merged_df) != "Country_Region")])
  
  return(merged_df)
}
