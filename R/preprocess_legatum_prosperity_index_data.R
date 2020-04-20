preprocess_legatum_prosperity_index_data <- function(path = "./data/Prosperity Index - The Legatum Institute Foundation/PI_2019_Data.xlsx"){
  
  require(tidyverse)
  require(openxlsx)
  
  sheet_index <- c(
    "Pillars x 12",
    "Elements x 65",
    "Indicators x 294"
  )
  
  list <- map(sheet_index, ~{read.xlsx(xlsxFile = path, sheet = .x)})
  list <- set_names(list, sheet_index)
  
  for (df in 1:length(list)){
    if (names(list[df]) == "Pillars x 12"){kpi <- "pillar_name"}
    if (names(list[df]) == "Elements x 65"){kpi <- "element_name"}
    if (names(list[df]) == "Indicators x 294"){kpi <- "indicator_name"}
    
    names <- names(list[[df]])
    names <- names[which(names %in% c("area_name", kpi, "score_2019"))]
    
    list[[df]] <- list[[df]] %>% 
      select_at(vars(names)) %>% 
      pivot_wider(names_from = !! kpi, values_from = "score_2019")
    
    list[[df]] <- rename(list[[df]], Country_Region = area_name)
    
    kpi <- str_remove_all(kpi, "_name$")
    names(list[[df]])[which(!(names(list[[df]]) %in% c("Country_Region")))] <- paste0(kpi, "__",names(list[[df]])[which(!(names(list[[df]]) %in% c("Country_Region")))])
  }
  rm(df)
  
  df <- data.frame(stringsAsFactors = FALSE)
  for (l in list){
    if (is_empty(df)){
      df <- l
    } else {
      df <- df %>% 
        left_join(l,
                  by = "Country_Region")
    }
  }
  
  df$Country_Region[which(df$Country_Region == "Myanmar")] <- "Burma"
  df$Country_Region[which(df$Country_Region == "Congo")] <- "Congo (Brazzaville)"
  df$Country_Region[which(df$Country_Region == "Democratic Republic of Congo")] <- "Congo (Kinshasa)"
  df$Country_Region[which(df$Country_Region == "Côte d'Ivoire")] <- "Cote d'Ivoire"
  df$Country_Region[which(df$Country_Region == "The Gambia")] <- "Gambia"
  df$Country_Region[which(df$Country_Region == "South Korea")] <- "Korea, South"
  df$Country_Region[which(df$Country_Region == "Taiwan, China, China")] <- "Taiwan*"
  df$Country_Region[which(df$Country_Region == "United States")] <- "US"
  
  names(df)[which(!(names(df) %in% c("Country_Region")))] <- paste0("Legatum__",names(df)[which(!(names(df) %in% c("Country_Region")))])
  
  return(df)
}


