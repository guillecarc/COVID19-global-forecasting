preprocess_traintest <- function(path = "./data/train_test/week4/"){
# Libraries and parameters ------------------------------------------------

require(openxlsx)
require(tidyverse)

# Load data ---------------------------------------------------------------

files <- list.files(path, 
                    full.names = TRUE, 
                    pattern = "train|test")

files_df_list <- map(files, read.csv,
                     stringsAsFactors = FALSE)

# Preprocess data ---------------------------------------------------------

files_df_list <- map(files_df_list,
                     ~{
                       .x$Date <- as.Date(.x$Date)
                       .x$Province_State[.x$Province_State == ""] <- NA
                       .x$Province_State <- factor(.x$Province_State)
                       .x$Province_State <- fct_explicit_na(.x$Province_State)
                       .x$Country_Region <- factor(.x$Country_Region)
                       .x$Country_Region <- fct_explicit_na(.x$Country_Region) 
                       return(.x)
                     })

files_df_list <- map2(files_df_list, basename(files),
                     ~{
                       .x$data_type <- NA
                       .x$data_type <- str_remove(.y, ".csv")
                       return(.x)
                     })

files_df_list <- set_names(files_df_list, basename(files))

files_df_list$test.csv <- rename(files_df_list$test, Id = ForecastId)

return(bind_rows(files_df_list))
}

         
         