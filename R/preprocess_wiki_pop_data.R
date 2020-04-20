preprocess_wiki_pop_data <- function(path = "./data/Wikipedia metadata/region_metadata.csv"){
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- df %>% 
    group_by(Country_Region) %>% 
    summarise(Wiki__pop = sum(population),
              Wiki__area = sum(area)) %>% 
    mutate(Wiki__density = Wiki__pop / Wiki__area)
  
  return(df)
}