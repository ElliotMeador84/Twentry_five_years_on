brary(tidyverse)
library(readxl)
library(glue)
library(shiny)
library(plotly)

source('/Users/emeador/OneDrive - SRUC/all_functions.R')


# process overview



## set working directory to data files



# Data are from

browseURL('https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8')


if(Sys.info()[[1]] == 'Windows'){
  setwd('C:/Users/emeador/OneDrive - SRUC/Data/ashe/home_geography/')
} else { ## Check the Mac root directory
  setwd('User/johne.meador/OneDrive - SRUC/Data/ashe/home_geography/')
  
}


# Read, clean and format files ------



x_files <- as.character(2002:2018)


results <- map_df(x_files, possibly(function(x){
  
  
  
  setwd(glue('C:/Users/emeador/OneDrive - SRUC/Data/ashe/home_geography/{x}'))
  
  
  # create column names
  names_column <- c('description',
                    'code',
                    'number_jobs_k',
                    'median',
                    'median_percentage_change',
                    'mean',
                    'mean_percentage_change',
                    glue('percentile_{seq(10,90,10)}'))
  
  # specify which sheets to pull
  
  names_sheet <- c('All',
                   'Male',
                   'Female',
                   'Full-Time',
                   'Part-Time',
                   'Male Full-Time',
                   'Male Part-Time',
                   'Female Full-Time',
                   'Female Part-Time')
  
  
  
  
  
  data_files <- dir() %>%
    str_subset(glue('{x}.xls$'))
  
  
  file_variables <- data_files %>%
    str_remove("PROV - Home Geography Table 8.") %>%
    str_remove_all('[[:digit:]]') %>%
    str_remove_all('\\ba\\b') %>%
    str_remove_all('.xls$') %>%
    str_remove_all('-') %>%
    str_squish() %>%
    str_to_lower() %>%
    str_replace_all(' ', '_')
  
  alfa <- map2(data_files,file_variables, possibly(function(.x, .z){
    map(names_sheet, function(.y){
      read_excel(.x,
                 sheet = .y,
                 trim_ws = T,
                 na = 'x',
                 skip = 4) %>%
        select(c(1:16)) %>%
        set_names(names_column) %>%
        mutate(sheet = .y) %>%
        mutate_if(is.numeric, as.character)
    } %>%
      mutate(file = .z))
    
  }, NULL))
  
  
  countries <- str_c(c('England','Scotland', 'Wales', 'Northern Ireland'))
  
  alfa %>%
    flatten_df() %>%
    select(file, sheet, everything()) %>%
    mutate_at(vars(number_jobs_k:percentile_90), list(~parse_number(.)))%>%
    mutate(country = case_when(
      description %in% countries ~ description, T~NA_character_)) %>%
    fill(country, .direction = 'down') %>%
    mutate(year = x)
  
}, NULL))




# Clean up some formatting ------

###############
## File Type ##
###############

file_to_remove <- str_c(c('^home_geography_table_._',
                          '^home_geography_table_._',
                          '^revised_home_geography_table_._'), collapse = '|')

results_i <- results %>%
  mutate(file = str_remove(file, file_to_remove),
         file = str_remove(file, '^paid_'),
         file = case_when(
           str_detect(file, 'weekly_pay_basic') ~ 'basic_pay_including_other_pay',
           T ~ file
         ),
         file = str_to_title(file)) %>%
  filter(str_length(description) < 50) %>% 
  mutate(file = str_replace_all(file, '_', ' '))



#################
## Description ##
#################




results_ii <- results_i %>%
  mutate(
    description = str_remove_all(description, '\\bUA\\b'),
    description = str_remove_all(description, '\\bCity of\\b'),
    description = str_remove_all(description, '\\bMc\\b'),
    description = str_replace_all(description, '\\band\\b', '&'),
    description = str_replace_all(description, 'Cardiff / Caerdydd', 'Cardiff'),
    description = str_remove_all(description, ','),
    description = str_remove_all(description, '/.*$')) %>%
  clean_council(description)%>%
  mutate(description = str_remove_all(description, '\\bMc\\b'),
         description = str_squish(description)) %>%
  mutate(description = case_when(
    str_detect(description, 'Scottish Borders') ~ 'Scottish Borders',
    str_detect(description, 'Western Isles') ~ 'Eilean Siar',
    T ~ description
  )) %>% 
  rename(area = description) %>% 
  mutate(year = parse_number(year))



ASHE_Table_8_Complete <- results_ii


ASHE_Table_8_tidy <- ASHE_Table_8_Complete %>% 
  gather(statistic, value, -c(file:number_jobs_k, year, country)) %>% 
  mutate(statistic = str_replace_all(statistic, '_', ' '), 
         statistic = str_to_title(statistic))



# Save the data

if(Sys.info()[[1]] == 'Windows'){
  setwd('C:/Users/emeador/OneDrive - SRUC/Data/ashe/home_geography')
} else {
  setwd('Users/johne.meador/OneDrive - SRUC/Data/ashe/home_geography')
  
}




write_csv(ASHE_Table_8_Complete, 'ASHE_Table_8_Complete.csv')
write_csv(ASHE_Table_8_tidy, 'ASHE_Table_8_tidy.csv')





if(Sys.info()[[1]] == 'Windows'){
  setwd('C:/R/Twentry_five_years_on/')
} else {
  setwd('User/johne.meador/Documents/R/Twentry_five_years_on/')
  
}













