library(tidyverse)
library(readxl)

## Unzip the file and save the files
unzip('C:/Users/emeador/Downloads/table82018provisional (2).zip', 
      exdir = 'data')

## Get sheet names 
excel_sheets("data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls")






#############################################
### Iterate and pull names for each sheet ###
#############################################
# function credit to https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames


read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    map(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
}

# process overview

x <- read_excel("data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls", 
           sheet = 2, trim_ws = T) %>% 
    slice(1:4) 

x_i <- t(x) %>% 
    as_tibble()
    

x_i[is.na(x_i)] <- ''


x_names <- x_i %>% 
  unite(name, c('V1', 'V2', 'V3', 'V4'), sep = ' ') %>% 
    mutate(name = str_trim(name)) %>% 
    pull(name)

############################################
### Iterate and pull data for each sheet ###
############################################


# Add the names from above to each sheet

y <- read_excel("data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls", 
           sheet = 2, trim_ws = T, skip = 5)




names(y) <- x_names

y %>% 
    janitor::clean_names() %>% 
    View()

######################
## Needs Completing ##
######################


 # 1. Import all data and clean
 # 2. Assign data with names


### Need to run this for all tables in the data directory

# Import sheet names ------------


sheet_names <- 
    excel_sheets("data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls")

worksheet_names_ls <- map(sheet_names, function(x){
    alfa <- 
        read_excel(
    "data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls", 
               sheet = x, 
               trim_ws = T) %>% 
        slice(1:4) %>% 
        t(.) %>% 
        as_tibble()
    
    alfa[is.na(alfa)] <- ''
    
    alfa %>% 
        unite(name, c('V1', 'V2', 'V3', 'V4'), sep = ' ') %>% 
        mutate(name = str_trim(name)) %>% 
        pull(name)
    
})

# Import sheets ------------

home_geography_ls <- map(sheet_names, function(x){
    read_excel("data/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls", 
               sheet = x, 
               trim_ws = T) 
    }) 

 ## Merge with names

home_geography_ls_i <- map2(home_geography_ls,
     worksheet_names_ls, 
     function(x, y){
    x %>% 
        set_names(y) %>% 
             set_names(make.unique(names(.))) %>% 
             janitor::clean_names()
})

# add variable for binding

map2_df(home_geography_ls_i[-1], 
     sheet_names[-1], 
     function(x, y){
          x %>%
             mutate(sheet = y)
}) %>% 
    drop_na(description) %>% 
    select(-x, -x1, -x2) %>% 
    filter(description != 'Description') %>% 
    mutate_at(vars(number_of_jobsb_thousand:x90), list(~as.numeric(.))) %>% 
    mutate_if(is.numeric, list(~round(., 2))) %>% 
    View()


# Map over all Table 8 files -------------

dir.remove <- dir('data') %>% 
    str_subset('2018 CV')

ASHA_Table.8_xls_all <- dir('data')[!dir('data') %in% dir.remove] 

ASHA_Table.8_xls_all <- glue::glue('data/{ASHA_Table.8_xls_all}')




# get sheet names
sheet_names_ls <- map(ASHA_Table.8_xls_all, function(x){
    excel_sheets(x) %>% 
        .[-1]
})


# get file names



all_file_names_ls <- map(ASHA_Table.8_xls_all, function(x){
    sheets <- excel_sheets(x)
    map(sheets, function(y){
       alfa <-  read_excel(
            x, 
            sheet = y, 
            trim_ws = T) %>% 
            slice(1:4) %>% 
            t(.) %>% 
            as_tibble()
        alfa[is.na(alfa)] <- ''

        alfa %>%
            unite(name, c('V1', 'V2', 'V3', 'V4'), sep = ' ') %>%
            mutate(name = str_trim(name)) %>%
            pull(name)
    })
        

    
})

# get all files

all_file_ls <- 
    map(ASHA_Table.8_xls_all, function(x){
    sheets <- excel_sheets(x)
    map(sheets, function(y){
        alfa <-  read_excel(
            x, 
            sheet = y, 
            trim_ws = T) %>% 
            slice(5:nrow(.))
    })
    
    
    
})


map(all_file_ls, function(x){
    map(x, length)
})


##############################
#### Filter the list here ####
##############################
############################## 


map(all_file_ls, function(x){
    map(x, function(y){
         y %>% 
            select(-1)
    })
})




names(all_file_ls)
 
names(all_file_ls) <- 
    str_remove(ASHA_Table.8_xls_all, 
           "data/PROV - Home Geography Table 8.") %>% 
  str_remove_all('[[:digit:]]') %>% 
  str_remove_all('\\ba\\b') %>% 
  str_remove_all('.xls$') %>% 
  str_remove_all('-') %>% 
    str_squish() %>% 
    str_to_lower() %>% 
    str_replace_all(' ', '_')


ls_df_names_eq_10 <- which(map(all_file_ls, length) == 10) %>% 
    names()

ls_names_eq_10 <- which(map(sheet_names_ls, length) == 9) 

# pull names for each sheet for tables that have 10 sheets
names_sheet_10 <- 
    sheet_names_ls[ls_names_eq_10][1] %>% 
    flatten_chr() %>% 
    str_replace_all('-', '_') %>% 
    str_replace_all(' ', '_') %>% 
    str_to_lower() %>% 
    .[-1]




df_eq_10_ls <- all_file_ls[ls_df_names_eq_10]

for (i in seq_along(df_eq_10_ls)){
    names(df_eq_10_ls[[i]]) <- names_sheet_10
}




all_file_names_ls[[1]][[2]] %>% 
    str_replace_all('-', '_') %>% 
    str_replace_all(' ', '_') %>% 
    str_to_lower()



column_names <- c('description', 
  'code',
  'number_jobs_in_k',
  'median', 
  'annual_median_change', 
  'mean', 
  'annual_mean_change', 
  glue::glue('percentile_{seq(0, 100, 10)}'), 
  'other_1',
  'other_2')




df_eq_10_ls 

which(names(df_eq_10_ls) == 'notes')



map(df_eq_10_ls, function(.x){
    names(df_eq_10_ls) != 'notes'})



df_eq_10_ls[1:11][[1]] %>% names()

map(df_eq_10_ls, function(.x){
    map(.x, function(.x){
        .x %>% 
            length()
    })    
})


map_if(df_eq_10_ls, is.data.frame, ~ .x %>% 
           set_names(org_names),
       .else = ~ map(.x, set_names, org_names))


##########################################
###  Need to filter the list of lists ####
##########################################










