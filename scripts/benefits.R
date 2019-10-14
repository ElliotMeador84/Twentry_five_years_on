library(tidyverse)
library(sf)
library(readxl)
library(tmap)
benefits <- read_excel("~/OneDrive - SRUC/Data/benefits/local_authority_benefits_combination_feb_19.xlsx")


uk_la <- st_read('~/OneDrive - SRUC/Data/geographic/boundaries/UK_local_authority/Local_Authority_Districts_April_2019_Boundaries_UK_BUC.shp')




uk_benefits <- uk_la %>% 
    left_join(benefits, by = c('lad19nm' = 'la'))

tm_shape(uk_benefits)+
    tm_fill('uc_out_of_work', palette = 'magma', n = 15)


names(benefits)



