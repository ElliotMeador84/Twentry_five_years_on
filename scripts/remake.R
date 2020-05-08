


# Libraries, functions & data -------

library(tidyverse)
library(janitor)
library(openxlsx)
library(glue)
library(flextable)
library(scales)
library(ggrepel)

library(sf)
library(tmap)
library(tmaptools)
library(ggspatial)

source('~/OneDrive - SRUC/all_functions.R')

setwd('~/OneDrive - SRUC/Twenty_five_years_on/figure_remake/')


## ASHE data



ASHE_Table_8_tidy <-
  read_csv('~/OneDrive - SRUC/Data/ashe/home_geography/ASHE_Table_8_tidy.csv')

## Deprivation Data

dep_england <-
  st_read(get_data('deprivation/england/english_deprivation_score_sf.shp'))

dep_scotland <-
  st_read(get_data('deprivation/scotland/simd2020_withgeog/simd2020.gpkg'))

scotland_dep_df <-
  read_csv(get_data('deprivation/scotland/historic/scotland_2020_simd.csv')) %>%
  clean_names()

england_dep_df <-
  read.xlsx(get_data('deprivation/england/england_dep_scores_2019.xlsx'),
            sheet = 'IoD2019 Scores') %>%
  as_tibble() %>%
  clean_names()

# Population


uk_population_change <- read.xlsx(
  get_data('population/UK_midyear_population.xlsx'),
  sheet = 'MYE 5',
  startRow = 5,
  colNames = T
) %>%
  as_tibble() %>%
  clean_names()

uk_age_change <-
  read.xlsx(
    get_data('population/UK_midyear_population.xlsx'),
    sheet = 'MYE 6',
    startRow = 5,
    colNames = T
  ) %>%
  as_tibble() %>%
  clean_names()






# Theme, colors, etc. ----------------



pop_theme <- function(...) {
  theme_classic() +
    theme(
      text = element_text(
        size = 13.5,
        color = 'black',
        family = 'serif'
      ),
      axis.text.x =
        element_text(angle = 45,
                     hjust = 1),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(
        angle = 0,
        hjust = 1,
        face = 'bold'
      ),
      axis.title.x = element_text(face = 'bold'),
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      strip.background = element_blank(),
      strip.text.y = element_text(angle = 0),
      plot.title = element_text(face = 'bold'),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.caption = element_text(face = 'italic', hjust = 0)
    ) +
    theme(...)
}



case_study_cols <- c(Spectral[c(1, 7, 11)],
                     adjustcolor(Greys[2], .25))

names(case_study_cols) <- c("Na h-Eileanan Siar",
                            "Northumberland",
                            "Perth and Kinross",
                            "Other")

# Employment deprivation ------------

dep_england_tidy <- england_dep_df %>%
  select(name = local_authority_district_name_2019,
         income_rate = income_score_rate,
         employment_rate = employment_score_rate) %>%
  mutate(case_study = case_when(
    str_detect(name,
               'Northumberland') ~ 'Northumberland',
    T ~ 'Rest of England-Scotland'
  ))



dep_scotland_tidy <- scotland_dep_df %>%
  select(name = council_area,
         employment_rate,
         income_rate) %>%
  mutate(case_study =
           case_when(
             str_detect(name, 'Perth and Kinross') ~ name,
             str_detect(name, 'Na h-Eileanan an Iar') ~ name,
             T ~ 'Rest of England-Scotland'
           ))



# Income deprivation ---------------


northumberland_sf <-  dep_england %>%
  filter(str_detect(lso11nm, 'Northumberland'))


scotland_sf <- dep_scotland


perthshire <- scotland_sf %>%
  filter(str_detect(council_area,
                    'Perth and Kinross'))


Na_h_Eileanan_an_Iar <- scotland_sf %>%
  filter(str_detect(council_area,
                    'Na h-Eileanan an Iar'))

Na_h_Eileanan_an_Iar_simp <-
  rmapshaper::ms_simplify(Na_h_Eileanan_an_Iar)

northumberland_employment <-
  northumberland_sf %>%
  select(Name = lso11nm,
         contains('emp'),
         income_rate = incm_dprvt) %>%
  rename(employment_rate = emp__)


union_ls <- map(list(perthshire,
                     Na_h_Eileanan_an_Iar,
                     northumberland_employment),
                function(x) {
                  st_union(x)
                }) %>%
  set_names(c(
    'perthshire_union',
    'nah_eileanan_union',
    'northumberland_union'
  ))



place_names <- c('Perthsire',
                 'Nah Eileanan an Iar',
                 'Northumberland')


income_employ_dep <-
  dep_england_tidy %>%
  bind_rows(dep_scotland_tidy)


# Employment table ---------
income_employ_dep %>%
  group_by(case_study) %>%
  summarise(
    Median =
      median(employment_rate,
             na.rm = T),
    Mean =
      mean(employment_rate,
           na.rm = T),
    Minimum =
      min(employment_rate,
          na.rm = T),
    Maximum =
      max(employment_rate,
          na.rm = T)
  ) %>%
  mutate_if(is.numeric,
            list(~ percent(.))) %>%
  rename('Case study area' =
           case_study) %>%
  flextable(cwidth = c(2, 1, 1, 1, 1)) %>%
  set_caption('Proportion of Employment Deprivation in case study areas') %>%
  footnote(
    i = c(1, 3),
    j = 1,
    ref_symbols = '1',
    value = as_paragraph(c(
      'Data are from Scottish Index of Multiple Deprivation 2020.'
    ))
  ) %>%
  footnote(
    i = c(2),
    j = 1,
    ref_symbols = '2',
    value = as_paragraph(c(
      'Data are from (England) Index of Multiple Deprivation 2019.'
    ))
  )


# Employment map ------------
employment_ls <-
  list(perthshire,
       Na_h_Eileanan_an_Iar,
       northumberland_employment)


title_ls <-
  glue('{place_names} - Employment Rate')

places_df <-
  tibble(
    name =
      c('Tarbert',
        'Perth',
        'Blairgowrie',
        'Bellingham'),
    latitude =
      c(57.897851,
        56.391221,
        56.589531,
        55.145715),
    longitude =
      c(-6.807777, -3.432775, -3.339232, -2.254733),
    authority =
      c('eileanan',
        'perth',
        'perth',
        'northumberland')
  )

perth_places <-
  filter(places_df,
         authority == 'perth')
eileanan_places <-
  filter(places_df,
         authority == 'eileanan')
northumberland_places <-
  filter(places_df,
         authority == 'northumberland')



places_ls <- list(perth_places,
                  eileanan_places,
                  northumberland_places) %>%
  map(function(x) {
    st_as_sf(
      x,
      coords = c("longitude",
                 "latitude"),
      crs = st_crs(employment_ls[[1]]),
      agr = "constant"
    )
  }) %>%
  set_names(c('perth',
              'eileanan',
              'northumberland') %>%
              paste0(., '_places'))



## custom colour functions
tran.log10 <-
  function(x) {
    num. <- x * 100
    num. <- log10(num.)
    num. <- num. / 100
    return(num.)
  }


lab.log10 <- function(x) {
  percent(x * 10)
}


# map across the three case-study areas

## EMPLOYMENT
employment_maps_ls <-
  map(1:3, function(x) {
    ggplot() +
      geom_sf(data = employment_ls[[x]],
              aes(fill =
                    tran.log10(employment_rate)),
              size = NA) +
      geom_sf_label(
        data = places_ls[[x]],
        aes(label = name),
        color = 'black',
        size = 2,
        nudge_x = .05,
        nudge_y = .05
      ) +
      scale_fill_viridis_c(name = 'Employment Rate',
                           labels = lab.log10) +
      labs(title =
             glue('{title_ls[[x]]}'),
           x = NA,
           y = NA) +
      theme_void() +
      theme(plot.margin =
              margin(.5, .5, .5, .5, 'cm')) +
      ## scale & compass
      annotation_scale(location = "bl",
                       width_hint = 0.5) +
      coord_sf()
  }) %>%
  set_names(glue('{names(places_ls)}_employment_map'))


## Save files
map2(employment_maps_ls,
     names(employment_maps_ls),
     function(x, y) {
       ggsave(
         x,
         filename =
           glue('employment_maps/{y}.pdf'),
         width = 6,
         height = 6
       )
     })

# Income map -----------
#
##INCOME
income_maps_ls <-
  map(1:3, function(x) {
    ggplot() +
      geom_sf(data = employment_ls[[x]],
              aes(fill =
                    tran.log10(income_rate)),
              size = NA) +
      geom_sf_label(
        data = places_ls[[x]],
        aes(label = name),
        color = 'black',
        fill = adjustcolor('white', 0.5),
        size = 2,
        nudge_x = .05,
        nudge_y = .05
      ) +
      scale_fill_viridis_c(name = 'Income Rate',
                           labels = lab.log10) +
      labs(title =
             glue('{title_ls[[x]]}'),
           x = NA,
           y = NA) +
      theme_void() +
      theme(plot.margin =
              margin(.5, .5, .5, .5, 'cm')) +
      ## scale & compass
      annotation_scale(location = "bl",
                       width_hint = 0.5)
  }) %>%
  set_names(glue('{names(places_ls)}_income_map'))


## Save files
map2(income_maps_ls,
     names(income_maps_ls),
     function(x, y) {
       ggsave(
         x,
         filename =
           glue('income_maps/{y}.png'),
         width = 6,
         height = 6
       )
     })

# Income table ----------
dep_england_tidy %>%
  bind_rows(dep_scotland_tidy) %>%
  group_by(case_study) %>%
  summarise(
    Median =
      median(income_rate,
             na.rm = T),
    Mean =
      mean(income_rate,
           na.rm = T),
    Minimum =
      min(income_rate,
          na.rm = T),
    Maximum =
      max(income_rate,
          na.rm = T)
  ) %>%
  mutate_if(is.numeric,
            list(~ percent(.))) %>%
  rename('Case study area' =
           case_study) %>%
  flextable(cwidth = c(2, 1, 1, 1, 1)) %>%
  set_caption('Proportion of Employment Deprivation in case study areas') %>%
  footnote(
    i = c(1, 3),
    j = 1,
    ref_symbols = '1',
    value = as_paragraph(c(
      'Data are from Scottish Index of Multiple Deprivation 2020.'
    ))
  ) %>%
  footnote(
    i = c(2),
    j = 1,
    ref_symbols = '2',
    value = as_paragraph(c(
      'Data are from (England) Index of Multiple Deprivation 2019.'
    ))
  )






# Employment-Income MAP2TABLE ----------

employment_map_df <-
  map_df(1:3, function(x) {
  
     places_ls[[x]] <-
      places_ls[[x]] %>%
      st_transform(crs =
        st_crs(employment_ls[[x]])) 
    
    st_join(employment_ls[[x]],
            places_ls[[x]],
            .predicate =
              st_intersects) %>%
      as_tibble() %>%
      mutate(name = replace_na(name,
      glue('Rest of {place_names[[x]]}')))
  }) %>% 
  select(-geometry)



places_update <- 
  read_csv('places_update.csv')

places_order <- places_update %>%
  distinct(authority) %>%
  pull(authority) %>%
  .[c(3, 2, 1.)]

employment_map_df %>%
  group_by(name) %>%
  summarise(
    employment_rate =
      mean(employment_rate,
           na.rm = T),
    income_rate =
      mean(income_rate,
           na.rm = T),
    over_crowd_rate =
      mean(overcrowded_rate,
           na.rm = T)
  ) %>%
  mutate_if(is.numeric, percent) %>%
  left_join(places_update, by = 'name') %>%
  arrange(match(authority,
                places_order)) %>%
  select(authority, name, everything()) %>%
  set_names(c(
    'Council Area',
    'Name',
    'Employment Rate',
    'Income Rate',
    'Overcrowded Rate'
  )) %>%
  mutate_if(is.character, str_to_title) %>%
  flextable(cwidth = c(1.75, 2.15, 1, 1, 1.1))

# ASHE  Table ------------------
case_study_df <-
  ASHE_Table_8_tidy %>%
  mutate(
    case_study = case_when(
      str_detect(area, 'Perth Kinross') ~ 'Perth and Kinross',
      str_detect(area, 'Eilean Siar') ~ 'Na h-Eileanan Siar',
      str_detect(area, 'Northumberland') ~ 'Northumberland',
      T ~ 'Rest of UK'
      
    )
  ) %>%
  drop_na(country)


case_study_df %>%
  filter(year >= max(year) - 3,
         sheet == 'All') %>%
  group_by(file, case_study) %>%
  summarise(mean = mean(value, na.rm = T)) %>%
  rowid_to_column() %>%
  spread(case_study, mean) %>%
  ungroup() %>%
  fill(names(.)) %>%
  drop_na() %>%
  mutate_if(is.numeric, function(x)
    dollar(x, prefix = '£')) %>%
  select(-rowid) %>%
  rename('Hours and Earnings\nIndicator' = file) %>%
  flextable(cwidth = c(2, 1.25, 1.25, 1.25, 1.25)) %>%
  set_caption('Comparing case study areas to the rest of the UK - 2015 through 2018 mean values shown')

# ASHE Plot -----------


case_study_cols <- viridis_pal()(3)

names(case_study_cols) <-
  bind_rows(example_tidy) %>%
  distinct(area) %>%
  pull(1)


## bring in the national average
case_study <- c('Perth Kinross',
                'Northumberland',
                'Eilean Siar')



ASHE_Scotland_England <-
  ASHE_Table_8_tidy %>%
  filter(country %in% c('England',
                        'Scotland'),
         statistic == 'Median') %>%
  arrange(area, sheet, year) %>%
  filter(
    sheet == 'All',!file %in% c('Overtime pay',
                                'Hours worked overtime',
                                'Annual pay incentive'),
    year >= 2002
  ) %>%
  mutate(base = case_when(year == min(year, na.rm = T) ~ value,
                          T ~ NA_real_)) %>%
  fill(base, .direction = 'down') %>%
  mutate(diff = value - base) %>%
  distinct() %>%
  group_by(country, file, year) %>%
  summarise(diff = median(diff, na.rm = T)) %>%
  ungroup() %>%
  rename(area = country) %>%
  mutate(area = 'Scotland/England median') %>%
  filter(!file %in%
           c('Annual pay incentive',
             'Overrtime pay')) %>%
  ungroup() %>%
  mutate(file = case_when(str_detect(file, 'pay') ~
                            paste(file, '(£)'),
                          T ~ file))


example <- ASHE_Table_8_tidy %>%
  filter(area  %in% case_study,
         statistic == 'Median')


example_tidy <- example %>%
  arrange(area, sheet, year) %>%
  filter(
    sheet == 'All',!file %in% c('Overtime pay',
                                'Hours worked overtime',
                                'Annual pay incentive'),
    year >= 2002
  ) %>%
  mutate(base = case_when(year == min(year, na.rm = T) ~ value,
                          T ~ NA_real_)) %>%
  fill(base, .direction = 'down') %>%
  mutate(diff = value - base) %>%
  split(.$file)



(
  ASHE_Faceted_plot <-
    bind_rows(example_tidy) %>%
    filter(!file %in%
             c('Annual pay incentive',
               'Overrtime pay')) %>%
    ungroup() %>%
    mutate(file = case_when(
      str_detect(file, 'pay') ~
        paste(file, '(£)'),
      T ~ file
    )) %>%
    ggplot(aes(
      year, diff,
      color = area,
      group = area
    )) +
    geom_smooth(se = F,
                size = 1.25,
                alpha = .5) +
    geom_smooth(
      data = ASHE_Scotland_England,
      se = F,
      color = 'black',
      size = 1.25,
      linetype = 3
    ) +
    scale_x_continuous(
      breaks = seq(2002, 2018, 4),
      labels = seq(2002, 2018, 4)
    ) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(
      values = case_study_cols,
      name = 'Case Study Local Authority',
      labels = c('Outer Hebrides',
                 'Northumberland',
                 'Perth and Kinross')
    ) +
    facet_wrap( ~ file,
                scales = 'free',
                ncol = 2) +
    pop_theme(plot.margin =
                margin(1, 1.5, 1, 1, 'cm')) +
    theme(legend.position = 'bottom') +
    labs(
      title = 'Yearly Change in Pay and Hours Worked Across Case-Study Areas',
      subtitle = 'SOURCE: Annual Survey of Hours & Earnings 2002 - 2018',
      x = 'Year',
      y = 'Median Change\nover time',
      caption = 'Value for all lines in 2002 is zero.\nLines are Locally Weighted Scatterplot Smoothing (LOESS) lines.\nBlack dotted lines show English-Scottish average.'
    )
)


ggsave(
  ASHE_Faceted_plot,
  filename = 'ashe/ASHE_Faceted_plot.png',
  width = 8.6,
  height = 11
)


# Population ----------------


search_terms <- str_c(c('Na h-Eileanan Siar',
                        'Perth and Kinross',
                        'Northumberland'),
                      collapse = '|')


case_study_population <- uk_population_change %>%
  select(code,
         name,
         geography = geography1,
         contains('estimated_population_mid')) %>%
  gather(year, population,-code,-name,-geography) %>%
  mutate(year = parse_number(year)) %>%
  filter(str_detect(name, search_terms))




population_df <- case_study_population %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(min_year = case_when(year == min(year) ~ population,
                              T ~ NA_real_)) %>%
  fill(min_year, .direction = 'up') %>%
  mutate(pop_change = population - min_year) %>%
  ungroup()



case_study_density <- uk_population_change %>%
  select(code,
         name,
         geography = geography1,
         contains('people_per_sq_km')) %>%
  gather(year, per_sq_k,-code,-name,-geography) %>%
  mutate(year = parse_number(year)) %>%
  filter(str_detect(name, search_terms))




density_df <- case_study_density %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(min_year = case_when(year == min(year) ~ per_sq_k,
                              T ~ NA_real_)) %>%
  fill(min_year, .direction = 'up') %>%
  mutate(per_sq_k_change = per_sq_k - min_year) %>%
  ungroup()


case_study_age <- uk_age_change %>%
  rename(geography = geography1) %>%
  gather(year, age,-code,-name,-geography) %>%
  mutate(year = parse_number(year)) %>%
  filter(str_detect(name, search_terms))



age_df <- case_study_age %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(min_year = case_when(year == min(year) ~ age,
                              T ~ NA_real_)) %>%
  fill(min_year, .direction = 'up') %>%
  mutate(age_change = age - min_year) %>%
  ungroup()


# Population Table ----------------







population_df %>%
  filter(year == max(year)) %>%
  select(
    name,
    population_2001 = min_year,
    population_2018 = population,
    change = pop_change
  ) %>%
  mutate(percent_change =
           percent(change / population_2001)) %>%
  mutate_if(is.numeric, comma) %>%
  select(-change) %>%
  set_names(c(
    'Local\nauthority',
    'Population\n2002',
    'Population\n2018',
    'Percent \nchange'
  )) %>%
  flextable(cwidth = c(2, 1, 1, 1))






case_study_cols <- scales::viridis_pal()(3)

names(case_study_cols) <- population_df %>%
  count(name) %>%
  pull(name)


pop_change_label_df <- population_df %>%
  filter(year == max(year)) %>%
  select(year, name, pop_change) %>%
  mutate(name =
           case_when(str_detect(name, 'Na h-Eilean') ~ 'Outer Hebrides', T ~ name))

population_df %>%
  ggplot(aes(year,
             pop_change,
             group = name,
             color = name)) +
  geom_hline(yintercept = 0, linetype = 4) +
  geom_line(size = 1.5, show.legend = F) +
  geom_point(size = 4.25, show.legend = F) +
  geom_text(
    data = pop_change_label_df,
    aes(year,
        pop_change,
        label = name),
    color = 'black',
    show.legend = F,
    nudge_x = .5,
    hjust = 0
  ) +
  scale_x_continuous(breaks = 2001:2018) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = case_study_cols) +
  pop_theme(plot.margin = margin(1, 5, 1, 1, 'cm')) +
  labs(
    title = 'Population has increased for all case study local authorities since 2001',
    subtitle = 'SOURCE: Office of National Statistics (ONS)',
    x = 'Year',
    y = 'Population\nchange'
  ) +
  coord_cartesian(clip = 'off')

# Scotland Multiple Deprivation ----------


all_rds_simd_path <- 
  list.files('~/Downloads/',                                full.names = T) %>%
  str_subset('DataZone') %>%
  str_subset('.rds') %>%
  .[-6]


dz_2001_2011_lookup <-
  read_csv(get_data('lookup/dz_2001_2011_wide.csv'))

la_dz_lookup <-
  read_csv("~/OneDrive - SRUC/Data/lookup/Local_authority_dz_int_lookup.csv")


simd_all_years <-
  map(all_rds_simd_path,
      function(x) {
        file <-  read_rds(x) %>%
          rename(data_zone = 1)
        
        file %>%
          left_join(dz_2001_2011_lookup,
                    by = c('data_zone'
                           = 'dz_2001'))
      })


simd_alfa <- simd_all_years %>%
  map(possibly(function(x) {
    if (nrow(x) == 9479) {
      x %>%
        select(-data_zone) %>%
        select(data_zone = dz_2011,
               everything()) %>%
        distinct(data_zone,
                 .keep_all = T)
    } else {
      x %>%
        select(data_zone,
               everything()) %>%
        distinct(data_zone,
                 .keep_all = T)
    }
  }, NULL))


simd_years <-
  basename(all_rds_simd_path) %>%
  str_remove_all('DataZone2001') %>%
  str_remove_all('DataZone2011') %>%
  parse_number()


income_simd <- map2_df(simd_alfa,
                       simd_years,
                       possibly(function(x, y) {
                         income <-    x %>%
                           select(data_zone,
                                  contains('inc_score'),
                                  contains('inc_rate')) %>%
                           rename(income_rate = 2) %>%
                           mutate(year = y) %>%
                           left_join(la_dz_lookup %>%
                                       select(data_zone, local_authority),
                                     by = 'data_zone') %>%
                           clean_council(local_authority)
                         
                         
                         if (mean(income$income_rate,
                                  na.rm = T) < 1) {
                           income %>%
                             mutate(income_rate = income_rate * 100)
                         } else {
                           income
                         }
                         
                       }, NULL))



scotland_income_deprivation <- income_simd %>%
  mutate(
    case_study = case_when(
      str_detect(local_authority, 'Eilean Siar') ~ 'Outer Hebrides',
      str_detect(local_authority, 'Perth Kinross') ~ 'Perth and Kinross',
      T ~ 'Rest of Scotland'
    )
  ) %>%
  group_by(case_study, year) %>%
  summarise(mean = mean(income_rate,
                        na.rm = T),
            median = median(income_rate,
                            na.rm = T))




scot_dep_cols <-
  c(case_study_cols[c(1, 3)],
    Greys[4])


names(scot_dep_cols) <-
  c('Outer Hebrides',
    'Perth and Kinross',
    'Rest of Scotland')


labels_df <-
  scotland_income_deprivation %>%
  filter(year == max(year))




(
  scotland_income_deprivation_plot <-
    scotland_income_deprivation %>%
    ggplot(aes(year, 
               median, 
               color = case_study)) +
    geom_line(size = 2, 
              show.legend = F) +
    geom_point(size = 4, 
               show.legend = F) +
    geom_text(
      data = labels_df,
      aes(year, median, label = case_study),
      nudge_x = 2.25,
      color = 'black'
    ) +
    scale_y_continuous(
      labels = percent_sign) +
    scale_color_manual(
      values = scot_dep_cols,
                       name = 'Case Study Area') +
    pop_theme(
      plot.margin = 
        margin(1, 2, 1, 1, 'cm')) +
    coord_cartesian(
      clip = 'off') +
    labs(
      title = 'Percent of Income-Deprived Across Scottish Case Study Areas',
      subtitle = 'SOURCE: Scottish Index of Multiple Deprivation (SIMD) Income Rates',
      x = 'Year',
      y = '\n\nMedian percent\nincome deprived',
      caption = 'All data is aggregated to 2011 datazone geographies. '
    )
)


ggsave(
  scotland_income_deprivation_plot,
  filename = 'deprivation/scotland/scotland_income_deprivation_plot.png',
  width = 9,
  height = 6.5
)


open_with('deprivation/scotland/scotland_income_deprivation_plot.png',
          'Preview')

# Deprivation cluster maps ------------
 
    #Scotland

simd2020_sf <- st_read(get_data('deprivation/scotland/simd2020_withgeog/simd2020.gpkg'))

simd_2020_numeric <-
  simd2020_sf %>% 
 as_tibble() %>% 
  select_if(is.numeric) %>% 
  select(-contains('rate')) %>%
  map_df(function(x){
    scale(x)
  })


simd_2020_numeric_no_na <- 
map_df(simd_2020_numeric, function(x) { 
  x[is.na(x)] <- 
     mean(x, na.rm = TRUE)
  x
})


simd_2020_numeric_no_na %>% 
  map(function(x)sum(is.na(x)))




 # specify clusters
cluster_n <- 5

simd_2020_kmeans <- 
  kmeans(simd_2020_numeric_no_na, 
         cluster_n, nstart = 20)

table(simd_2020_kmeans$cluster)


cluster_label_df <- 
  simd_2020_kmeans$centers %>% 
  as_tibble() %>% 
  mutate(cluster =
  letters[1:cluster_n]) %>%
  select(cluster, everything()) %>% 
  gather(key, value, -cluster) %>%
  group_by(key) %>% 
  filter(value == max(value)) %>% 
  ungroup() %>% 
  split(.$cluster) %>% 
  map_df(function(x){
    x %>% 
      arrange(key) %>% 
      mutate(key = str_to_sentence(
        str_replace_all(key, '_', ' '))) %>% 
      pull(key) %>% 
      paste(collapse = '; ')
  }) %>% 
  gather(cluster, label)

cluster_label_df

simd2020_sf$cluster <- 
  simd_2020_kmeans$cluster



simd_cluster_map <- simd2020_sf %>% 
  ggplot(aes(fill = 
               as.character(cluster)))+
  geom_sf(color = NA)+
  scale_fill_viridis_d()+
  theme_minimal()+
  labs(title = 'Deprivation cluster results', 
       fill = 'Cluster')

ggsave(simd_cluster_map, 
       filename = 'simd_cluster_map.pdf', 
       height = 8, 
       width = 6)

open_with('simd_cluster_map.pdf', 'Preview')



  
  
  