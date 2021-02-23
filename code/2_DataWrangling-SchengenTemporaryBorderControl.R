# Data wrangling & visualization

# Downloaded on 2021/02/02
# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Notes
### ------------------------------------------------------------------------ ###

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "qdap", "rio", "countrycode", "janitor", "lubridate", "rvest",
            "patchwork", "tidytext", "eurostat", "sf", "ggraph", "tidygraph", "igraph",
            "ggwaffle")

# Load data
# created in:
# - 1_DataCreate-SchengenTemporaryBorderControl_2006-2020.R
# - 1a_DataCreate-SchengenTemporaryBorderControl_2000-2006.R

### ------------------------------------------------------------------------ ###

# Load data
# 2006-2020
bcontrol_1a.df <- import("./data/TemporaryBorderClosures 2006-2021.xlsx")

# 2000-2006
# make compatible
bcontrol_1b.df <- import("./data/TemporaryBorderClosures 1996-2006.xlsx") %>%
  mutate(nb = nb_a * -1,
         category = if_else(category == "pol_meeting", "pol_event", category)) %>%
  select(-notes, -availability, -link, -nb_a, -document_id)

# Delete three cases that overlap in October and November 2006
bcontrol_1b.df <- bcontrol_1b.df %>%
  filter(!nb %in% c(-1, -2, -3))

# Join data from 2000-2006 and 2006-2021
bcontrol.df <- bcontrol_1a.df %>%
  bind_rows(bcontrol_1b.df) %>%
  mutate(land_border_control = ifelse(land_border_control == "T", TRUE, FALSE)) %>%
  filter(land_border_control == TRUE) %>%
  select(-land_border_control)

# Delete closures against countries that are not yet member states
bcontrol.df <- bcontrol.df %>%
  mutate(control_before_implementation = ymd(bcontrol.df$begin) < ymd(bcontrol.df$implemented_neighbour)) %>%
  filter(control_before_implementation == FALSE) %>%
  select(-control_before_implementation)

# Remove unnecessary objects
rm(list = setdiff(ls(), c("bcontrol.df", "theme.basic")))

# Additional variables & data wrangling
### ------------------------------------------------------------------------ ###
# Turn closures that span multiple years into separate columns
# see: https://stackoverflow.com/questions/60828291/split-durations-into-annual-parts
# (A) Save original date column
bcontrol.df <- bcontrol.df %>%
  mutate(duration = paste(begin, end, sep = " - "),
         duration = if_else(duration == "NA - NA", NA_character_, duration))

# (B) Adjust column 'begin' to start at the beginning of the new year
bcontrol_myear.df <- bcontrol.df %>%
  filter(year(end) != year(begin)) %>%
  mutate(begin = ceiling_date(ymd(begin), "year"), begin)

# (C) Adjust column 'end' to end at the turn of the year
bcontrol.df <- bcontrol.df %>%
  mutate(end = if_else(year(end) != year(begin), 
                       ceiling_date(ymd(begin), "year") - days(1), ymd(end)))

# (D) Bind together and arrange
bcontrol.df <- bcontrol.df %>%
  bind_rows(bcontrol_myear.df) %>%
  arrange(nb)

# Compute length of closure in days
# Note: +1 day to account for intervals that last only one day
bcontrol.df <- bcontrol.df %>%
  mutate(check_length = interval(ymd(begin), ymd(end)) / days(1),
         check_length = if_else(check_length == 0, 1, check_length)) %>%
  select(nb, begin, end, check_length, everything())

# Dictionary method to classify stated reasons for closure
### ------------------------------------------------------------------------ ###
# Add a ID to the original data for merging
bcontrol.df <- bcontrol.df %>%
  mutate(id = 1:n()) %>% 
  select(id, everything())

# Clean column: reasons_scope
text_clean.df <- bcontrol.df %>% 
  unnest_tokens(word, reasons_scope) %>% 
  group_by(id) %>%
  mutate(row_num = 1:n())

# Paste cleaned text back together
text_clean.df <- text_clean.df %>%
  group_by(id) %>%
  summarise(reasons_scope_clean = paste(word, collapse = " ")) %>%
  mutate(reasons_scope_clean = if_else(reasons_scope_clean == "NA", 
                                       NA_character_,
                                       reasons_scope_clean))

# Join cleaned text to the original data
bcontrol.df <- bcontrol.df %>%
  left_join(text_clean.df, by = c("id"))

# Dictionary
bcontrol.df <- bcontrol.df %>%
  mutate(category_dict =
           case_when(
             # COVID-19
             str_detect(reasons_scope_clean, "^(?!.*terror|movement).*(coronavirus|covid 19)(?!.+(terror|movement))") == TRUE ~ "pandemic",
             
             str_detect(reasons_scope_clean, "coronavirus|covid 19") == TRUE & 
               str_detect(reasons_scope_clean, "bomb|terror|attack") == TRUE &
               str_detect(reasons_scope_clean, "influx|flow|migrant|movement") == TRUE ~ "pandemic, terror & migration",
             
             str_detect(reasons_scope_clean, "coronavirus|covid 19") == TRUE &
               str_detect(reasons_scope_clean, "bomb|terror|attack") == TRUE &
               str_detect(reasons_scope_clean, "influx|flow|migrant|movement") == FALSE ~ "pandemic & terror",
             
             str_detect(reasons_scope_clean, "coronavirus|covid 19") == TRUE &
               str_detect(reasons_scope_clean, "influx|flow|migrant|movement") == TRUE &
               str_detect(reasons_scope_clean, "bomb|terror|attack") == FALSE ~ "pandemic & migration",
             
             # Events
             str_detect(reasons_scope_clean, 
                        "^(?!.*terror).*(meeting|conference|summit|forum|parliament|visit|ministerial)") == TRUE ~ "pol_event",
             str_detect(reasons_scope_clean, "demonstration|celebration|eta") == TRUE ~ "demo_event",
             str_detect(reasons_scope_clean, "football|tour") == TRUE ~ "sport_event",
             str_detect(reasons_scope_clean, "ceremony") == TRUE ~ "other_event",
             
             # Security
             str_detect(reasons_scope_clean, "bomb|terror|attack") == TRUE ~ "terrorism",
             str_detect(reasons_scope_clean, "(public policy and internal security|public order and internal security|security situation in europe)(?!.+movement)") == TRUE ~ "public order and security",
             
             # Immigration
             str_detect(reasons_scope_clean, 
                        "^(?!.*security).*(influx|flow|migrant|movement|recommendation of the council of 12 may 2016|recommendation of the council of 11 november 2016|recommendation of the council of 7 february 2017|recommendation of the council of 11 may 2017)") == TRUE ~ "migration",
             str_detect(reasons_scope_clean, 
                        "security situation in europe and threats resulting from the continuous significant secondary movements|security situation in europe and continuous significant secondary movements|migration and security") == TRUE ~ "crimmigration",
             
             TRUE ~ NA_character_
           ))

# Merge manually coded TBC (2000-2006) and dictionary approach (2006-2020)
bcontrol.df <- bcontrol.df %>%
  mutate(category = if_else(is.na(category) & !is.na(category_dict), category_dict, category)) %>%
  select(-category_dict)

# Remove unnecessary objects
rm(list = setdiff(ls(), c("bcontrol.df", "theme.basic")))

# Export
export(bcontrol.df, "./data/TemporaryBorderControls 1996-2020.RDS")
