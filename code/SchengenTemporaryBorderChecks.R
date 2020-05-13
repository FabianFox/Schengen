# Data on temporarily reinstated border controls in the Schengen Area

# Downloaded on 2020/05/13
# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Issues
### ------------------------------------------------------------------------###

# - Closures that only last one day are counted as zero days (fix: +1 days)
# - Create two datasets: (1) Incidents that span across year counted twice
#                        (2) One entry for each closure irrespective of 
#                            duration
# - in 2017, Norway has closed different borders for more than 365 days (land / air border)

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "qdap", "rio", "countrycode", "janitor", "lubridate", "rvest",
       "patchwork", "tidytext", "eurostat")

# Settings
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
#    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# (1) Temporary border checks
### ------------------------------------------------------------------------###
# Location of file
loc <- "./data/ms_notifications_-_reintroduction_of_border_control_en.xlsx"

# Load the data
bcontrol.df <- import(loc)[3:193,] %>%
  setNames(.[1,]) %>%
  .[-1,] %>%
  clean_names() %>%
  separate_rows(duration, sep = ";") %>%
  mutate(nb = n():1,
         duration = str_squish(duration),
         duration_clean = str_extract_all(
           duration, "[:digit:]+/[:digit:]{2}/[:digit:]{4}"),
         begin = map(duration_clean, 1),
         end = map(duration_clean, 2),
         reasons_scope = str_squish(reasons_scope)) %>%
  select(nb, duration, begin, end, everything())

# Add Begin/End of border control to data frame
bcontrol.df <- bcontrol.df %>%
  mutate(begin = map(duration_clean, 1),
         end = map(duration_clean, 2)) %>%
  select(nb, duration, begin, end, everything())

# Repair incomplete dates
# Additional note: The NB column in the original table counts two incidents of 
#                  border closure twice thus reducing the actual number of 
#                  incidents to 136. Actually, there are 138 temporary border 
#                  closures.

bcontrol.df[bcontrol.df$nb == 179, "end"] <- "14/05/2020"

# nb 174 and 171 entails multiple entries which need to be split
bcontrol.df <- bcontrol.df %>%
  filter(!nb %in% c(174, 171) ) %>%
  add_row(
    nb = c(174, 174, 171, 171, 171),
    duration = c("15/04/2020- 05/05/2020", "12/05/2020- 11/11/2020", 
                 "15/04/2020- 05/05/2020", "15/05/2020- 13/08/2020",
                 "12/05/2020- 12/11/2020"),
    begin = c(list("15/04/2020"), list("12/05/2020"), list("15/04/2020"), 
              list("15/05/2020"), list("12/05/2020")),
    end = c(list("05/05/2020"), list("11/11/2020"), list("05/05/2020"), 
            list("13/08/2020"), list("12/11/2020")),
    member_state = c("Germany", "Germany", "Norway", "Norway", "Norway"),
    reasons_scope = c(
    "coronavirus COVID-19; internal land and air borders with Austria, 
    Switzerland, France, Luxembourg, Denmark, Italy and Spain, sea border
    with Denmark",
    "secondary movements, situation at the external borders; land border 
    with Austria",
    "coronavirus COVID-19; all internal borders",
    "coronavirus COVID-19; all internal borders",
    "terrorist threats, secondary movements; ports with ferry connections 
    with Denmark, Germany and Sweden;"),
    duration_clean = c(
      list(c("15/04/2020", "05/05/2020", "12/05/2020", "11/11/2020")),
      list(c("15/04/2020", "05/05/2020", "12/05/2020", "11/11/2020")),
      list(c("15/04/2020", "05/05/2020", "15/05/2020", "13/08/2020",
        "12/05/2020", "12/11/2020")),
      list(c("15/04/2020", "05/05/2020", "15/05/2020", "13/08/2020",
        "12/05/2020", "12/11/2020")),
      list(c("15/04/2020", "05/05/2020", "15/05/2020", "13/08/2020",
        "12/05/2020", "12/11/2020"))
      ))

bcontrol.df[bcontrol.df$nb == 172, "end"] <- "05/05/2020"
bcontrol.df[bcontrol.df$nb == 168, "end"] <- "17/04/2020"

bcontrol.df[bcontrol.df$nb == 142, c("begin", "end")] <- "07/04/2020"
bcontrol.df[bcontrol.df$nb == 128, "end"] <- "24/04/2020"
bcontrol.df[bcontrol.df$nb == 110, c("begin", "end")] <- c("10/02/2019", "16/02/2019")
bcontrol.df[bcontrol.df$nb == 108, "end"] <- "16/12/2018"
bcontrol.df[bcontrol.df$nb == 93, c("begin", "end")] <- c("12/11/2017", "19/11/2017")
bcontrol.df[bcontrol.df$nb == 92, c("begin", "end")] <- c("12/11/2017", "12/05/2018")
bcontrol.df[bcontrol.df$nb == 90, c("begin", "end")] <- c("12/11/2017", "12/05/2018")
bcontrol.df[bcontrol.df$nb == 89, c("begin", "end")] <- c("12/11/2017", "12/05/2018")
bcontrol.df[bcontrol.df$nb == 88, c("begin", "end")] <- c("12/11/2017", "12/05/2018")
bcontrol.df[bcontrol.df$nb == 85, c("begin", "end")] <- c("10/05/2017", "30/05/2017")
bcontrol.df[bcontrol.df$nb == 66, "end"] <- "02/08/2016"
bcontrol.df[bcontrol.df$nb == 65, "end"] <- "12/11/2016"
bcontrol.df[bcontrol.df$nb == 63, c("begin", "end")] <- c("01/06/2016", "12/11/2016")
bcontrol.df[bcontrol.df$nb == 50, c("begin", "end")] <- c("13/11/2015", "13/12/2015")
bcontrol.df[bcontrol.df$nb == 49, c("begin", "end")] <- c("12/11/2015", "09/01/2016")
bcontrol.df[bcontrol.df$nb == 48, c("begin", "end")] <- c("09/11/2015", "31/12/2015")
bcontrol.df[bcontrol.df$nb == 47, c("begin", "end")] <- c("17/10/2015", "26/10/2015")
bcontrol.df[bcontrol.df$nb == 46, c("begin", "end")] <- c("17/09/2015", "26/09/2015")
bcontrol.df[bcontrol.df$nb == 45, c("begin", "end")] <- c("27/09/2015", "16/10/2015")
bcontrol.df[bcontrol.df$nb == 44, c("begin", "end")] <- c("16/09/2015", "25/09/2015")
bcontrol.df[bcontrol.df$nb == 43, c("begin", "end")] <- c("26/09/2015", "15/10/2015")
bcontrol.df[bcontrol.df$nb == 42, c("begin", "end")] <- c("16/10/2015", "04/11/2015")
bcontrol.df[bcontrol.df$nb == 40, c("begin", "end")] <- c("13/09/2015", "22/09/2015")
bcontrol.df[bcontrol.df$nb == 39, c("begin", "end")] <- c("23/09/2015", "12/10/2015")
bcontrol.df[bcontrol.df$nb == 38, c("begin", "end")] <- c("13/10/2015", "01/11/2015")
bcontrol.df[bcontrol.df$nb == 37, c("begin", "end")] <- c("02/11/2015", "13/11/2015")
bcontrol.df[bcontrol.df$nb == 36, c("begin", "end")] <- c("16/05/2015", "15/6/2015")
bcontrol.df[bcontrol.df$nb == 35, c("begin", "end")] <- c("31/08/2014", "03/09/2014")
bcontrol.df[bcontrol.df$nb == 34, c("begin", "end")] <- c("24/07/2014", "31/07/2014")
bcontrol.df[bcontrol.df$nb == 33, c("begin", "end")] <- c("01/06/2014", "06/06/2014") # G7 Summit
bcontrol.df[bcontrol.df$nb == 32, c("begin", "end")] <- c("14/03/2014", "28/03/2014")
bcontrol.df[bcontrol.df$nb == 31, c("begin", "end")] <- c("08/11/2013", "23/11/2013")
bcontrol.df[bcontrol.df$nb == 30, c("begin", "end")] <- c("03/12/2012", "12/12/2012")
bcontrol.df[bcontrol.df$nb == 29, c("begin", "end")] <- c("04/06/2012", "01/07/2012")
bcontrol.df[bcontrol.df$nb == 28, c("begin", "end")] <- c("02/05/2012", "04/05/2012")
bcontrol.df[bcontrol.df$nb == 24, c("begin", "end")] <- c("04/06/2011", "09/06/2011")
bcontrol.df[bcontrol.df$nb == 23, c("begin", "end")] <- c("16/11/2010", "20/11/2010")
bcontrol.df[bcontrol.df$nb == 22, c("begin", "end")] <- c("24/05/2010", "01/06/2010")
bcontrol.df[bcontrol.df$nb == 21, c("begin", "end")] <- c("28/05/2010", "02/06/2010")
bcontrol.df[bcontrol.df$nb == 20, c("begin", "end")] <- c("17/04/2010", "23/04/2010")
bcontrol.df[bcontrol.df$nb == 19, c("begin", "end")] <- c("05/04/2010", "18/04/2010")
bcontrol.df[bcontrol.df$nb == 18, c("begin", "end")] <- c("01/12/2009", "18/12/2009")
bcontrol.df[bcontrol.df$nb == 16, c("begin", "end")] <- c("27/09/2009", "27/09/2009") # 50th anniversary of ETA
bcontrol.df[bcontrol.df$nb == 15, c("begin", "end")] <- c("26/09/2009", "27/09/2009")
bcontrol.df[bcontrol.df$nb == 14, c("begin", "end")] <- c("19/09/2009", "19/09/2009")
bcontrol.df[bcontrol.df$nb == 10, c("begin", "end")] <- c("05/03/2009", "07/03/2009")
bcontrol.df[bcontrol.df$nb == 8, c("begin", "end")] <- c("27/09/2008", "27/09/2008")
bcontrol.df[bcontrol.df$nb == 6, c("begin", "end")] <- c("02/11/2007", "03/11/2007")
bcontrol.df[bcontrol.df$nb == 5, c("begin", "end")] <- c("25/05/2007", "09/06/2007")
bcontrol.df[bcontrol.df$nb == 4, c("begin", "end")] <- c("12/02/2007", "16/02/2007")
bcontrol.df[bcontrol.df$nb == 3, c("begin", "end")] <- c("13/11/2006", "29/11/2006")
bcontrol.df[bcontrol.df$nb == 2, c("begin", "end")] <- c("09/10/2006", "21/10/2006")
bcontrol.df[bcontrol.df$nb == 1, c("begin", "end")] <- c("21/10/2006", "21/10/2006")

# Turn begin/end into correct dates & fill missing member state: Switzerland
bcontrol.df <- bcontrol.df %>%
  select(-duration_clean, -duration) %>%
  mutate_at(vars("begin", "end"), list(~dmy(.))) %>%
  fill(member_state, .direction = "down") %>%
  mutate(iso3_state = countrycode(member_state, "country.name.en", "iso3c"))

# (2) Add Wikipedia table providing information of Schengen membership
### ------------------------------------------------------------------------###
schengen.df <- read_html("https://en.wikipedia.org/wiki/Schengen_Area") %>%
  html_node(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[3]") %>%
  html_table(header = T, fill = T) %>%
  select(1,4,5) %>%
  set_names("member_state", "signed", "implemented") %>%
  mutate_all(list(~bracketX(.))) %>%
  mutate_at(vars("signed", "implemented"), list(~dmy(.))) %>%
  .[-27,] %>% # - Schengen Area
  mutate(iso3_state = countrycode(member_state, "country.name.en", "iso3c")) %>%
  select(-member_state)

# Iceland/Norway have an additional agreement. Only the multilateral agreement
# is considered here. 
schengen.df[11, "signed"] <- dmy("19 December 1996")
schengen.df[19, "signed"] <- dmy("19 December 1996")

# (3) Join data frames
### ------------------------------------------------------------------------###
bcontrol.df <- bcontrol.df %>%
  left_join(y = schengen.df)

# Add missing Schengen member states and different naming conventions
bcontrol.df <- bcontrol.df %>%
  bind_rows(
    schengen.df[which(
      !unique(schengen.df$iso3_state) %in%
        bcontrol.df$iso3_state),]) %>%
  mutate(member_state = if_else(is.na(member_state), 
                                countrycode(iso3_state, 
                                            "iso3c", "country.name.en"), 
                                member_state),
         eu_name = countrycode(iso3_state, "iso3c", "eurostat")) %>%
  select(nb, member_state, iso3_state, eu_name, everything())

# Make the dataset dyadic
## -------------------------------------------------------------------------- ##
# Load data:
# - Direct Contiguity
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
# Latest observation: 2016
contdird <- import(file = "./data/contdird.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

# Select only the latest observation (2016), land borders (conttype)
# and remove unnecessary variables
contdird <- contdird %>%
  select(state1no, state2no, year, conttype) %>%
  filter(year == 2016 & conttype %in% c(1:3))

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX")

# (2) Transform
contdird <- contdird %>%
  mutate(state1 = countrycode(sourcevar = state1no, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2no, origin = "cown", destination = "iso3c", custom_match = custom.match),
         eu_name1 = countrycode(sourcevar = state1, origin = "iso3c", destination = "eurostat"),
         eu_name2 = countrycode(sourcevar = state2, origin = "iso3c", destination = "eurostat")) %>%
  select(state1, state2, eu_name1, eu_name2, conttype, year) %>%
  filter(eu_name1 %in% c(eu_countries$code, efta_countries$code) &
           eu_name2 %in% c(eu_countries$code, efta_countries$code))

# EU states and neighbours
eu_neighbours <- contdird %>%
  group_by(state1) %>%
  summarise(neighbours_iso3 = paste0(state2, collapse = ",")) %>%
  select(state1, neighbours_iso3)

# Join to original data frame
bcontrol_dyad.df <- bcontrol.df %>%
  left_join(y = eu_neighbours, by = c("iso3_state" = "state1"))

# Make state and neighbours data longer 
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  separate(neighbours_iso3, into = paste0("n", 1:9), extra = "drop") %>%
  pivot_longer(cols = n1:n9, values_to = "neighbour") %>%
  filter(!is.na(neighbour)) %>%
  select(-name) %>%
  mutate(neighbour_eu = countrycode(neighbour, "iso3c", "eurostat"),
         neighbour_name = countrycode(neighbour, "iso3c", "country.name.en"))

# Identify temporary closure against country and add type
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  mutate(neighbour_mention = str_detect(
    reasons_scope, paste(neighbour_name, neighbour_eu, 
                         "[A|a]ll", "Land, air and sea", 
                         "except.+[Liechtenstein|France]", 
                         sep = "|"))) %>%
  filter(neighbour_mention == TRUE)

# Add: "except.+[Liechtenstein|France]")

# Check remaining incidents that cannot be aligend with any specific borders
# Note:
# - some cases mention only regions or certain BCPs (not entire countries)
# - some cases do not specify a border
# - sometimes it seems that all borders are affected but without a keyword (i.e. 'all')

bcontrol_dyad.df %>%
  mutate(neighbour_mention = str_detect(
    reasons_scope, paste(neighbour_name, neighbour_eu, 
                         "[A|a]ll", "Land, air and sea", sep = "|"))) %>%
  filter(neighbour_mention == FALSE) %>%
  distinct(nb, .keep_all = T) %>%
  select(nb, member_state, reasons_scope, neighbour) %>%
  View()
                                        
# Dictionary method to classify stated reasons for closure
### ------------------------------------------------------------------------###
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
dict.df <- tibble(
  pandemic = list(c("coronavirus", "covid 19")),
  event = list(c("conference", "terrorist threat[s]")),
  migration = list(c("secondary movement[s]"))
)

# Remove unnecessary objects
rm(list = setdiff(ls(), c("bcontrol.df", "theme.basic")))

# Additional variables & data wrangling
### ------------------------------------------------------------------------###
# Turn closures that span multiple years into separate columns
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
                       ceiling_date(ymd(begin), "year") - days(1), end))

# (D) Bind together and arrange
bcontrol.df <- bcontrol.df %>%
  bind_rows(bcontrol_myear.df) %>%
  arrange(nb)

# Compute length of closure in days
# Note: +1 day to account for intervals that last only one day
bcontrol.df <- bcontrol.df %>%
  mutate(closure_length = interval(ymd(begin), ymd(end)) / days(1) + 1) %>%
  select(nb, begin, end, closure_length, everything())

# Visualization
### ------------------------------------------------------------------------###
# Number of temporary border closures by year 
num_closure.fig <- bcontrol.df %>%
  filter(!is.na(begin)) %>%
  group_by(year = floor_date(ymd(begin), "year")) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = year, y = n), stat = "identity") +
  labs(x = "", y = "",
       title = "Number of temporary border closures by year",
       caption = "Note: Temporary border closures that persist across years are counted once for each year.") +
  theme.basic 

# Number of temporary border closures: mean length
length_closure.fig <- bcontrol.df %>%
  filter(!is.na(begin)) %>%
  group_by(year = floor_date(ymd(begin), "year")) %>%
  summarise(sum_closure = sum(closure_length, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = year, y = sum_closure), stat = "identity") +
  labs(x = "", y = "",
       title = "Total duration of temporary border closures by year",
       caption = "Note: Temporary border closures that persist across years are counted for the specific year.") +
  theme.basic

# Combine
num_closure.fig / length_closure.fig

# Closure by member state
length_closure_country.fig <- bcontrol.df %>%
  filter(!is.na(begin)) %>%
  group_by(iso3_state, year = floor_date(ymd(begin), "year")) %>%
  summarise(sum_closure = sum(closure_length, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = sum_closure), stat = "identity") +
  facet_wrap(~iso3_state) +
  labs(x = "", y = "",
       title = "Total duration of temporary border closures by year",
       caption = "Note: Temporary border closures that persist across years are counted for the specific year.") +
  theme.basic + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By now excluding some events concerning security issues
bcontrol.df <- bcontrol.df %>%
  mutate(
    reasons_scope = tolower(reasons_scope), 
    migration = case_when(
      str_detect(reasons_scope, "migration|migrant|migratory|influx|recommendation|movements") == TRUE ~ "Migration",
      str_detect(reasons_scope, "migration|migrant|migratory|influx|recommendation|movements") == FALSE ~ "Other")) %>%
  as_tibble() 

# Join to EU28-base
year <- 2015:2018
migration <- c("Migration", "Other")

bcontrol.expand.df <- tibble(
  country = countrycode::codelist$iso3c
) %>%
  mutate(eu28 = countrycode(country, "iso3c", "eu28")) %>%
  filter(eu28 == "EU") %>%
  expand(country, year, migration) 

# By individual Member States
bcontrol.expand.join <- bcontrol.df %>%
  filter(Begin >= 2015) %>%
  group_by(`Member State`, Begin, migration, country) %>%
  distinct(`Member State`, Begin, migration, .keep_all = TRUE) %>%
  summarize(checks = n()) %>%
  arrange(desc(checks))

# Join data to the expanded df
bcontrol.expand.df <- bcontrol.expand.df %>%
  left_join(bcontrol.expand.join, by = c("country", "migration", "year" = "Begin")) %>%
  mutate(member_state = countrycode(country, "iso3c", "country.name.en"),
         checks = ifelse(is.na(checks), 0, checks)) %>%
  select(country, year, member_state, migration, checks) 

# Housekeeping
rm(year_duration, bcontrol.expand.join, loc, migration, year)

# Visualization
### ------------------------------------------------------------------------###

# Note: States can reinstate border checks several times per year. Here, each type is counted only once
#       per year. 

# Over time (only countries in grouping variable, see AsylumData.R))
bcontrol.plot <- bcontrol.df %>%
  group_by(Begin, migration) %>%
  distinct(`Member State`, Begin, migration, .keep_all = TRUE) %>%
  mutate(group = case_when(
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    country %in% c("AUT", "BEL", "DNK",  "DEU", "FIN", "LUX", "SWE") ~ "Host states",
    country %in% c("CYP", "GRC", "ITA", "MLT") ~ "Frontline states"
  )) %>%
  filter(!is.na(group)) %>%
  summarize(checks = n()) %>%
  ggplot() +
  geom_bar(aes(x = Begin, y = checks, fill = migration), stat = "identity") +
  labs(title = "Total number of temporary border controls, 2006 - 2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(2006, 2018, 4)) +
  scale_fill_manual("Reason: ", values = c("Migration" = "#CCCCCC", "Other" = "#4D4D4D")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Countries that reinstated border controls at least once from 2015-2018
bcontrol.year.plot <- bcontrol.expand.df %>%
  mutate(group = case_when(
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    country %in% c("AUT", "BEL", "DNK",  "DEU", "FIN", "LUX", "SWE") ~ "Host states",
    country %in% c("CYP", "GRC", "ITA", "MLT") ~ "Frontline states"
  )) %>%
  filter(migration == "Migration",
         !is.na(group)) %>%
  group_by(country) %>%
  mutate(sum_checks = sum(checks)) %>%
  ggplot() +
  geom_waffle(aes(x = fct_relevel(country,
                                  "SVK", "POL", "CZE", "HUN",
                                  "ITA", "GRC", "CYP", "MLT",
                                  "LUX", "FIN", "BEL", "SWE", "DNK", "DEU", "AUT"),
                  y = year, fill = factor(checks))) +
  scale_fill_manual(values=c("#FFFFFF", "#000000")) +
  labs(title = "Number of temporary border controls, 2015-2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))
  
# By grouping variable (see AsylumData.R)
bcontrol.group.df <- bcontrol.expand.df %>%
  group_by(country, year, migration) %>%
  distinct(country, year, migration, .keep_all = TRUE) %>%
  mutate(group = case_when(
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    country %in% c("AUT", "BEL", "DNK",  "DEU", "FIN", "LUX", "SWE") ~ "Host states",
    country %in% c("CYP", "GRC", "ITA", "MLT") ~ "Frontline states"
  )) %>%
  filter(!is.na(group)) %>%
  group_by(group, year, migration) %>%
  mutate(n_country = n(),
         checks = sum(checks)) %>%
  select(group, year, checks, migration, n_country) %>%
  distinct()

bcontrol.group.plot <- ggplot(bcontrol.group.df) +
  geom_bar(aes(x = year, y = checks, fill = migration), stat = "identity") +
  facet_wrap(~group) +
  labs(title = "Number of temporary border controls by group, 2006 - 2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(2015, 2018, 1)) +
  scale_fill_manual("Reason", values = c("Migration" = "#CCCCCC", "Other" = "#4D4D4D")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Combined plots
legend <- get_legend(bcontrol.plot + theme(legend.position = "bottom"))

combined.control.plot <- plot_grid(
  bcontrol.plot + theme(legend.position = "none"),
  bcontrol.group.plot + theme(legend.position = "none"),
  nrow = 3,
  rel_heights = c(0.4, 0.4, 0.1),
  legend
  )

# Saving data and figures
### ------------------------------------------------------------------------###
# Data:
saveRDS(bcontrol.member.df, file = "./data/TempControlSchengen.rds")

# Plots:
ggsave(filename = "./FRAN-reports/TempControlsSchengenFig.tiff", 
       plot = combined.control.plot, device = "tiff", dpi = 600)

ggsave(filename = "./FRAN-reports/TempControlsSchengenFig_ByCountry.tiff", 
       plot = bcontrol.year.plot, device = "tiff", dpi = 600)
