# Data on temporarily reinstated border controls in the Schengen Area

# Downloaded on 2021/02/16
# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Notes
### ------------------------------------------------------------------------###

# - Closures that only last one day are counted as zero days (fix: +1 days)
# - Create two datasets: (1) Incidents that span across year counted twice
#                        (2) One entry for each closure irrespective of 
#                            duration

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "qdap", "rio", "countrycode", "janitor", "lubridate", "rvest",
       "patchwork", "tidytext", "eurostat")

# (1) Raw data on temporary border controls
### ------------------------------------------------------------------------###
# The pdf file is exported to Excel with Adobe Acrobat and then re-imported to R.

# Location of file
loc <- "./data/Border controls 2006-2021.xlsx"

# Load the data and create a column for duration of border closure
bcontrol.df <- import(loc)[4:257,] %>%
  setNames(.[1,]) %>%
  .[-1,] %>%
  clean_names() %>%
  separate_rows(duration, sep = ";") %>%
  select(nb_old = nb, everything()) %>%
  mutate(nb = n():1,
         duration = str_squish(duration),
         duration_clean = str_extract_all(
           duration, "[:digit:]+/[:digit:]{2}/[:digit:]{4}"),
         begin = map(duration_clean, 1),
         end = map(duration_clean, 2),
         reasons_scope = str_squish(reasons_scope)) %>%
  select(nb, duration, begin, end, nb_old, everything())

# Add Begin/End of border control to data frame
bcontrol.df <- bcontrol.df %>%
  mutate(begin = map(duration_clean, 1),
         end = map(duration_clean, 2)) %>%
  select(nb, duration, begin, end, nb_old, everything())

# Transform into basic dataframe to avoid list-replacement
bcontrol.df <- as.data.frame(bcontrol.df)

# Repair incomplete dates
# Additional note: The NB column in the original table counts two incidents of 
#                  border closure twice. Accordingly, the count of incidents differs
#                  between the original table and the accurate number. 

bcontrol.df[bcontrol.df$nb == 234, "end"] <- "30/04/2021" # end < begin; year: 2020 -> 2021
bcontrol.df[bcontrol.df$nb == 230, c("begin", "end")] <- c("14/09/2020", "13/10/2020")
bcontrol.df[bcontrol.df$nb == 229, c("begin", "end")] <- c("30/09/2020", "30/10/2020")
bcontrol.df[bcontrol.df$nb == 227, "end"] <- "30/09/2020"
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

bcontrol.df[bcontrol.df$nb == 142, "end"] <- "07/04/2020"
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
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div/table[3]") %>%
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

# Save Schengen membership data
# export(schengen.df, "./data/SchengenMembership.rds")

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
  select(nb, member_state, iso3_state, eu_name, nb_old, everything())

# Make the dataset dyadic
## -------------------------------------------------------------------------- ##
# Load data:
# - Direct Contiguity
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
# Latest observation: 2016
contdird <- import(file = "./data/contdird.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

# Select only the latest observation (2016), land borders 
# (conttype: 1 (land, river) & 2 (12-mile sea)) and remove unnecessary variables
contdird <- contdird %>%
  select(state1no, state2no, year, conttype) %>%
  filter(year == 2016 & conttype %in% c(1:2))

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX")

# (2) Transform
contdird <- contdird %>%
  mutate(state1 = countrycode(sourcevar = state1no, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2no, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         eu_name1 = countrycode(sourcevar = state1, origin = "iso3c", 
                                destination = "eurostat"),
         eu_name2 = countrycode(sourcevar = state2, origin = "iso3c", 
                                destination = "eurostat")) %>%
  select(state1, state2, eu_name1, eu_name2, conttype, year) %>%
  filter(eu_name1 %in% c(eu_countries$code, efta_countries$code) &
           eu_name2 %in% c(eu_countries$code, efta_countries$code))

# EU states and neighbors
eu_neighbours <- contdird %>%
  group_by(state1) %>%
  summarise(neighbours_iso3 = paste0(state2, collapse = ",")) %>%
  select(state1, neighbours_iso3)

# Save data on EU neighbours
# export(eu_neighbours, "./data/EU_neighbours.rds")

# Join to original data frame
bcontrol_dyad.df <- bcontrol.df %>%
  left_join(y = eu_neighbours, by = c("iso3_state" = "state1"))

# Make state and neighbor data longer 
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  separate(neighbours_iso3, into = paste0("n", 1:9), extra = "drop") %>%
  pivot_longer(cols = n1:n9, values_to = "neighbour") %>%
  filter(!is.na(neighbour)) %>%
  select(-name) %>%
  mutate(neighbour_eu = countrycode(neighbour, "iso3c", "eurostat"),
         neighbour_name = countrycode(neighbour, "iso3c", "country.name.en"))

# Arrange by nb
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  arrange(desc(nb)) %>%
  mutate(reasons_scope = str_squish(reasons_scope)) %>%
  select(nb, member_state, iso3_state, begin,	end, implemented, signed, 
         reasons_scope, neighbour_name, neighbour_iso3 = neighbour)

# Add Schengen membership for neighboring countries
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  left_join(y = schengen.df, by = c("neighbour_iso3" = "iso3_state")) %>%
  rename("signed_neighbour" = "signed.y",
         "implemented_neighbour" = "implemented.y",
         "signed" = "signed.x",
         "implemented" = "implemented.x")

# Export for manual editing
### ------------------------------------------------------------------------###
export(bcontrol_dyad.df, "./data/TemporaryBorderClosures_2021-02-16.xlsx")
# renamed to "TemporaryBorderClosures 2006-2021.xlsx"

# Import edited data
### ------------------------------------------------------------------------###
# Edited to include only land border closures (edited on 2021/02/16)
bcontrol.df <- import("./data/TemporaryBorderClosures 2006-2021.xlsx")
