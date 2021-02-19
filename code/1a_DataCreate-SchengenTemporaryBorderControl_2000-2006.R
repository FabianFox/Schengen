# Data on temporarily reinstated border controls in the Schengen Area

# Data received from the transparency service of the General Secretariat of the
# Council (2020/09/15)

# Coverage: 2000-2006

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
loc <- "./data/Border controls 2000-2006.csv"

# Load the data
bcontrol.df <- import(loc) %>%
  set_names(nm = .[1,]) %>%
  .[,c(-1,-5)] %>%
  clean_names()

# Remove redundant header columns
bcontrol.df <- bcontrol.df %>%
  mutate(col_length = str_length(document)) %>%
  filter(col_length >= 10) %>%
  select(-col_length)
  
# Separate document number from border specification
bcontrol.df <- bcontrol.df %>%
  mutate(document_id = str_extract(document, "[:digit:]+/[:digit:]+(/05 REV 1)*"),
         document = str_remove(document, "[:digit:]+/[:digit:]+(/05 REV 1)*"),
         # Idiosyncratic cases
         document_id2 = str_extract(document, "[:digit:]+/[:digit:]+"),
         document = str_remove(document, "[:digit:]+/[:digit:]+"),
         document_id = ifelse(!is.na(document_id2), 
                              paste0(document_id, "+", document_id2),
                                     document_id)) %>%
  select(-document_id2)

# Extract country names
bcontrol.df <- bcontrol.df %>%
  separate_rows(delegation, sep = "and") %>%
  mutate(iso3_state = flatten_chr(str_extract_all(delegation, "[:upper:]+")))

# Add a unique id
bcontrol.df <- bcontrol.df %>%
  mutate(nb_a = 1:n()) %>%
  select(nb_a, everything())

# Clean date column
bcontrol.df <- bcontrol.df %>%
  mutate(dates_concerned = str_replace_all(dates_concerned, "\\.", "/")) %>%
  separate(dates_concerned, into = c("begin", "end"), sep = "-", remove = FALSE)

# Repair incomplete dates
bcontrol.df[bcontrol.df$nb_a == 1, "begin"] <- "13/11/06"
bcontrol.df[bcontrol.df$nb_a == 2, "end"] <- "21/10/06"
bcontrol.df[bcontrol.df$nb_a == 3, "begin"] <- "09/10/06"
bcontrol.df[bcontrol.df$nb_a == 4, "begin"] <- "25/08/06"
bcontrol.df[bcontrol.df$nb_a == 5, "begin"] <- "09/5/06"
bcontrol.df[bcontrol.df$nb_a == 6, "begin"] <- "09/5/06"
bcontrol.df[bcontrol.df$nb_a == 7, "begin"] <- "01/06/06"
bcontrol.df[bcontrol.df$nb_a == 8, "end"] <- "25/2/06"
bcontrol.df[bcontrol.df$nb_a == 9, "begin"] <- "09/2/06"
bcontrol.df[bcontrol.df$nb_a == 11, c("begin", "end")] <- c("09/12/05", "08/01/06") # Corrected: Original letter states different date
bcontrol.df[bcontrol.df$nb_a == 12, "begin"] <- "26/11/2005"
# Separate the following case into ITA (3 days) and ESP (1 day)
# ITA
bcontrol.df[bcontrol.df$nb_a == 19, c("begin", "end")] <- c("07/02/05", "10/02/05") # border controls for 1 (ESP) and 3 (ITA) days
# FRA
bcontrol.df <- bcontrol.df %>%
  add_row(nb_a = 61, document = "\n- Frontières entre la France et l'Italie\n- Frontières entre la France et l'Espagne",
          delegation = "FR del", dates_concerned = "5/2/05", begin = "5/2/05", end = "5/2/05", document_id = "6185/1/05 REV 1",
          iso3_state = "FR")
bcontrol.df[bcontrol.df$nb_a == 21, "begin"] <- "01/06/04"
bcontrol.df[bcontrol.df$nb_a == 22, "begin"] <- "26/5/04"
bcontrol.df[bcontrol.df$nb_a == 23, "begin"] <- "15/05/04"
bcontrol.df[bcontrol.df$nb_a == 27, "end"] <- "02/03/03"
bcontrol.df[bcontrol.df$nb_a == 29, "begin"] <- "06/12/02"
bcontrol.df[bcontrol.df$nb_a == 30, "begin"] <- "06/12/02" 
bcontrol.df[bcontrol.df$nb_a == 31, "begin"] <- "01/11/02"
bcontrol.df[bcontrol.df$nb_a == 32, "begin"] <- "01/11/02"
bcontrol.df[bcontrol.df$nb_a == 33, "end"] <- "19/10/02"
bcontrol.df[bcontrol.df$nb_a == 34, "begin"] <- "09/09/02"
bcontrol.df[bcontrol.df$nb_a == 35, "begin"] <- "14/06/02"
bcontrol.df[bcontrol.df$nb_a == 36, "begin"] <- "15/06/02"
bcontrol.df[bcontrol.df$nb_a == 37, "begin"] <- "07/06/02"
bcontrol.df[bcontrol.df$nb_a == 38, "begin"] <- "07/05/02"
bcontrol.df[bcontrol.df$nb_a == 39, "begin"] <- "21/03/02" # typo '221/3' corrected by use of original document
bcontrol.df[bcontrol.df$nb_a == 40, "begin"] <- "09/03/02"
bcontrol.df[bcontrol.df$nb_a == 41, "begin"] <- "07/03/02"
bcontrol.df[bcontrol.df$nb_a == 42, "end"] <- "01/02/02"
bcontrol.df[bcontrol.df$nb_a == 43, "begin"] <- "30/01/02"
bcontrol.df[bcontrol.df$nb_a == 45, "begin"] <- "05/12/01"
bcontrol.df[bcontrol.df$nb_a == 47, "begin"] <- "14/07/01"
bcontrol.df[bcontrol.df$nb_a == 48, "begin"] <- "15/06/01"
bcontrol.df[bcontrol.df$nb_a == 49, "begin"] <- "25/06/01"
bcontrol.df[bcontrol.df$nb_a == 52, "begin"] <- "02/12/00"
bcontrol.df[bcontrol.df$nb_a == 53, "begin"] <- "25/11/00"
bcontrol.df[bcontrol.df$nb_a == 54, "begin"] <- "11/10/00"
bcontrol.df[bcontrol.df$nb_a == 55, "begin"] <- "10/10/00"
bcontrol.df[bcontrol.df$nb_a == 56, "begin"] <- "07/07/00"
bcontrol.df[bcontrol.df$nb_a == 57, "begin"] <- "01/06/00"
bcontrol.df[bcontrol.df$nb_a == 58, "begin"] <- "01/06/00"
bcontrol.df[bcontrol.df$nb_a == 59, "begin"] <- "10/01/00"
bcontrol.df[bcontrol.df$nb_a == 60, "begin"] <- "10/01/00"

# One additional case has been provided in a separate document for the time period
# of 1996 to 2000 (received: 2020/10/01)
bcontrol.df <- bcontrol.df %>%
  add_row(nb_a = 62, document = "the visit of the President of the Islamic Republic Iran in France",
          delegation = "FR del", dates_concerned = "", begin = "23/10/99", end = "29/10/99", document_id = "710161",
          iso3_state = "FR")

# Turn begin/end into correct dates
bcontrol.df <- bcontrol.df %>%
  select(-dates_concerned) %>%
  mutate_at(vars("begin", "end"), list(~dmy(.))) %>%
  mutate(iso3_state = countrycode(iso3_state, "iso2c", "iso3c"),
         member_state = countrycode(iso3_state, "iso3c", "country.name.en")) %>%
  select(-delegation)

# (2) Add Schengen membership data
### ------------------------------------------------------------------------###
# Note: created in: 1_DataCreate_SchengenTemporaryBorderControl_2006_2020.R
#       line: 155-174
schengen.df <- import("./data/SchengenMembership.rds")

# (3) Join data frames
### ------------------------------------------------------------------------###
bcontrol.df <- bcontrol.df %>%
  left_join(y = schengen.df)

# (4) Make the dataset dyadic
## -------------------------------------------------------------------------- ##
# Load data:
# eu_neigbours 
# Note: created in: 1_DataCreate_SchengenTemporaryBorderControl_2006_2020.R
#       line: 194-234
eu_neighbours <- import("./data/EU_neighbours.rds")

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
  arrange(desc(nb_a)) %>%
  mutate(reasons_scope = str_squish(document)) %>%
  select(nb_a, member_state, iso3_state, document_id, begin,	end, implemented, signed, 
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
# export(bcontrol_dyad.df, "./data/TemporaryBorderClosures 1996-2006.xlsx")

# Import edited data
### ------------------------------------------------------------------------###
bcontrol_1b.df <- import("./data/TemporaryBorderClosures 1996-2006.xlsx")
