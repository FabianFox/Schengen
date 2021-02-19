# Population of Schengen Area

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "janitor", "lubridate", "rvest",
            "patchwork","eurostat")

# Import data on Schengen Membership
# Line 184: 1_DataCreate-SchengenTemporaryBorderControl
schengen.df <- import("./data/SchengenMembership.rds") %>%
  mutate(name = countrycode(iso3_state, "iso3c", "country.name.en", 
                            custom_match = c("CZE" = "Czech Republic")),
         region = countrycode(iso3_state, "iso3c", "region"),
         name_eu = countrycode(iso3_state, "iso3c", "eurostat"))

# Population 1. January 2020 from Eurostat
pop.df <- get_eurostat("demo_gind", time_format = "num", 
                    stringsAsFactors = FALSE,
                    filters = list(time = 2020)) %>%
  select(country = geo, population = values, year = time, measure = indic_de) %>%
  filter(country %in% schengen.df$name_eu, measure == "JAN") %>%
  mutate(country = countrycode(country, "eurostat", "iso3c")) 

# Merge
schengen_pop.df <- schengen.df %>%
  left_join(y= pop.df, by = c("iso3_state" = "country")) %>%
  select(-year, -measure)

# Total Population as of 2020/01/01
schengen_pop.df %>%
  summarise(total_pop = sum(population))
