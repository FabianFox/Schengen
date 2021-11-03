# Data analysis

# Downloaded on 2020/09/30
# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Notes
### ------------------------------------------------------------------------ ###

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "janitor", "lubridate", "rvest",
            "patchwork", "tidytext", "eurostat", "sf", "ggraph", "tidygraph", "igraph",
            "ggwaffle", "gt")

# Additional settings (i.e. for figures)
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text = element_text(colour = "black", size = 12)
    #    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Extrafonts
extrafont::loadfonts(device = "win")

# Load data
### ------------------------------------------------------------------------ ###
# created in:
# - 1_DataCreate-SchengenTemporaryBorderControl_2006-2020.R
# - 1a_DataCreate-SchengenTemporaryBorderControl_2000-2006.R
# - 2_DataWrangling-SchengenTemporaryBorderControl.R
bcontrol.df <- import("./data/TemporaryBorderControls 1996-2020.RDS")

# Exclude TBCs that are implemented in 2021
bcontrol.df <- bcontrol.df %>%
  filter(year(begin) < 2021)

# Export dataset for third users
bcontrol.df %>%
  select(iso3_state, state = member_state, neighbour_iso3, neighbour_name, begin, 
         end, check_length, reasons_scope, category, duration) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>% 
  export(., "./data/TemporaryBorderControls_1999-2020.csv")

# Analysis
### ------------------------------------------------------------------------ ###

# Number of unique notifications
# 1999-2006: 52
# 2006-2020: 214
# total: 266
bcontrol.df %>%
  mutate(dataset = ifelse(nb < 0, "1999-2006", "2006-2020")) %>% 
  group_by(dataset) %>%
  distinct(nb, .keep_all = T) %>%
  summarise(notification = length(nb)) %>%
  mutate(total = sum(notification))

# Number of data points: 
# total: 831
sum(bcontrol.df$nb < 0) # 1999-2006: 120
sum(bcontrol.df$nb > 0) # 2006-2020: 711

# Total: 831
dim(bcontrol.df)

# Number of checks and mean length
### ------------------------------------------------------------------------ ###
# Number of total temporary border checks by year 
num_checks.df <- bcontrol.df %>%
  group_by(year = floor_date(ymd(begin), "year")) %>%
  # number of checks by year
  summarise(nchecks = n()) %>%
  # share of checks (in year) by total checks (i.e. sums to 100%)
  mutate(share_nchecks = nchecks / sum(nchecks) * 100)

# Number of checks by distinct countries per year
num_country_checks.df <- bcontrol.df %>%
  group_by(year = floor_date(ymd(begin), "year"), iso3_state) %>%
  summarise(ncountry = n()) %>%
  group_by(year) %>%
  summarise(ncountry = n())

# Length of closures (mean)
length_checks.df <- bcontrol.df %>%
  group_by(year = floor_date(ymd(begin), "year")) %>%
  summarise(check_length = mean(check_length, na.rm = FALSE))

# Join data frames (from stackoverflow: https://bit.ly/30GS1yw)
num_checks.df <- list(num_checks.df, 
                      num_country_checks.df, 
                      length_checks.df) %>% 
  reduce(inner_join, by = "year")

# Figure: Number of checks and length of checks
checks_panel_a.fig <- num_checks.df %>%
  ggplot() +
  geom_bar(aes(x = year, y = nchecks), stat = "identity", fill = "#000000") +
  annotate("text", x = as.Date("2008-01-01") - months(6), y = 220,
           hjust = 0, size = 5,
           label = "Initial border controls in\nrelation to the migration\nand refugee crisis") +
  annotate("segment", x = as.Date("2013-01-01")- months(3), 
           xend =  as.Date("2015-01-01") - months(1), 
           y = 205, yend = 120) +
  annotate("text", x = as.Date("2014-01-01") - months(6), y = 320,
           hjust = 0, size = 5,
           label = "Initial border controls\nin relation to COVID-19") +
  annotate("segment", x = as.Date("2019-01-01") - months(5), 
           xend =  as.Date("2020-01-01"), 
           y = 320, yend = 300) +
  annotate("text", x = as.Date("2015-01-01") - months(12), y = 215,
           hjust = 0, size = 5,
           label = "Terror attacks in France,\nBelgium and Germany") +
  annotate("segment", x = as.Date("2017-01-01") - months(4), 
           xend =  as.Date("2015-01-01") + months(1), 
           y = 180, yend = 120) +
  annotate("segment", x = as.Date("2017-01-01") - months(4), 
           xend =  as.Date("2016-01-01"), 
           y = 180, yend = 80) +
  annotate("segment", x = as.Date("2017-01-01") - months(4), 
           xend =  as.Date("2017-01-01"), 
           y = 180, yend = 60) + 
  scale_y_continuous(limits = c(0, 350)) +
  scale_x_date(
    breaks = seq(from = date("2000-01-01"), to = date("2020-12-31"), by = "5 year"),
    labels = paste(seq(2000, 2020, 5)),
    limits = c(date("1998-01-01"), date("2020-12-31"))) +
  labs(x = "", y = "Number",
       title = "") +
  theme.basic +
  theme(axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

checks_panel_b.fig <- num_checks.df %>%
  ggplot() +
  geom_line(aes(x = year, y = check_length), stat = "identity", size = 1.5) +
  geom_point(aes(x = year, y = check_length), stat = "identity", size = 3) +
  scale_x_date(
    breaks = seq(from = date("2000-01-01"), to = date("2020-12-31"), by = "5 year"),
    labels = paste(seq(2000, 2020, 5)),
    limits = c(date("1998-01-01"), date("2020-12-31"))) +
  labs(x = "", y = "Days",
       title = "",
       caption = "Note: Temporary border controls that persist across years are counted as separate cases.") +
  theme.basic +
  theme(axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Combine using patchwork
checks_year_length.fig <- checks_panel_a.fig / checks_panel_b.fig

# Description
bcontrol.df %>%
  mutate(segment = 
           car::recode(year(begin), 
           recodes = 
           "2001:2005='2001-2005'; 2006:2010='2006-2010'; 2011:2015='2011-2015';
           2016:2020='2016-2020'; else = 'unassigned'")) %>%
  group_by(segment) %>%
  summarise(mean_length_days = round(mean(check_length), 0),
            mean_length_months = mean(mean_length_days / 30))

# Network analysis
### ------------------------------------------------------------------------ ###

# Import data on Schengen Membership
# Line 184: 1_DataCreate-SchengenTemporaryBorderControl
schengen.df <- import("./data/SchengenMembership.rds") %>%
  mutate(name = countrycode(iso3_state, "iso3c", "country.name.en", 
                            custom_match = c("CZE" = "Czech Republic")),
         region = countrycode(iso3_state, "iso3c", "region"))

# Create a complete dataset of neighbouring states in the EU

# All borders in the Schengen area
# Import data on EU neighbours
# Line 244: 1_DataCreate-SchengenTemporaryBorderControl
eu_neighbours <- import("./data/EU_neighbours.rds")

# Make longer
eu_neighbours <- eu_neighbours %>% 
  separate(neighbours_iso3, into = paste0("n", 1:9), extra = "drop") %>%
  pivot_longer(cols = n1:n9, values_to = "neighbour") %>%
  filter(!is.na(neighbour)) %>%
  select(-name) %>%
  mutate(neighbour_eu = countrycode(neighbour, "iso3c", "eurostat"),
         neighbour_name = countrycode(neighbour, "iso3c", "country.name.en")) %>%
  rename(iso3_state = state1,
         neighbour_iso3 = neighbour) %>%
  left_join(y = schengen.df[,c("implemented", "iso3_state")], 
            by = c("iso3_state" = "iso3_state")) %>%
  left_join(y = schengen.df[,c("implemented", "iso3_state")], 
            by = c("neighbour_iso3" = "iso3_state")) %>%
  rename(implemented = implemented.x,
         neighbour_implemented = implemented.y)

borders.df <- eu_neighbours %>%
  mutate(year = list(seq(1999, 2020, 1))) %>%
  unnest(year) %>%
  filter(year(round_date(implemented, "year")) <= year & 
           year(round_date(neighbour_implemented, "year")) <= year) %>%
  group_by(year) %>%
  nest() %>%
  rename(borders = data) %>%
  arrange(year)

# Import shapefile
# https://github.com/thomasp85/ggraph/issues/24
# shp-file from naturalearth filtered for Schengen Member States
schengen.shp <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name_en %in% schengen.df$name)

# Get centroids
# Country names and centroids
ctr.sf <- st_centroid(schengen.shp)

# Get countries
countries <- ctr.sf %>%
  pull(name_en) %>%
  countrycode(., origin = "country.name.en", "iso3c")

# Get centroids
ctr <- as.data.frame(as(st_geometry(ctr.sf$geometry), "Spatial")@coords) %>%
  cbind(countries) %>%
  rename(name = countries,
         x = coords.x1,
         y = coords.x2) %>%
  select(name, everything()) %>%
  arrange(name) %>%
  data.frame() 

# Manual jittering based on coordinates
# X
ctr[ctr$name == "AUT", c("x")] <- ctr[ctr$name == "AUT", c("x")] + 3
ctr[ctr$name == "BEL", c("x")] <- ctr[ctr$name == "BEL", c("x")] - 5
ctr[ctr$name == "CHE", c("x")] <- ctr[ctr$name == "CHE", c("x")] - 8
ctr[ctr$name == "CZE", c("x")] <- ctr[ctr$name == "CZE", c("x")] + 8
ctr[ctr$name == "DEU", c("x")] <- ctr[ctr$name == "DEU", c("x")] 
ctr[ctr$name == "DNK", c("x")] <- ctr[ctr$name == "DNK", c("x")] 
ctr[ctr$name == "ESP", c("x")] <- ctr[ctr$name == "ESP", c("x")] - 10
ctr[ctr$name == "EST", c("x")] <- ctr[ctr$name == "EST", c("x")] + 20
ctr[ctr$name == "FIN", c("x")] <- ctr[ctr$name == "FIN", c("x")] + 15
ctr[ctr$name == "FRA", c("x")] <- ctr[ctr$name == "FRA", c("x")] - 8
ctr[ctr$name == "GRC", c("x")] <- ctr[ctr$name == "GRC", c("x")] + 15
ctr[ctr$name == "HUN", c("x")] <- ctr[ctr$name == "HUN", c("x")] + 12
ctr[ctr$name == "ISL", c("x")] <- ctr[ctr$name == "ISL", c("x")] - 15
ctr[ctr$name == "ITA", c("x")] <- ctr[ctr$name == "ITA", c("x")] 
ctr[ctr$name == "LIE", c("x")] <- ctr[ctr$name == "LIE", c("x")] - 2
ctr[ctr$name == "LTU", c("x")] <- ctr[ctr$name == "LTU", c("x")] + 18
ctr[ctr$name == "LUX", c("x")] <- ctr[ctr$name == "LUX", c("x")] - 5
ctr[ctr$name == "LVA", c("x")] <- ctr[ctr$name == "LVA", c("x")] + 18
ctr[ctr$name == "MLT", c("x")] <- ctr[ctr$name == "MLT", c("x")] + 5
ctr[ctr$name == "NLD", c("x")] <- ctr[ctr$name == "NLD", c("x")] - 5
ctr[ctr$name == "NOR", c("x")] <- ctr[ctr$name == "NOR", c("x")] + 5
ctr[ctr$name == "POL", c("x")] <- ctr[ctr$name == "POL", c("x")] + 12
ctr[ctr$name == "PRT", c("x")] <- ctr[ctr$name == "PRT", c("x")] -15
ctr[ctr$name == "SVK", c("x")] <- ctr[ctr$name == "SVK", c("x")] + 15
ctr[ctr$name == "SVN", c("x")] <- ctr[ctr$name == "SVN", c("x")] + 10
ctr[ctr$name == "SWE", c("x")] <- ctr[ctr$name == "SWE", c("x")] + 10

# Y
ctr[ctr$name == "AUT", c("y")] <- ctr[ctr$name == "AUT", c("y")] - 10
ctr[ctr$name == "BEL", c("y")] <- ctr[ctr$name == "BEL", c("y")] + 11
ctr[ctr$name == "CHE", c("y")] <- ctr[ctr$name == "CHE", c("y")] -10
ctr[ctr$name == "CZE", c("y")] <- ctr[ctr$name == "CZE", c("y")] - 3
ctr[ctr$name == "DEU", c("y")] <- ctr[ctr$name == "DEU", c("y")] 
ctr[ctr$name == "DNK", c("y")] <- ctr[ctr$name == "DNK", c("y")] + 17
ctr[ctr$name == "ESP", c("y")] <- ctr[ctr$name == "ESP", c("y")] - 15
ctr[ctr$name == "EST", c("y")] <- ctr[ctr$name == "EST", c("y")] + 17
ctr[ctr$name == "FIN", c("y")] <- ctr[ctr$name == "FIN", c("y")] + 15
ctr[ctr$name == "FRA", c("y")] <- ctr[ctr$name == "FRA", c("y")] - 5
ctr[ctr$name == "GRC", c("y")] <- ctr[ctr$name == "GRC", c("y")] - 15
ctr[ctr$name == "HUN", c("y")] <- ctr[ctr$name == "HUN", c("y")] - 10
ctr[ctr$name == "ISL", c("y")] <- ctr[ctr$name == "ISL", c("y")] + 15
ctr[ctr$name == "ITA", c("y")] <- ctr[ctr$name == "ITA", c("y")] - 15
ctr[ctr$name == "LIE", c("y")] <- ctr[ctr$name == "LIE", c("y")] - 5
ctr[ctr$name == "LTU", c("y")] <- ctr[ctr$name == "LTU", c("y")] + 3
ctr[ctr$name == "LUX", c("y")] <- ctr[ctr$name == "LUX", c("y")] + 1
ctr[ctr$name == "LVA", c("y")] <- ctr[ctr$name == "LVA", c("y")] + 10
ctr[ctr$name == "MLT", c("y")] <- ctr[ctr$name == "MLT", c("y")] - 15
ctr[ctr$name == "NLD", c("y")] <- ctr[ctr$name == "NLD", c("y")] + 18
ctr[ctr$name == "NOR", c("y")] <- ctr[ctr$name == "NOR", c("y")] + 18
ctr[ctr$name == "POL", c("y")] <- ctr[ctr$name == "POL", c("y")] + 5
ctr[ctr$name == "PRT", c("y")] <- ctr[ctr$name == "PRT", c("y")] - 15
ctr[ctr$name == "SVK", c("y")] <- ctr[ctr$name == "SVK", c("y")] - 3
ctr[ctr$name == "SVN", c("y")] <- ctr[ctr$name == "SVN", c("y")] - 15
ctr[ctr$name == "SWE", c("y")] <- ctr[ctr$name == "SWE", c("y")] + 15
  
# Combine as data frame
nodes.df <- ctr %>%
  mutate(year = paste0(seq(1999, 2020, 1), collapse = ",")) %>%
  separate_rows(year) %>%
  mutate(year = strtoi(year)) %>%
  left_join(y = schengen.df[c("implemented", "iso3_state")], 
            by = c("name" = "iso3_state")) %>%
  filter(year(floor_date(implemented, "year")) <= year) %>% # Implementation date is rounded to nearest year
  group_by(year) %>%
  nest() %>% 
  rename(nodes = data) %>%
  arrange(year) 

# Prepare network data
edges.df <- bcontrol.df %>%
  group_by(iso3_state, neighbour_iso3, year = year(floor_date(ymd(begin), "year"))) %>%
  summarise(weight = sum(check_length)) %>%
  mutate(weight = ifelse(weight > 365, 365, weight)) %>%    # Some borders are subject to
  rename(from = iso3_state, to = neighbour_iso3) %>%        # TBC for more than 365 days       
  group_by(year) %>%                                        # per year.
  nest() %>%
  rename(edges = data) %>%
  arrange(year)

# Graph
graph.df <- list(edges.df, nodes.df, borders.df) %>%
  reduce(inner_join, by = "year") %>%
  mutate(n_borders = map_dbl(borders, ~nrow(.x)),
         n_edges = map_dbl(edges, ~nrow(.x)),
         density = n_edges / n_borders * 100,
         graphs = list(graph_from_data_frame(d = edges, vertices = nodes, 
                                             directed = TRUE)),
         reciprocity = map_dbl(graphs, ~reciprocity(.x, ignore.loops = T) * 100), 
         plots = pmap(list(graphs, density, reciprocity, year), 
                      ~ggraph(graph = ..1, layout = "manual", x = x, y = y) +
                        geom_edge_parallel(aes(colour = weight), 
                                           start_cap = circle(8, "mm"), 
                                           end_cap = circle(8, "mm"),
                                           arrow = arrow(type = "closed", 
                                                         length = unit(3, "mm")),
                                           sep = unit(5, "mm")) +
                        geom_node_text(aes(label = name), size = 5) +
                        scale_edge_colour_continuous(low = "#f0f0f0", 
                                                     high = "#000000", 
                                                     guide = "edge_colourbar", 
                                                     limits = c(1, 365),
                                                     name = "Number of days") +
                        annotate("text", x = -20, y = 80, 
                                 label = paste0("Network density: ", 
                                                round(..2, 0), "%",
                                                "\n",
                                                "Reciprocity: ",
                                                round(..3, 0), "%"),
                                 hjust = "left", size = 6) +
                        annotate("rect", xmin = -25, xmax = 5, ymin = 76, ymax = 84,
                                 alpha = .2) +
                        annotate("text", x = Inf, y = Inf, fontface = "bold", 
                                 vjust = "inward", hjust = "inward", 
                                 label = paste0(..4), size = 8) +
                        theme_graph() +
                        theme(legend.title = element_text(size = 18), 
                              legend.text = element_text(size = 16)))) %>%
  ungroup()

# Remove legend for combined publication plot
graph.df$plots[[22]] <- graph.df$plots[[22]] + theme(legend.position = "none")

# Length of TBCs
# Note: In 2020, the mean length is 193.98 if durations > 365 are not excluded else 
# 187.56. This is done in line 311 of the script. 
graph.df %>%
  filter(year %in% c(2015, 2020)) %>%
  ungroup() %>%
  select(edges) %>%
  map(.x = .$edges, .f = ~psych::describe(.x$weight))

# Which borders are affected by multiple TBCs
graph.df %>%
  map(.x = .$edges, .f = ~filter(.x, weight > 365))

# Detail information on 2015 and 2020
# 2015
graph.df %>% 
  filter(year %in% c(2015)) %>% 
  pull(edges) %>%
  .[[1]] %>%
  print(n = 100)

# 2020
graph.df %>% 
  filter(year %in% c(2020)) %>% 
  pull(edges) %>%
  .[[1]] %>%
  print(n = 100)

# Network density
### ------------------------------------------------------------------------ ###
# Figure
density.plot <- graph.df %>%
  ggplot() +
  geom_bar(aes(x = year, y = density), stat = "identity", fill = "#000000") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(x = "", y = "",
       title = "",
       caption = "Note: Temporary border controls that persist across years are counted as separate cases.") +
  theme.basic +
  theme(axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 16))

# Table (Appendix A2)
density.tbl <- graph.df %>%
  select(year, nodes, n_borders, n_edges, density) %>%
  mutate(nodes = map(nodes, ~.x %>%
                       n_distinct(.$name))) %>%
  gt() %>%
  fmt_percent(
    columns = "density",
    decimals = 1,
    scale_values = FALSE
  ) %>%
  cols_align(align = "right")

# Reason behind TBC
### ------------------------------------------------------------------------ ###
# Collapse categories further (specific events to event; crime and terrorism to 'terrorism & criminality')
bcontrol.df <- bcontrol.df %>%
  mutate(collapsed_category = 
           case_when(
             str_detect(category, "event") == TRUE ~ "event",
             str_detect(category, "crime|terrorism") == TRUE ~ "terrorism & criminality",
             is.na(category) == TRUE ~ "missing information",
             TRUE ~ category))

# Collapse further:
bcontrol.df <- bcontrol.df %>%
  mutate(full_collapse = 
           case_when(
             str_detect(collapsed_category, "public order and security|pandemic, terror & migration|pandemic & terror|pandemic") ~ "other",
             str_detect(collapsed_category, "crime|terrorism") == TRUE ~ "terrorism & criminality", 
             TRUE ~ collapsed_category
           ),
         full_collapse = factor(full_collapse, 
                                levels = c("terrorism & criminality", "crimmigration", "migration", "event", "other", "missing information"),
                                labels = c("Terrorism & Criminality", "Crimmigration", "Migration", "Event", "Other", "Missing information")))

# Main reason by year
bcontrol_reason.df <- bcontrol.df %>%
  group_by(year = year(floor_date(ymd(begin), "year"))) %>%
  count(full_collapse) %>%
  arrange(year, desc(n)) %>%
  slice(1)

# TBC by MS throughout years
### ------------------------------------------------------------------------ ###
ms_checks.df <- bcontrol.df %>%
  distinct(iso3_state, neighbour_iso3, year = year(floor_date(ymd(begin), "year"))) %>%
  mutate(iso3_state = factor(iso3_state, levels = schengen.df$iso3_state)) %>%
  count(iso3_state, year) %>%
  complete(iso3_state, year = 1999:2020) %>%
  mutate(binary_n = if_else(is.na(n), 0, 1)) %>%
  group_by(iso3_state) %>%
  mutate(total_checks = sum(binary_n)) %>%
  ungroup()

# Main reason by year and MS
bcontrol_reason.df <- bcontrol.df %>%
  group_by(iso3_state, year = year(floor_date(ymd(begin), "year"))) %>%
  count(full_collapse) %>%
  arrange(iso3_state, year, desc(n)) %>%
  slice(1) %>%
  select(-n)

# Merge
ms_checks.df <- ms_checks.df %>%
  left_join(y = bcontrol_reason.df)

# Waffle plot
ms_checks_year.fig <- ggplot(ms_checks.df) +
  geom_waffle(aes(x = year, y = fct_reorder(iso3_state, total_checks), fill = full_collapse)) +
  scale_fill_viridis(name = "Reason", 
                     discrete = TRUE,
                     na.value = "#FFFFFF", 
                     na.translate = FALSE,
                     option = "E") +
  labs(x = "", y = "",
       caption = "Note: Temporary border controls that persist across years are counted as separate cases.") +
  theme.basic +
  theme(axis.text = element_text(size = 16),
        plot.caption = element_text(size = 14),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16))

# Total number of years with temporary border controls
ms_checks.df %>%
  distinct(iso3_state, total_checks) %>%
  arrange(desc(total_checks))

# Appendix 1: Changes in the composition of the Schengen Area, 1999-October 2020
### ------------------------------------------------------------------------ ###
# All members
ms_borders.df <- graph.df %>% 
  unnest(nodes) %>%
  select(year, name, n_borders) %>%
  arrange(year, name) %>%
  group_by(year) %>%
  mutate(n_members = n()) %>%
  ungroup() %>%
  group_by(year, n_borders, n_members) %>%
  summarize(name = paste(name, collapse = ", ")) %>%
  select(year, name, n_borders, n_members)

# New members by accession
ms_new_borders.df <- graph.df %>% 
  unnest(nodes) %>%
  select(year, name, n_borders) %>%
  arrange(year, name) %>%
  group_by(year) %>%
  mutate(n_members = n()) %>%
  ungroup() %>%
  group_by(name) %>%
  arrange(year) %>%
  filter(row_number() == 1) %>%
  group_by(year, n_borders, n_members) %>%
  summarize(name = paste(name, collapse = ", ")) %>%
  select(year, name, n_borders, n_members)

# Appendix 3: Reasons by member states across time (table A3)
### ------------------------------------------------------------------------ ###
reason_n.tbl <- bcontrol.df %>%
  group_by(iso3_state, year = year(floor_date(ymd(begin), "year"))) %>%
  count(full_collapse) %>%
  ungroup() %>%
  arrange(iso3_state, year, desc(n)) %>%
  mutate(reason_n = paste0(full_collapse, " (N=", n, ")")) %>%
  group_by(iso3_state, year) %>%
  summarise(reason_n = paste0(reason_n, collapse = ", ")) %>%
  ungroup() %>%
  complete(iso3_state, year = 1999:2020) %>% 
  pivot_wider(id_cols = year, 
              names_from = iso3_state, 
              values_from = reason_n)

# Split into two tables
# Part 1
table_a3_1.tbl <- reason_n.tbl %>%
  select(1, 2:12) %>%
  gt() %>%
  fmt_missing(columns = everything(), rows = everything(), missing_text = "")

# Part 2
table_a3_2.tbl <- reason_n.tbl %>%
  select(1, 13:23) %>%
  gt() %>%
  fmt_missing(columns = everything(), rows = everything(), missing_text = "")

# Robustness check: find overlapping intervals in the dataset
### ------------------------------------------------------------------------ ###
# adapted from: https://stackoverflow.com/questions/53213418/r-collapse-and-merge-overlapping-time-intervals
bcontrol_overlap.df <- bcontrol.df %>%
  group_by(iso3_state, neighbour_iso3) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id, begin) %>%
  mutate(indx = c(0, cumsum(as.numeric(lead(begin)) + 1  > # find overlaps
                              cummax(as.numeric(end)))[-n()])) %>%
  ungroup() %>%
  group_by(group_id, indx) %>%
  arrange(desc(check_length)) %>% # retain lowest/largest (desc) interval
  filter(row_number() == 1)

# Segment plot: Border controls by Member States throughout time
### ------------------------------------------------------------------------ ###
# comparable to: https://github.com/dw-data/border-controls

# Segment plots for each MS
bcontrol_segment.df <- bcontrol.df %>%
  group_by(member_state) %>%
  nest() %>%
  arrange(member_state) %>%
  mutate(plots = map2(.x = data, .y = member_state, 
                      ~ggplot(data = .x,
                              aes(x = as.Date(begin), xend = as.Date(end), 
                                  y = fct_rev(neighbour_iso3), 
                                  yend = fct_rev(neighbour_iso3),
                                  colour = category)) + 
                        geom_segment(size = 5) +
                        scale_x_date(limits = as.Date(c("2006-01-01", "2020-12-31"))) +
                        labs(title = paste0(.y), x = "", y = "") +
                        theme.basic))

# Saving data, figures and tables
### ------------------------------------------------------------------------ ###

# Figure 1 - Data Processing

# Figure 2 - Temporary border controls by year/country
ggsave(
  plot = checks_year_length.fig, "./figures/Fig 2 - Number and length of TBC by year.eps", width = 11, height = 8, unit = "in",
  dpi = 1200
)

# Figure 3 - Density plot
ggsave(
  plot = density.plot, "./figures/Fig 3 - Network density.eps", width = 10, height = 5, unit = "in",
  dpi = 1200
)

# Figure 4 - Network Graphs (2015 / 2020)
graph.df %>%
  filter(year %in% c(2015, 2020)) %>%
  map2(.x = .$plots, .y = .$year, .f = ~ggsave(
    plot = .x, filename = paste0("./figures/Fig 3 - Network Graph ", .y, ".eps"),
    width = 11, height = 8, unit = "in",
    dpi = 1200,
    device = cairo_ps))

# Figure 5 - MS use of TBC by year
ggsave(
  plot = ms_checks_year.fig, "./figures/Fig 5 - MS use of TBC by year.eps", width = 11, height = 8, unit = "in",
  dpi = 1200, device = cairo_ps
)

# Table A1 - Changes in the composition of the Schengen Area, 1999-October 2020
export(ms_new_borders.df, "./figures/Table A1 - Changes in composition of Schengen Area.txt")

# Table A2 - Density 
gtsave(density.tbl, "./figures/Table A2 - Network Density.rtf")

# Table A3 - Reasons by MS throughout time
# Part 1
gtsave(table_a3_1.tbl, "./figures/Table A3 P1 - Reasons_MS_Year.rtf")
# Part 2
gtsave(table_a3_2.tbl, "./figures/Table A3 P2 - Reasons_MS_Year.rtf")


