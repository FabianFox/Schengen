# Data for dyads DEU-AUT, DNK-SWE, DNK-DEU
# created for PETI/LIBE committee hearing on 23/07/18

# Data from 2000 - 2020
# run 3_DataAnalysis-SchengenTemporaryBorderControl.R until line 362
tbc.df <- graph.df %>% 
  select(year, edges) %>%
  unnest(edges) %>% 
  mutate(dyad = str_c(from, "_", to)) %>% 
  filter(dyad %in% c("DEU_AUT", "DNK_SWE", "DNK_DEU")) 

# Add data from 2021 - 2023/07/14
tbc_new.df <- tribble(
  ~year, ~from, ~to, ~weight, ~dyad,
  2021, "DEU", "AUT", 183, "DEU_AUT",
  2021, "DNK", "DEU", 183, "DNK_DEU",
  2021, "DNK", "SWE", 183, "DNK_SWE",
  2021, "DEU", "AUT", 49, "DEU_AUT",
  2022, "DEU", "AUT", 130, "DEU_AUT",
  2021, "DNK", "DEU", 49, "DNK_DEU",
  2022, "DNK", "DEU", 130, "DNK_DEU",
  2021, "DNK", "SWE", 49, "DNK_SWE",
  2022, "DNK", "SWE", 130, "DNK_SWE",
  2022, "DNK", "DEU", 183, "DNK_DEU",
  2022, "DNK", "SWE", 183, "DNK_SWE",
  2022, "DEU", "AUT", 183, "DEU_AUT",
  2022, "DNK", "DEU", 49, "DNK_DEU",
  2022, "DNK", "SWE", 49, "DNK_SWE",
  2023, "DNK", "SWE", 130, "DNK_SWE",
  2023, "DNK", "DEU", 130, "DNK_DEU",
  2022, "DEU", "AUT", 49, "DEU_AUT",
  2023, "DEU", "AUT", 130, "DEU_AUT", 
  2023, "DEU", "AUT", 183, "DEU_AUT",
  2023, "DNK", "DEU", 183, "DNK_DEU") %>%
  group_by(year, dyad, from, to) %>%
  summarize(weight = sum(weight)) %>%
  ungroup()

# Join
tbc.df <- tbc.df %>%
  bind_rows(tbc_new.df)

# Export
export(tbc.df, "./data/TemporaryBorderControls_Hearing.rds")

# Create figure
tbc.df %>%
  ggplot(aes(x = year, y = weight)) +
  geom_bar(stat = "identity") +
  facet_wrap(~dyad, nrow = 3) +
  theme.basic

