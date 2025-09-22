###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# install/load packages
pacman::p_load(readxl, lubridate, haven, dplyr, tidyr, stringr, countrycode, ggplot2, rnaturalearth, rnaturalearthdata, RColorBrewer)

#### 0. IMPORT/CLEAN DATA #### 
df <- read_excel("20250830 WHO_Extracted-data_Consensed_review_457022_20250830231950.xlsx")
# see that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."
# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)

# we need two databases: 1) one with one row/observation per study, to summarize study characteristics, and 
# 2) one with a row per analysis done, to report on the associations measured (longer format) -> one row per comarison (so-called model)
# 

#### 1. DESCRIPTION OF STUDIES ####
# 1.1 create a map with the frequency of studies by country
countries <- df %>%
  select(Study_country) %>%
  mutate(Study_country = str_remove_all(Study_country, "Other:\\s*")) %>% 
  mutate(Study_country = str_replace_all(Study_country, c(
    "Brasil" = "Brazil",
    "UK" = "United Kingdom",
    "South-Korea" = "South Korea",
    "Korea" = "South Korea",   # careful: will also catch "North Korea" if present
    "OPT" = "Palestine"        # adapt as needed
  ))) %>%
  # split multi-country entries into separate rows
  mutate(Study_country = str_split(Study_country, ",|;")) %>%
  unnest(Study_country) %>%
  mutate(Study_country = str_trim(Study_country)) %>%
  mutate(iso3 = countrycode(Study_country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3))   # drop rows that couldn’t be matched
country_counts <- countries %>%
  count(iso3, name = "n")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- world %>%
  left_join(country_counts, by = c("iso_a3" = "iso3"))
ggplot(world_data) +
  geom_sf(aes(fill = n)) +
  theme_minimal() +
  labs(
    title = "Number of studies per country",
    fill = "Studies" ) + 
  scale_fill_distiller(
  palette = "YlOrRd",  # yellow → orange → red
  direction = 1,
  na.value = "grey90"
)
# 1.2 level of healthcare
df <- df %>%
  mutate(facilitylevel = case_when(
      str_detect(Healthcare_facility_type, regex("Primary|community", ignore_case = TRUE)) ~ "Primary",
      str_detect(Healthcare_facility_type, regex("Secondary|general|regional|district", ignore_case = TRUE)) ~ "Secondary",
      str_detect(Healthcare_facility_type, regex("Tertiary|university|academic|quaternary|paediatric hospital", ignore_case = TRUE)) ~ "Tertiary",
      str_detect(Healthcare_facility_type, regex("Unknown|unclear|NA", ignore_case = TRUE)) ~ "Unknown/unclear",
      TRUE ~ "Mixed/Other"))
df_summary <- df %>%
  count(facilitylevel, name = "n") %>%
  arrange(desc(n))
df_summary$facilitylevel <- factor(df_summary$facilitylevel, levels = c("Unknown/unclear", "Mixed/Other", "Tertiary", "Secondary", "Primary"))
ggplot(df_summary, aes(x = facilitylevel, y = n, fill = facilitylevel)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() + 
  scale_fill_manual(values = c(
    "Primary" = "#005AB5", 
    "Secondary" = "#DC3220", 
    "Tertiary" = "#7B8D8E", 
    "Unknown/unclear" = "#999999", 
    "Mixed/Other" = "#E17C05"
  )) +
  labs(
    title = "Studies by healthcare facility level",
    x = NULL,
    y = "Number of studies"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
# 1.3. patient population
patientpop <- df %>% select(Population_admitting_ward) %>%
  mutate(Population_admitting_ward = str_split(Population_admitting_ward, ";")) %>%
  unnest(Population_admitting_ward) %>%
  mutate(Population_admitting_ward = str_trim(Population_admitting_ward),
         Population_admitting_ward = case_when(
           str_detect(Population_admitting_ward, "Critical") ~ "Critical",
           str_detect(Population_admitting_ward, "Immunocompromised") ~ "Immunocompromised",
           str_detect(Population_admitting_ward, "Neonates") ~ "Neonates",
           str_detect(Population_admitting_ward, "Pediatric") ~ "Pediatric",
           str_detect(Population_admitting_ward, "Adult") ~ "Adult",
           str_detect(Population_admitting_ward, "Emergency") ~ "Critical",
           str_detect(Population_admitting_ward, "Surgical") ~ "Surgical/Burns",
           str_detect(Population_admitting_ward, "General") ~ "General",
           str_detect(Population_admitting_ward, "Unknown") ~ "Unknown/Unclear",
           str_detect(Population_admitting_ward, "Other") ~ "Other",
           str_detect(Population_admitting_ward, "Infectious diseases (ie covid19)") ~ "Other",
           TRUE ~ Population_admitting_ward
         ))
patientpop_summary <- patientpop %>%
  count(Population_admitting_ward, name = "n") %>%
  arrange(desc(n))
print(patientpop_summary)
ggplot(patientpop_summary, aes(x = reorder(Population_admitting_ward, n), y = n, fill = Population_admitting_ward)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Adult" = "#005AB5",
    "Pediatric" = "#DC3220",
    "Neonates" = "#E17C05",
    "Critical" = "#7B8D8E",
    "Immunocompromised" = "#4C9F70",
    "Surgical/Burns" = "#FF9F1C",
    "General" = "#999999",
    "Other" = "#8E6C8A",
    "Unknown/Unclear" = "#CCCCCC"
  )) +
  labs(
    title = "Studies by patient population",
    x = NULL,
    y = "Number of studies"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


#### 2. SUMMARY OF PROXY INDICATORS ####
# exposures of interest that are potential indicators are saved under variable "resistant_group variable-1_name"??
table(df$`resistant_group variable-1_name`)

