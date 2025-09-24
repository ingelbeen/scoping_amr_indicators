###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# install/load packages
pacman::p_load(readxl, writexl, lubridate, haven, dplyr, tidyr, stringr, countrycode, ggplot2, forcats, rnaturalearth, rnaturalearthdata, RColorBrewer)

#### 0. IMPORT/CLEAN DATA #### 
df <- read_excel("20250830 WHO_Extracted-data_Consensed_review_457022_20250830231950.xlsx")
# see that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."
# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)
# remove all empty columns
df <- df %>%  select(where(~ any(!is.na(.) & . != "")))

# we need three databases for the planned analyses: 
# 1) one with one row/observation per study, to summarize study characteristics,  
# 2) one with a row per analysis (i.e, comparison AMR vs susceptible) done, to report on the associations measured (longer format) -> one row per comparison (so-called 'model')
# 3) one with a row per variable of interest (indicator/predictor) reported, to summarize proxy indicators

# create df_long, transforming the dataframe df to a long format with a row for each comparison of R vs S (model_1, model_2 and model_3)
df$`resistant_group SD_2` <- as.character(df$`resistant_group SD_2`) # vars need to be in the same format to be combined
df$`resistant_group SD_3` <- as.character(df$`resistant_group SD_3`)
df$`Model_3 Resistant_group_tot_nb` <- as.character(df$`Model_3 Resistant_group_tot_nb`)
df$`Model_3 Susceptible_group_tot_nb` <- as.character(df$`Model_3 Susceptible_group_tot_nb`)

df_long <- df %>%
  pivot_longer(
    cols = matches("^Model_[123]"), 
    names_to = c("Model", ".value"),
    names_pattern = "Model_(\\d+) (.*)"
  )
# remove rows which don't contain a comparison -> 216 had model 1, another 24 had a model 2, another 5 had a model 3
df_long <- df_long %>% filter(!is.na(Resistant_grp_definition))

colnames(df)
print(colnames(df_long), max = 1900)
table(df_long$Model)
table(df_long$Resistant_grp_definition)
table(df_long$`Number_1 Resistant_group_definition`)

# clean resistance profiles
table(df_long$Resistant_grp_definition, useNA = "always")
# replace non specific 'resistance' by other variable values that have the specific resistance profile studied
df_long <- df_long %>%
  mutate(
    Resistant_grp_definition = case_when(
      Resistant_grp_definition %in% c("AMR", "Resistant", "Resistance +", "MRO", "MDRO", "Resistance to First-line Antibiotics") ~ 
        coalesce(AMR_mechanism_1, AMR_mechanism_2, Resistant_grp_definition),
      TRUE ~ Resistant_grp_definition))
resist_models <- as.data.frame(table(df_long$Resistant_grp_definition, useNA = "always"))
write.table(resist_models, "resist_models.txt")

# group other values
df_long <- df_long %>%
  mutate(
    Resistance_group = case_when(
      grepl("MRSA", Resistant_grp_definition, ignore.case = TRUE) ~ "methicillin resistance",
      grepl("ESBL", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("VRE", Resistant_grp_definition, ignore.case = TRUE) ~ "vancomycin resistance",
      grepl("\\bMDR\\b", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR (some defined, some non specified)",
      grepl("(MRO)", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR (some defined, some non specified)",
      grepl("MDRAB", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR (some defined, some non specified)",
      grepl("MDRO", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin, carbapenem or methicillin resistance (depending on isolated pathogen)",
      grepl("Resistance (+)", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin, carbapenem or methicillin resistance (depending on isolated pathogen)",
      grepl("XDR", Resistant_grp_definition, ignore.case = TRUE) ~ "XDR (not defined)",
      grepl("3GC", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("EPE", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("third generation", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("third-generation", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("Cephalosporin-resistant", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("Ceftriaxone-resistant", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("ESCR", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("Broad-spectrum cephalosp", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("cefotax", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("colistin", Resistant_grp_definition, ignore.case = TRUE) ~ "colistin resistance",
      grepl("ciprofloxacin|levofloxacin|fluoroquinolone", Resistant_grp_definition, ignore.case = TRUE) ~ "fluoroquinolone resistance",
      grepl("trimethoprim|sulfamethoxazole|co-trimoxazole", Resistant_grp_definition, ignore.case = TRUE) ~ "TMP/SMX resistance",
      grepl("nitrofurantoin", Resistant_grp_definition, ignore.case = TRUE) ~ "nitrofurantoin resistance",
      grepl("piperacillin|tazobactam", Resistant_grp_definition, ignore.case = TRUE) ~ "pip/tazo resistance",
      grepl("oxacillin|penicillin|linezolid", Resistant_grp_definition, ignore.case = TRUE) ~ "methicillin and linezolid resistance",
      grepl("CR", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("Carba", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("CPE", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("Meropenem-nonsus", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("KPC", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("reduced susceptibility to cefuroxime and genta", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR to empiric treatment",
      grepl("CASR", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem + ampicillin/sulbactam resistance",
      grepl("resistant group", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR to empiric treatment",
      grepl("Ampicillin and gentamicin", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR to empiric treatment",
      grepl("SNS", Resistant_grp_definition, ignore.case = TRUE) ~ "sulbactam non susceptible",
      grepl("LNZ", Resistant_grp_definition, ignore.case = TRUE) ~ "linezolid susceptible",
      grepl("imipenem-res", Resistant_grp_definition, ignore.case = TRUE) ~ "imipenem resistance",
      grepl("HLRG", Resistant_grp_definition, ignore.case = TRUE) ~ "high-level gentamicin resistance",
      grepl("Drug resistant gram posit", Resistant_grp_definition, ignore.case = TRUE) ~ "not specified resistance",
      TRUE ~ Resistant_grp_definition))   # keep original for unique/rare categories

# create 'indicators', an even longer df, with one row per variable/exposure of interest reported
colnames(df_long)
df_long <- df_long %>% rename_with(~ str_replace_all(., "-(\\d+)_name", "_\\1")) # make sure all variable names belonging to the same indicator have the same number at the end
# reformat variables that should be combined
df_long <- df_long %>% mutate(across(contains("resistant_group mean_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group SD_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group p-value_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("compartor_group mean_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group SD_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group p-value_"), as.character))
df_long <- df_long %>% mutate(across(contains("compartor_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("compartor_group SD_"), as.character))

# reshape df_long to into a longer format, in which all variables ending with _1, _2, up to _20 are brought together in the same variable, so that for each number there is one row
df_longer <- df_long %>%  pivot_longer(
    cols = matches("_(?:[1-9]|1[0-9]|20)$"),   # matches _1 to _20
    names_to = c(".value", "set"),             # .value keeps base var names
    names_pattern = "(.*)_(\\d+)$")
colnames(df_longer)

# probably still needs an even longer data format (df_longest?), to have all measures of association grouped in a single variable, instead of a separate column for each

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
           str_detect(Population_admitting_ward, "covid19") ~ "Other",
           TRUE ~ Population_admitting_ward
         ))
patientpop_summary <- patientpop %>%
  count(Population_admitting_ward, name = "n") %>%
  arrange(desc(n))
print(patientpop_summary) # not sure what to make of the category 'general'
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
    title = "Studies by patient population*",
    x = NULL,
    y = "Number of studies",
    caption = "*If multiple patient populations are included, each subpopulation is counted"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# bacterial isolates
isolates <- df %>%
  mutate(isolate = str_split(`Bacterial-isolate_type`, ";")) %>%
  unnest(isolate) %>%
  mutate(isolate = str_trim(isolate)) %>%
  mutate(isolate = str_remove(isolate, "^Other:")) %>%
  mutate(isolate = str_squish(isolate)) %>%
  # harmonize some key groups
  mutate(isolate = case_when(
    str_detect(isolate, "Escherichia coli|E\\. coli") ~ "Escherichia coli",
    str_detect(isolate, "Klebsiella") ~ "Klebsiella spp.",
    str_detect(isolate, "Enterobacter") ~ "Enterobacter spp.",
    str_detect(isolate, "Acinetobacter") ~ "Acinetobacter spp.",
    str_detect(isolate, "Pseudomonas") ~ "Pseudomonas spp.",
    str_detect(isolate, "Staphylococcus aureus") ~ "Staphylococcus aureus",
    str_detect(isolate, "Staphylococcus") ~ "Coagulase-negative staphylococci",
    str_detect(isolate, "Enterococci|Enterococcus") ~ "Enterococcus spp.",
    str_detect(isolate, "Streptococcus pneumoniae") ~ "Streptococcus pneumoniae",
    str_detect(isolate, "Streptococcus") ~ "Streptococcus spp.",
    str_detect(isolate, "Proteus") ~ "Proteus spp.",
    str_detect(isolate, "Serratia") ~ "Serratia spp.",
    str_detect(isolate, "Salmonella") ~ "Salmonella spp.",
    str_detect(isolate, "Citrobacter") ~ "Citrobacter spp.",
    TRUE ~ "Other/rare"
  ))
isolate_summary <- isolates %>%
  count(isolate, sort = TRUE)
ggplot(isolate_summary, aes(x = reorder(isolate, n), y = n, fill = isolate)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  labs(
    title = "Bacterial isolates reported in studies",
    subtitle = "Top pathogens are dominated by Gram-negatives",
    x = NULL, y = "Number of mentions",
    caption = "If multiple isolates are reported in a study, each is counted"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 10)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40")
  )

# summarize AMR profiles
counts <- df_long %>%
  count(Resistance_group, sort = TRUE)
counts
ggplot(counts, aes(x = fct_reorder(Resistance_group, n), y = n)) +
  geom_col(fill = "#800000") +
  coord_flip() +
  labs(
    x = "Resistance profile",
    y = "Number of analyses",
    title = "Distribution of reported AMR profiles in the analysis"
  ) +
  theme_minimal(base_size = 13)

# create a variable for each pathogen-antibiotic combination





#### 2. SUMMARY OF PROXY INDICATORS ####
# exposures of interest that are potential indicators are saved under variable "resistant_group variable-1_name"??
table(df$`resistant_group variable-1_name`)

