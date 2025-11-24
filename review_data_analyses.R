###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# last update: 20/10/2025 17h23

# install/load packages
pacman::p_load(here, readxl, writexl, openxlsx, lubridate, haven, dplyr, tidyr, stringr, countrycode, ggplot2, forcats, rnaturalearth, rnaturalearthdata, RColorBrewer, ggbeeswarm, DT, gt, scales, formattable, gtsummary, flextable, officer)

#### 0. IMPORT/CLEAN DATA ####
df <- read_excel("db/data_raw/20251011 review_457022_20251011204039_ALLminus6.xlsx") # note that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."

# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)

# #join df - additional 6 studies
# # read study_info sheet
# new_study_info <- read.xlsx("additional_studies_extract_info.xlsx", sheet = "study_info")
# #check that columns align
# setdiff(names(df), names(new_study_info))
# setdiff(names(new_study_info), names(df))
# #append new rows
# df <- bind_rows(df, new_study_info)
# #check
# nrow(df)

# remove all empty columns
# df <- df %>%  select(where(~ any(!is.na(.) & . != ""))) # gives an error
df <- df %>%  select(where(~ any(!is.na(.)) && (is.numeric(.) || is.logical(.) || any(as.character(.) != ""))))

# some variable names have twice the '_n' that refers to the n-th proxy indicator in that article. keep only the '_n' at the end of the column name
names(df) <- gsub("(?<=total)_[0-9]+", "", names(df), perl = TRUE)
names(df) <- gsub("resistant_20 ", "resistant_group ", names(df))
names(df) <- gsub("susceptible_compartor_group", "susceptible_comparator_group", names(df)) # typo in one of the column names (only for the first _1 continuous indicator)

# I here initially excluded those analyses that did not report AMR as an outcome measure, but that variable Outcome_measure_repoted (sic) might not be entered correctly, with some studies having cost or mortality as primary outcome but still reporting on an analysis with AMR as outcome
table(df$Outcome_measure_repoted)

# clean variables describing studies in df
# publication year
table(df$Publication_year, useNA = "always") # is ok

# a var 'analysisdesign' based on the var Study design
df$analysisdesign_simplified <- ifelse(grepl("control", df$Study_design, ignore.case = TRUE),"Case-control study", # nested case control within a cohort is considered case-control, since the analysis is
                                    ifelse(grepl("cohort|case-0series|surveillance", df$Study_design, ignore.case = TRUE), "Cohort study",
                                           ifelse(grepl("cross[ -]?sectional", df$Study_design, ignore.case = TRUE),"Cross-sectional study", "Other/not specified")))
table(df$Study_design, df$analysisdesign_simplified, useNA = "always")

# a var 'studypop', extracting age specific populations from the var 'Population_admitting_ward'
df$studypop <- case_when(
  grepl("adult", df$Population_admitting_ward, ignore.case = TRUE) &
    grepl("pediatric|paediatric", df$Population_admitting_ward, ignore.case = TRUE) &
    grepl("neonat", df$Population_admitting_ward, ignore.case = TRUE) ~ "All ages or not specified",
  grepl("adult", df$Population_admitting_ward, ignore.case = TRUE) &
    grepl("neonat", df$Population_admitting_ward, ignore.case = TRUE) ~ "All ages or not specified",
  grepl("adult", df$Population_admitting_ward, ignore.case = TRUE) &
    grepl("pediatric|paediatric", df$Population_admitting_ward, ignore.case = TRUE) ~ "Adults & pediatrics",
  grepl("pediatric|paediatric", df$Population_admitting_ward, ignore.case = TRUE) &
    grepl("neonat", df$Population_admitting_ward, ignore.case = TRUE) ~ "Pediatrics incl. neonates",
  grepl("adult", df$Population_admitting_ward, ignore.case = TRUE) ~ "Adults",
  grepl("pediatric|paediatric", df$Population_admitting_ward, ignore.case = TRUE) ~ "Pediatrics",
  grepl("neonat", df$Population_admitting_ward, ignore.case = TRUE) ~ "Neonates",
    TRUE ~ "All ages or not specified")
table(df$studypop, useNA = "always")

# a var 'highrisk" should ideally identify the specific studies in high-risk populations, the complication being that many studies have a mix of high risk and 'regular' hospitalisations
# now it excludes the studies that are also labelled "General"
#consider neonates with "non-high risk", neonates differences will show wiht comparison of age (neonate, peds, adults)
df$highrisk <- case_when(
  (grepl("Immunocompromised", df$Population_admitting_ward, ignore.case = TRUE)==T & grepl("general ", df$Population_admitting_ward, ignore.case = TRUE)==F) ~ "high risk",
  (grepl("Critical", df$Population_admitting_ward, ignore.case = TRUE)==T & grepl("general ", df$Population_admitting_ward, ignore.case = TRUE)==F) ~ "high risk",
  TRUE ~ "mixed or unspecified population")
# checked the studies with as admitting ward 'surgical and burns': only #3494 is actually burns (and is also labelled critical) – Burns and ICU patients included from a Burn unit “patients who had clinically relevant Pa BSIs and were registered in the database of the BICU of CologneMerheim Medical Center”
table(df$Population_admitting_ward, df$highrisk)

# study countries
df <- df %>% mutate(Study_country = str_remove_all(Study_country, "Other:\\s*"))
df <- df %>% mutate(Study_country = str_replace_all(Study_country, c(
    "Brasil" = "Brazil",
    "UK" = "United Kingdom",
    "South-Korea" = "South Korea",
    "South-Africa" = "South Africa",
    "OPT" = "Palestine")))
df$Study_country[df$Study_country=="Korea"] <- "South Korea"
table(df$Study_country)
# assign world bank income groups to the countries
income_lookup <- c(
  "Argentina" = "Upper middle income",
  "Australia" = "High income",
  "Australia; New Zealand" = "High income",
  "Bangladesh" = "Lower middle income",
  "Brazil" = "Upper middle income",
  "Canada" = "High income",
  "China" = "Upper middle income",
  "Colombia" = "Upper middle income",
  "Colombia, Argentina, Ecuador, Guatemala, Mexico, Peru, Venezuela" = "Upper middle income",
  "Egypt" = "Lower middle income",
  "Europe" = "High income",
  "France" = "High income",
  "French Guiana" = "Upper middle income",
  "Germany" = "High income",
  "Ghana" = "Lower middle income",
  "Greece" = "High income",
  "India" = "Lower middle income",
  "Indonesia" = "Lower middle income",
  "Iran" = "Upper middle income",
  "Israel" = "High income",
  "Italy" = "High income",
  "Italy, Spain, Germany, Croatia, Israel" = "High income",
  "Japan" = "High income",
  "Lebanon" = "Upper middle income",
  "Madagascar" = "Low income",
  "Malawi" = "Low income",
  "Mexico" = "Upper middle income",
  "Netherlands" = "High income",
  "Pakistan" = "Lower middle income",
  "Palestine" = "Lower middle income",
  "Peru" = "Upper middle income",
  "Scotland" = "High income",
  "Senegal" = "Low income",
  "Serbia" = "Upper middle income",
  "Singapore" = "High income",
  "South Africa" = "Upper middle income",
  "South Korea" = "High income",
  "Spain" = "High income",
  "Sweden" = "High income",
  "Switzerland" = "High income",
  "Taiwan" = "High income",
  "Tanzania" = "Low income",
  "Thailand" = "Upper middle income",
  "Turkey" = "Upper middle income",
  "United Kingdom; 12 countries: Spain (n = 14 centers), Turkey (n = 4), Brazil (n = 3), Italy (n = 3), Argentina (n = 2), Germany (n = 2), Chile (n = 1), Colombia (n = 1), Lebanon (n = 1), Slovakia (n = 1), Switzerland (n = 1), and the United Kingdom (n = 1" = "Multiple",
  "United States" = "High income",
  "24 countries from Europe (Turkey, Portugal, Italy, Slovak Republic, Serbia, France, Romania, Cyprus, Bosnia and Herzegovina, Kosovo, North Macedonia, Bulgaria, Belgium, Hungary), Middle East and North Africa (Egypt, Lebanon, Oman, Iran, Palestine), South Asia (Bangladesh, Pakistan, India), Latin America and the Caribbean (Puerto Rico), and East Asia (Thailand)" = "Multiple")
df$incomegroup <- income_lookup[df$Study_country]



# one study is an intervention study (11818) and therefore part of a different analysis - to be removed
df <- df %>% filter(Study_ID!="#11818")

#publication_year variable categorization
table(df$Publication_year)

#one study (#4321) has a publication year of 2009, needs to change to 2010 - verified
df <- df %>%
  mutate(
    Publication_year = if_else(Study_ID == "#4321" & Publication_year == 2009, 2010, Publication_year)
  )

df <- df %>%
  mutate(
    pub_year_cat = case_when(
      Publication_year >= 2010 & Publication_year <= 2014 ~ "2010–2014",
      Publication_year >= 2015 & Publication_year <= 2019 ~ "2015–2019",
      Publication_year >= 2020 & Publication_year <= 2024 ~ "2020–2024",
      TRUE ~ NA_character_
    )
  )

table(df$pub_year_cat, useNA = "ifany")


#facility type - recode
df <- df %>%
  mutate(
    facility_type = case_when(
      str_starts(Healthcare_facility_type, regex("Other", ignore_case = TRUE)) ~ "Mixed",
      TRUE ~ Healthcare_facility_type
    )
  )

df %>% count(Healthcare_facility_type, facility_type)
table(df$facility_type)

#BSI_SOURCE, deconcatenate - split each source in a different variable then count each - every study reports multiple source
#define list of sources

bsi_sources <- c(
    "Bone/joint",
    "Device-related (catheter, central)",
    "Intra-abdominal/gastrointestinal",
    "Skin-soft tissue",
    "Lower respiratory tract (ie pneumoniae)",
    "Urinary tract",
    "Primary",
    "Unknown/unspecified",
    "Surgical site",
    "Other"
  )

#define short labels for output
bsi_labels <- c(
  "Bone/joint"                          = "Bone/joint",
  "Device-related (catheter, central)"  = "Device-related",
  "Intra-abdominal/gastrointestinal"    = "Intra-abdominal/gastrointestinal",
  "Skin-soft tissue"                    = "Skin-soft tissue",
  "Lower respiratory tract (ie pneumoniae)" = "Respiratory",
  "Urinary tract"                       = "Urinary tract",
  "Primary"                             = "Primary",
  "Unknown/unspecified"          = "Unknown/unspecified source",
  "Surgical site"                       = "Surgical site",
  "Other"                        = "Other source"
)

#create one y/n col per bsi_source y/n
for (s in bsi_sources) {
  colname <- bsi_labels[[s]] %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    tolower()

  df[[colname]] <- if_else(
    str_detect(df$`BSI_suspected-source`, fixed(s, ignore_case = TRUE)),
    "Yes", "No"
  )
}

bsi_cols <- unname(bsi_labels) %>%                    # use values (labels)
  str_replace_all("[^A-Za-z0-9]+", "_") %>%
  tolower()

#summarize - counts + proportions
summary_bsi_sources <- df %>%
  summarise(across(all_of(bsi_cols),
                   ~ sum(tolower(trimws(as.character(.))) == "yes", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "BSI Source", values_to = "Count") %>%
  mutate(Proportion = sprintf("%.1f%%", Count / nrow(df) * 100)) %>%
  arrange(desc(Count))

summary_bsi_sources

#BSI_source_nb, variable to count, among known sources - excluding unknown - how many have one source (single), and how many >1 source (mixed)
#then among those unique, what are the specific sources reported

#a.identify columns with known sources (excluding unknown/unclear)
known_bsi_cols <- c(
  "bone_joint",
  "device_related",
  "intra_abdominal_gastrointestinal",
  "skin_soft_tissue",
  "respiratory",
  "urinary_tract",
  "primary",
  "surgical_site",
  "other_source"
)

#b.make sure standardized
df <- df %>%
  mutate(across(all_of(known_bsi_cols), ~ {
    v <- trimws(tolower(as.character(.)))
    case_when(
      v %in% c("true","yes","1")  ~ TRUE,
      v %in% c("false","no","0")  ~ FALSE,
      TRUE                        ~ NA
    )
  }))

#c.count number of known sources reported per study and classify as single or mixed
df <- df %>%
  mutate(
    bsi_source_count = rowSums(across(all_of(known_bsi_cols)), na.rm = TRUE),
    bsi_source_nb = case_when(
      bsi_source_count == 0 ~ NA_character_,
      bsi_source_count == 1 ~ "Single",
      bsi_source_count >  1 ~ "Mixed"
    )
  ) %>%
  ungroup()


#d.number and proportion among those with >=1 known source (mixed)
bsi_source_summary <- df %>%
  filter(!is.na(bsi_source_nb)) %>%
  count(bsi_source_nb) %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

bsi_source_summary

#d.among those unique - sources reported
bsi_unique_sources <- df %>%
  filter(bsi_source_nb == "Unique") %>%
  summarise(across(all_of(known_bsi_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Source", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1))

bsi_unique_sources

#ACQUISITION OF INFECTION (infection origin) - deconcatenate and create different col for each source
#a.check the "other" category to see if needs to be recategorized under existing ones
df %>%
  filter(str_detect(.data[["Infection_origin"]], regex("^Other", ignore_case = TRUE))) %>%
  distinct(Study_ID, .data[["Infection_origin"]])

#one study (#1363) replace the Other (ICU-acquired) under infection origin by hospital acquired
df <- df %>%
  mutate(
    Infection_origin = if_else(Study_ID == "#1363" &
                              Infection_origin  == "Other: Intensive care unit-acquired", "Hospital acquired infections",
                              Infection_origin)
  )

#b.create one variable per origin (yes/no)
infection_origin <- c(
  "Community acquired infections",
  "Hospital acquired infections",
  "Healthcare associated infections",
  "Unknown/unspecified origin", #just lable
  "Other origin" #just label
)

#c.create one col per infection origin with true/false
for (o in infection_origin) {
  colname <- o %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    tolower()

  # choose the right pattern
  if (o == "Unknown/unspecified origin") {
    pattern <- "Unknown/unspecified"
  } else if (o == "Other origin") {
    pattern <- "^Other"
  } else {
    pattern <- o
  }

  df[[colname]] <- str_detect(df$Infection_origin, regex(pattern, ignore_case = TRUE))
}

#c.summarise counts & proportions
origin_cols <- infection_origin %>%
  str_replace_all("[^A-Za-z0-9]+", "_") %>%
  tolower()

summary_infection_origin <- df %>%
  summarise(across(all_of(origin_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(),
               names_to = "Acquisition of infection",
               values_to = "Count") %>%
  mutate(Proportion = round(Count / nrow(df) * 100, 1)) %>%
  arrange(desc(Count))

summary_infection_origin

#Infection origin nb (one type only, >1 type =)
#a.define infection-origin columns (using exact column names)
infection_origin_cols <- c(
  "community_acquired_infections",
  "hospital_acquired_infections",
  "healthcare_associated_infections",
  "other_origin",
  "unknown_unspecified_origin"
)

#b.count known origins (exclude unknown) and classify Single / Mixed
known_infection_cols <- setdiff(infection_origin_cols, "unknown_unspecified_origin")

df <- df %>%
  mutate(
    infection_origin_count = rowSums(across(all_of(known_infection_cols)), na.rm = TRUE),
    infection_origin_nb = dplyr::case_when(
      infection_origin_count == 0 ~ NA_character_,
      infection_origin_count == 1 ~ "Single",
      infection_origin_count >  1 ~ "Mixed"
    )
  )

#c.counts & proportions among those with ≥1 known origin
infection_origin_summary <- df %>%
  filter(!is.na(infection_origin_nb)) %>%
  count(infection_origin_nb) %>%
  mutate(Proportion = round(n / sum(n) * 100, 1))

infection_origin_summary

#d.among the Unique group, which specific origins - among known origins only
infection_origin_unique <- df %>%
  filter(infection_origin_nb == "Single") %>%
  summarise(across(all_of(known_infection_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Acquisition of infection", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1))

infection_origin_unique

#Models - count studies with a/2/>=3 models

#name col
r_cols <- c(
  "Model_1 Resistant_group_tot_nb",
  "Model_2 Resistant_group_tot_nb",
  "Model_3 Resistant_group_tot_nb"
)
s_cols <- c(
  "Model_1 Susceptible_group_tot_nb",
  "Model_2 Susceptible_group_tot_nb",
  "Model_3 Susceptible_group_tot_nb"
)

#make numeric
df <- df %>%
  mutate(across(all_of(c(r_cols, s_cols)),
                ~ suppressWarnings(as.numeric(trimws(as.character(.))))))

#check how many models per study - note some studies have more than 3 but no space to fill them in so considered >=3
df <- df %>%
  mutate(
    n_models = rowSums(across(all_of(r_cols), ~ !is.na(.x) & .x != ""), na.rm = TRUE),
    model_nb = case_when(
      n_models == 1 ~ "1",
      n_models == 2 ~ "2",
      n_models >= 3 ~ ">2",
      TRUE ~ NA_character_
    ),
    #per-study totals across models ---
    total_resistant   = rowSums(across(all_of(r_cols)), na.rm = TRUE),
    total_susceptible = rowSums(across(all_of(s_cols)), na.rm = TRUE)
  )

#summary of model count categories
model_nb_summary <- df %>%
  count(model_nb) %>%
  mutate(Proportion = round(100 * n / sum(n, na.rm = TRUE), 1))

model_nb_summary

#Totals R and S - total population of R and S among all studies
res_by_model <- sapply(r_cols, function(x) sum(df[[x]], na.rm = TRUE))
sus_by_model <- sapply(s_cols, function(x) sum(df[[x]], na.rm = TRUE))

totals_by_model <- tibble(
  Model = c("Model 1","Model 2","Model 3"),
  Resistant_total   = unname(res_by_model),
  Susceptible_total = unname(sus_by_model)
)

totals_by_model

#overall totals across all models
overall_totals <- tibble(
  Resistant_all_models   = sum(res_by_model),
  Susceptible_all_models = sum(sus_by_model)
)
overall_totals

## --------------- DESCRIPTIVE TABLE 1 - OUTPUT --------------------------------------------------------------
#a.make sure table variables to include are in the order needed
df <- df %>%
  mutate(
    analysisdesign_simplified = factor(
      analysisdesign_simplified,
      levels = c("Case-control study","Cohort study","Cross-sectional study","Other/not specified")
    ),
    pub_year_cat = factor(pub_year_cat,
                          levels = c("2010–2014","2015–2019","2020–2024"),
                          ordered = TRUE),
    facility_type = factor(facility_type),
    highrisk = factor(highrisk,
                      levels = c("high risk","mixed or unspecified population")),
    studypop = factor(studypop,
                      levels = c("Neonates", "Pediatrics incl. neonates", "Pediatrics", 
                                 "Adults & pediatrics", "Adults","All ages or not specified",
                                 ))
  )


df <- df %>%
  mutate(
    bsi_source_nb = factor(bsi_source_nb, levels = c("Mixed", "Single")),
    infection_origin_nb = factor(infection_origin_nb, levels = c("Mixed", "Single"))
  )

#b.ensure BSI/Origin are logical TRUE/FALSE
df <- df %>%
  mutate(across(all_of(c(bsi_vars, origin_vars)), ~ as.logical(.)))

#c.build 3 sections: study characteriestics, BSI and infection source
# A- Study characteristics
tbl_char <- tbl_summary(
  data = df,
  include = c(analysisdesign_simplified, pub_year_cat, facility_type, highrisk, studypop),
  type = list(all_categorical() ~ "categorical"),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_categorical() ~ c(0, 1)),
  missing = "no",
  label = list(
    analysisdesign_simplified ~ "Study design",
    pub_year_cat ~ "Publication year",
    facility_type ~ "Facility type",
    highrisk ~ "Population risk groups",
    studypop ~ "Population age categories"
  )
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

#BSI suspected source (count TRUE only)
tbl_bsi_nb <- tbl_summary(
  data = df %>% filter(!is.na(bsi_source_nb)),
  include = bsi_source_nb,
  type = list(all_categorical() ~ "categorical"),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_categorical() ~ c(0, 1)),
  missing = "no",
  label = list(bsi_source_nb ~ "BSI source type")
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

#detail bsi sources rows (true only)
bsi_vars <- c("respiratory","urinary_tract","intra_abdominal_gastrointestinal",
              "skin_soft_tissue","device_related","primary","bone_joint",
              "surgical_site","unknown_unspecified_source","other_source")


tbl_bsi <- tbl_summary(
  data = df,
  include = all_of(bsi_vars),
  type = list(all_dichotomous() ~ "dichotomous"),
  value = list(all_dichotomous() ~ TRUE),
  statistic = list(all_dichotomous() ~ "{n} ({p}%)"),
  digits = list(all_dichotomous() ~ c(0, 1)),
  missing = "no",
  label = list(
    respiratory ~ "Respiratory",
    urinary_tract ~ "Urinary tract",
    intra_abdominal_gastrointestinal ~ "Intra-abdominal/gastrointestinal",
    skin_soft_tissue ~ "Skin-soft tissue",
    device_related ~ "Device-related",
    primary ~ "Primary",
    bone_joint ~ "Bone/joint",
    surgical_site ~ "Surgical site",
    unknown_unspecified_source ~ "Unknown/unspecified",
    other_source ~ "Other"
  )
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

#combine BSI type + details
tbl_bsi_section <- tbl_stack(list(tbl_bsi, tbl_bsi_nb))

# C- Infection origin (count TRUE only)
tbl_origin_nb <- tbl_summary(
  data = df %>% filter(!is.na(infection_origin_nb)),
  include = infection_origin_nb,
  type = list(all_categorical() ~ "categorical"),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_categorical() ~ c(0, 1)),
  missing = "no",
  label = list(infection_origin_nb ~ "Acquisition of infection type")
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

#detail origin rows (true onyl)
origin_vars <- c("hospital_acquired_infections","community_acquired_infections",
                 "healthcare_associated_infections","unknown_unspecified_origin",
                 "other_origin")

tbl_origin <- tbl_summary(
  data = df,
  include = all_of(origin_vars),
  type = list(all_dichotomous() ~ "dichotomous"),
  value = list(all_dichotomous() ~ TRUE),
  statistic = list(all_dichotomous() ~ "{n} ({p}%)"),
  digits = list(all_dichotomous() ~ c(0, 1)),
  missing = "no",
  label = list(
    hospital_acquired_infections ~ "Hospital-acquired",
    community_acquired_infections ~ "Community-acquired",
    healthcare_associated_infections ~ "Healthcare-associated",
    unknown_unspecified_origin ~ "Unknown/unspecified",
    other_origin ~ "Other"
  )
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

# combine origin type + details
tbl_origin_section <- tbl_stack(list(tbl_origin, tbl_origin_nb))

#e. model totals (sum across studies - created above)
tbl_tot <- tbl_summary(
  data = df,
  include = c(total_resistant, total_susceptible),
  type = list(all_continuous() ~ "continuous"),
  statistic = list(all_continuous() ~ "{sum}"),
  digits = list(all_continuous() ~ 0),
  missing = "no",
  label = list(
    total_resistant ~ "Total resistant population",
    total_susceptible ~ "Total susceptible population"
  )
) |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")


#f.put headers for the different sections
table_1 <- tbl_stack(
  tbls = list(tbl_char, tbl_bsi_section, tbl_origin_section, tbl_tot),
  group_header = c(
    "**Study characteristics — n (%)**",
    "**BSI suspected source — n (%)**",
    "**Acquisition of infection — n (%)**",
    "**Total population**"
  )
) |>
  bold_labels() |>
  modify_header(
    update = list(
      groupname_col ~ "",           # hide “Group” header label
      label ~ "**Characteristic**"
    )
  )

#style section header rows in flextable
library(flextable)
library(stringr)

ft <- as_flex_table(table_1)

# rows where the first column equals your group headers (markdown stripped)
section_rows <- which(str_remove_all(ft$body$dataset$label, "\\*") %in% c(
  "Study characteristics — n (%)",
  "BSI suspected source — n (%)",
  "Acquisition of infection — n (%)",
  "Total population"
))

# make them bold and left-aligned (column 'label' is the first/body label column)
ft <- bold(ft, i = section_rows, j = "label", bold = TRUE)
ft <- align(ft, i = section_rows, j = "label", align = "left")

# export
read_docx() |>
  body_add_flextable(ft) |>
  print(target = "study_characteristics_table1.docx")





# we need three databases for the planned analyses:
# 1) one with one row/observation per study, to summarize study characteristics -> df
# 2) one with a row per analysis (i.e, comparison AMR vs susceptible) done, to report on the associations measured -> df_long with one row per comparison (so-called 'model')
# 3) one with a row per variable of interest (indicator/predictor) reported, to summarize proxy indicators -> three different df_longer: one numeric, one categorical, one ?

# create df_long, transforming the dataframe df to a long format with a row for each comparison of R vs S (model_1, model_2 and model_3)
df$`resistant_group SD_2` <- as.character(df$`resistant_group SD_2`) # vars need to be in the same format to be combined
df$`resistant_group SD_3` <- as.character(df$`resistant_group SD_3`)
df$`Model_3 Resistant_group_tot_nb` <- as.character(df$`Model_3 Resistant_group_tot_nb`)
df$`Model_3 Susceptible_group_tot_nb` <- as.character(df$`Model_3 Susceptible_group_tot_nb`)

#convert to character - df_long identified different types and provided error
df <- df %>%
  mutate(across(starts_with("Model"), as.character))

df_long <- df %>%
  pivot_longer(
    cols = matches("^Model_[123]"),
    names_to = c("Model", ".value"),
    names_pattern = "Model_(\\d+) (.*)"
  )

# remove rows which don't contain a comparison -> 216 had model 1, another 24 had a model 2, another 5 had a model 3
df_long <- df_long %>% filter(!is.na(Resistant_grp_definition))

# clean resistance profiles & group resistance profiles in a variable amr
table(df_long$Resistant_grp_definition, useNA = "always")
# replace non specific 'resistance' by other variable values that have the specific resistance profile studied
df_long <- df_long %>%
  mutate(Resistant_grp_definition = case_when(
      Resistant_grp_definition %in% c("AMR", "Resistant", "Resistance +", "MRO", "MDRO", "Resistance to First-line Antibiotics") ~
        coalesce(AMR_mechanism_1, AMR_mechanism_2, Resistant_grp_definition),
      TRUE ~ Resistant_grp_definition))
# resist_models <- as.data.frame(table(df_long$Resistant_grp_definition, useNA = "always"))
# write.table(resist_models, "resist_models.txt")
df_long <- df_long %>%  mutate(amr = case_when(
      (grepl("MRSA", Resistant_grp_definition, ignore.case = TRUE)==T & grepl("ESBL|CRE|vancom|MDR", Resistant_grp_definition, ignore.case = TRUE)==F) ~ "methicillin resistance",
      (grepl("oxacillin|methicillin", Resistant_grp_definition, ignore.case = TRUE)==T & grepl("linezolid", Resistant_grp_definition, ignore.case = TRUE)==T) ~ "methicillin and linezolid resistance",
      (grepl("oxacillin|methicillin", Resistant_grp_definition, ignore.case = TRUE)==T & grepl("linezolid", Resistant_grp_definition, ignore.case = TRUE)==F) ~ "methicillin resistance",
      grepl("ESBL|blaAmpC producing|Ceph-R", Resistant_grp_definition, ignore.case = TRUE) ~ "3rd gen cephalosporin resistance",
      grepl("VRE", Resistant_grp_definition, ignore.case = TRUE) ~ "vancomycin resistance",
      grepl("\\bMDR\\b", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR",
      grepl("(MRO)", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR",
      grepl("MDRAB", Resistant_grp_definition, ignore.case = TRUE) ~ "MDR",
      grepl("MDRO", Resistant_grp_definition, ignore.case = TRUE) ~ "multiple AMR profiles combined",
      grepl("Carba", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("Resistance (+)", Resistant_grp_definition, ignore.case = TRUE) ~ "multiple AMR profiles combined",
      grepl("XDR", Resistant_grp_definition, ignore.case = TRUE) ~ "XDR",
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
      grepl("CR", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("CPE", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("Meropenem-nonsus|CnSKP", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("KPC", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("reduced susceptibility to cefuroxime and genta", Resistant_grp_definition, ignore.case = TRUE) ~ "cefuroxime + gentamycin resistance",
      grepl("CASR", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem + ampicillin/sulbactam resistance",
      grepl("resistant group", Resistant_grp_definition, ignore.case = TRUE) ~ "?",
      grepl("Ampicillin and gentamicin", Resistant_grp_definition, ignore.case = TRUE) ~ "ampicillin + gentamycin resistance",
      grepl("SNS", Resistant_grp_definition, ignore.case = TRUE) ~ "sulbactam non susceptible",
      grepl("LNZ", Resistant_grp_definition, ignore.case = TRUE) ~ "linezolid non susceptible/resistant",
      grepl("Ampicillin-resistant Enterococcus faecalis", Resistant_grp_definition, ignore.case = TRUE) ~ "ampicillin resistant",
      grepl("LIN-R", Resistant_grp_definition, ignore.case = TRUE) ~ "linezolid non susceptible/resistant",
      grepl("linezolid-resistant", Resistant_grp_definition, ignore.case = TRUE) ~ "linezolid non susceptible/resistant",
      grepl("imipenem-res", Resistant_grp_definition, ignore.case = TRUE) ~ "carbapenem resistance",
      grepl("HLRG", Resistant_grp_definition, ignore.case = TRUE) ~ "high-level gentamicin resistance",
      grepl("Drug resistant gram posit", Resistant_grp_definition, ignore.case = TRUE) ~ "not specified resistance",
      grepl("IRABC", Resistant_grp_definition, ignore.case = TRUE) ~"carbapenem resistance",
      grepl("Penicillin resistant Streptococcus pneumoniae", Resistant_grp_definition, ignore.case = TRUE) ~"penicillin resistance",
      grepl("Ampicillin resistant", Resistant_grp_definition, ignore.case = TRUE) ~"ampicillin resistance",
      TRUE ~ tolower(Resistant_grp_definition)))   # keep original for unique/rare categories
# some more manual verifications done (email Esmée 16/10/2025 with exact description of AMR profiles in papers)
df_long$amr[df_long$Resistant_grp_definition=="MDR gram negative catheter relates BSI (CRBSI)"] <- "carbapenem resistance"
df_long$amr[df_long$Study_ID=="#9394"] <- "carbapenem resistance"
df_long$amr[df_long$Study_ID=="#1574"] <- "carbapenem resistance"
df_long$amr[df_long$Study_ID=="#4088"] <- "3rd gen cephalosporin resistance"
df_long$amr[df_long$Study_ID=="#6260" & df_long$Resistant_grp_definition=="MDRAB"] <- "MDR"
df_long$amr[df_long$Study_ID %in% c("#8208", "#5509", "#4217", "#3114", "#2869", "#2480", "#2444", "#2122", "#1446", "#1412", "#1217", "#917",
                                    "#835", "#798", "#637", "#346")] <- "MDR"
df_long$amr[df_long$Study_ID %in% c("#3999", "#3992", "#3756", "#2771")] <- "MDR but incorrectly or more extensively defined"
df_long$amr[df_long$Study_ID %in% c("#3335", "#3189", "#2213", "#1984", "#1010", "#648", "#637", "#601", "#46")] <- "multiple AMR profiles combined"
df_long$amr[df_long$Study_ID %in% c("#9333", "#7504", "#4938")] <- "resistance against multiple Watch antibiotics"
df_long$amr[df_long$Study_ID=="#69"] <- "multiple AMR profiles combined"
df_long$amr[df_long$Resistant_grp_definition=="BSA-resistant (broad-spectrum antibiotic resistant) GNBSI"] <- "resistance against multiple Watch antibiotics"
df_long$amr[df_long$Study_ID=="#3959"] <- "multiple AMR profiles combined" # no specific R class, "resistant to the initial antibiotic treatment regimen [cefepime (n¼ 41; 50.0%), piperacillin-tazobactam (n¼ 25; 30.5%), or imipenem/meropenem (n¼ 16; 19.5%), plus either an aminoglycoside or ciprofloxacin (n¼ 28; 34.1%)]
df_long$amr[df_long$Study_ID=="#3771"] <- "multiple AMR profiles combined" # no specific R class, all major MDRO, and all but one (ampicillin/sulbactam R) are Watch antibiotics
df_long$amr[df_long$Study_ID=="#1995"] <- "resistance against multiple Watch antibiotics" # DTR, difficult to treat, all those mentioned are Watch
df_long$amr[df_long$Study_ID=="#1007"] <- "resistance against multiple Watch antibiotics" # Antibiotic-resistant microorganisms (ARM) included the following: Enterobacterales resistant to third-generation cephalosporins (3GCREB) or carbapenems (CRE), Pseudomonas aeruginosa and other non-fermenting Gram-negative rods (NFGNB), Enterococcus faecium and glycopeptide-resistant enterococci (GRE), methicillinresistant Staphylococcus aureus (MRSA) and Candida”
df_long$amr[df_long$Resistant_grp_definition=="PSBSI (persistent S. aureus in bloodstream infection)"] <- "methicillin resistance"
df_long$amr[df_long$Study_ID=="#1454"] <- "resistance against multiple Watch antibiotics" # "DTR was defined as nonsusceptibility (resistance or intermediate) to all tested agents in the carbapenem, β-lactam, and fluoroquinolone categories"
table(df_long$amr, useNA = "always")
df_long$amr[grepl("Resistance", df_long$Resistant_grp_definition)==T&df_long$Study_ID=="#3847"] <- "multiple AMR profiles combined"
df_long$amr[grepl("3GC-R", df_long$Resistant_grp_definition)==T&df_long$Study_ID=="#3847"] <- "3rd gen cephalosporin resistance"

# one study is entered twice as different AMR models, but are actually the same
df_long <- df_long %>% filter(Study_ID!="#2444"|Resistant_grp_definition!="MDR BSI")

# export an overview
amrgroups <- df_long %>% group_by(amr, Resistant_grp_definition) %>% summarise(n=n())
write_xlsx(amrgroups, "amrgroups.xlsx")

# summary of those with non specific resistance profiles reported
check_amr <- df_long %>% filter(grepl("\\bMDR\\b|(MRO)|MDRAB|MDRO|XDR|resistant group|Resistance (+)|Drug resistant gram positive", Resistant_grp_definition, ignore.case = TRUE)) %>% select(Study_ID, Resistant_grp_definition, amr)
write_xlsx(check_amr, "check_amr.xlsx")
check_amr2 <- df_long %>% filter(grepl("oxacillin|methicillin|linezolid", Resistant_grp_definition, ignore.case = TRUE)) %>% select(Study_ID, Resistant_grp_definition, amr)
check_amr3 <- df_long %>% filter(grepl("ARM|IRABC|Difficult to treat (DTR) - before propensity score (PS) matching|Difficult to treat (DTR) - after propensity score (PS) matching|Resistant BSI|Resistant gram-negatives|Resistant GNBSI|PSBSI (persistent S. aureus in bloodstream infection)|PDR", Resistant_grp_definition, ignore.case = TRUE)) %>% select(Study_ID, Resistant_grp_definition, amr, Title...3, Title...6)

# check columns
print(colnames(df_long), max = 1900)

# clean isolated bacteria, and group them if possible, creating a new variable 'pathogengroup'
df_long <- df_long %>% mutate(pathogen_antibiotic_combination = case_when(
      # 1) CR-Enterobacterales
      (amr == "carbapenem resistance" | amr == "carbapenem + ampicillin/sulbactam resistance") &
        grepl("Escherichia coli|E\\. coli|Klebsiella|Enterobacter|Salmonella", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "carbapenem-resistant Enterobacterales",  #KM - should be "carbapenem resistance instead of 3rd gen cephalosporin resistance
      # 2) C3GR-Enterobacterales (including ESBL)
      amr == "3rd gen cephalosporin resistance" &
        grepl("Escherichia coli|E\\. coli|Klebsiella|Enterobacter|Salmonella", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "C3G-resistant Enterobacterales",  #KM - ESBL not part of amr categories
      # 3) MRSA
      amr == "methicillin resistance" &
        grepl("Staphylococcus aureus|MRSA|MSSA", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "methicillin-resistant S. aureus",
      # 4) PRSP
      amr == "penicillin resistance" &
        grepl("Streptococcus pneumoniae|S\\. pneumoniae", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "penicillin-resistant S.pneumoniae",
      # 5) CRAB
      (amr == "carbapenem resistance"|amr == "carbapenem + ampicillin/sulbactam resistance")& # I now added the CASR amr profile too, since largely overlapping, but to CHECK
        grepl("Acinetobacter", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "carbapenem-resistant A.baumanii",
      # 6) CR-P. Aeruginosa
      amr == "carbapenem resistance" &
        grepl("Pseudomonas|P\\. aeruginosa|P\\.aeruginosa", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "carbapenem-resistant P.aeruginosa",
      # 7) VRE
      amr == "vancomycin resistance" &
      grepl("E.faecium|E. faecium|Enterococcus|Enterococci|VRE", `Bacterial-isolate_type`, ignore.case = TRUE) ~ "vancomycin-resistant Enterococci",
      # 8) Other (default)
      TRUE ~ "other/combination of multiple pathogens"))

table(df_long$pathogen_antibiotic_combination) # need to check the 152 Other if not any missed # KM - table does not show any C3GR-Enterobacterales or PRSP
checkbugdrug <- df_long %>% filter(pathogen_antibiotic_combination=="Other") %>% select(amr, Resistant_grp_definition, `Bacterial-isolate_type`, Study_ID)

# create an even longer df, with one row per variable/exposure of interest reported
df_long <- df_long %>% rename_with(~ str_replace_all(., "-(\\d+)_name", "_\\1")) # make sure all variable names belonging to the same indicator have the same number at the end
# reformat variables that should be combined
df_long <- df_long %>% mutate(across(contains("resistant_group mean_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group SD_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group p-value_"), as.character))
df_long <- df_long %>% mutate(across(contains("resistant_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group mean_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group SD_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group p-value_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("comparator_group SD_"), as.character))

# -----------------------DESCRIPTIVE_continuous_proxy-indicators ------------------------------------------------------------
# exclude columns beyond the first proxy-indicators part of the data extraction table (up to the notes of proxy indicator 20)
continuous_df <- df_long %>%
  select(1:393, studypop, highrisk, amr, pathogen_antibiotic_combination, Model, Resistant_grp_definition, Resistant_group_tot_nb,
         Susceptible_group_definition, Susceptible_group_tot_nb)

# reshape continuous_df to into a longer format, in which all variables ending with _1, _2, up to _20 (the numerical indicators) are brought together in the same variable, so that for each number there is one row
continuous_df <- continuous_df %>%  pivot_longer(
    cols = matches("_(?:[1-9]|1[0-9]|20)$"),   # matches _1 to _20
    names_to = c(".value", "set"),             # .value keeps base var names
    names_pattern = "(.*)_(\\d+)$")
colnames(continuous_df)

# get rid of empty rows (per study model, up to 20 indicators reported, but usually les, so many rows are empty)
continuous_df <- continuous_df %>% filter(!is.na(`resistant_group definition`))

#join continuous info - additional 6 studies
# #read continuous_ind sheet
# new_cont_info <- read.xlsx("additional_studies_extract_info.xlsx", sheet = "continuous_ind")
# #check column alignment
# setdiff(names(continuous_df), names(new_cont_info))
# setdiff(names(new_cont_info), names(continuous_df))
# #append
# continuous_df <- bind_rows(continuous_df, new_cont_info)
# #final check
# nrow(continuous_df)

# # check if for a study with multiple indicators reported, the definitions and values are correct (they first didn't because of a typo in a column name in the raw data)
# df %>% filter(Study_ID=="#12384") %>% select(`resistant_group mean_1`, `susceptible_comparator_group mean_1`)
# df %>% filter(Study_ID=="#12384") %>% select(`resistant_group mean_2`, `susceptible_comparator_group mean_2`)
# continuous_df %>% filter(Study_ID=="#12384") %>% select(`resistant_group variable`, `resistant_group mean`, `susceptible_comparator_group mean`)

#export list of ALL continuous proxy indicators keeping unique indicator names. Initial Export with ghost lines, remove space
write_xlsx(
  continuous_df %>%
    mutate(
      ind = as.character(`resistant_group variable`),
      ind = str_replace_all(ind, "\u00A0", " "),   # replace non-breaking spaces
      ind = str_replace_all(ind, "[\r\n\t]", " "), # drop CR/LF/tabs
      ind = str_squish(ind)                        # trim + collapse internal spaces
    ) %>%
    filter(!is.na(ind), ind != "") %>%
    arrange(ind) %>%
    select(`studypop`, `highrisk`, `amr`, `pathogen_antibiotic_combination`, `resistant_group variable` = ind, `Covidence #`, `Model`, `resistant_group definition`, `resistant_group mean`, `resistant_group median`, `Notes`, `General notes_AMR`, `General notes - if any...48`),
  "list_all_continuous_indicators.xlsx"
)

#export list of UNIQUE continuous proxy indicators keeping unique indicator names
continuous_df %>%
  mutate(
    ind = as.character(`resistant_group variable`),
    ind = str_replace_all(ind, "\u00A0", " "),   # replace non-breaking spaces
    ind = str_replace_all(ind, "[\r\n\t]", " "), # drop CR/LF/tabs
    ind = str_squish(ind)                        # trim + collapse internal spaces
  ) %>%
  select(`studypop`, `highrisk`, `amr`, `pathogen_antibiotic_combination`, `resistant_group variable` = ind, `Covidence #`, `Model`, `resistant_group definition`, `resistant_group mean`, `resistant_group median`, `Notes`, `General notes_AMR`, `General notes - if any...48`)  %>%
  filter(!is.na(`resistant_group variable`),
         `resistant_group variable` != "") %>%
  distinct() %>%
  arrange('resistant_group variable') %>%
  write_xlsx("list_unique_continuous_indicators.xlsx")

#correct study 2195 - resistant_group definition MRSA - showing empty for one indicator
continuous_df <- continuous_df %>%
  mutate(
    `resistant_group definition` = if_else(
      `Study_ID` == "#2195" &
        `resistant_group variable` == "Total WBCx103" &
        `resistant_group mean` == 12,
      "MRSA",
      `resistant_group definition`   # keep existing values otherwise
    )
  )

#verify change
continuous_df %>%
filter(`Study_ID` == "#2195") %>%
  select(`resistant_group variable`, `resistant_group mean`, `resistant_group definition`)

#correct study 3900 - resistant_group definition MDR - showing empty for one indicator
continuous_df <- continuous_df %>%
  mutate(
    `resistant_group definition` = if_else(
      `Study_ID` == "#3900" &
        `resistant_group variable` == "total length of stay (days)" &
        `resistant_group median` == 20,
      "MDR",
      `resistant_group definition`   # keep existing values otherwise
    )
  )

#correct study 4385 p-value for Fibrinogen
continuous_df <- continuous_df %>%
  mutate(
    `resistant_group p-value` = if_else(
      `Study_ID` == "#4385" &
        `resistant_group variable` == "Liver and kidney function - Fibrinogen (g/L)",
        "<0.001",
      `resistant_group p-value`   # keep existing values otherwise
    )
  )

#verify change
continuous_df %>%
  filter(`Study_ID` == "#4385") %>%
  select(`resistant_group variable`, `resistant_group p-value`)

#KM - continuous indicators list checked manually in excel and categories created - with differences from the below initial categorization
#use the excel and join to the original continuous_df dataframe
#When need to change based on input from others, apply global renames for mass renames of catergories +/- targeted override for small tweaks with mutate - so no need to change subsequent code

#point mapping inside repo so anyone can access it
cont_map_path <- here::here(
  "db", "indicators_mapping", "map_all_continuous_indicators_categorized_initial.xlsx"
  )

stopifnot(file.exists(cont_map_path)) #ensure file exists

#a.clean the list
clean_text <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\u00A0", " ") |>
    str_replace_all("[\r\n\t]", " ") |>
    str_squish() |>
    str_to_lower()
}

#b.read mapping into R and check key columns needed to join are there : covidence #, resistant_group variable, resistant_group definition, indicator category level1, indicator cat and the value of the mean or median, whichever exists
cont_map_df <- read_excel(cont_map_path)

stopifnot(all(c(
              "Covidence #", "resistant_group variable", "resistant_group definition",
              "cont_indicatorcategory_l1", "cont_indicatorcategory_l2", "cont_indicatorcategory_l3",
              "cont_indicatorcategory_l4","cont_indicatorcategory_l5_unit","cont_indicatorcategory_l6_Ab-type",
              "resistant_group mean", "resistant_group median") %in% names(cont_map_df)))

#c.create value keys for the matching variables and clean them on both sides - needed to clean variables ie lower cases, spaces..., to join on median or mean value whichver exists and prevent row multiplication - many in the database
cont_map_keys <- cont_map_df %>%
  mutate(
    cov_clean = clean_text(`Covidence #`),
    res_var_clean = clean_text(`resistant_group variable`),
    res_def_clean = clean_text(`resistant_group definition`),
    #create the key to match for value of mean or median depending on which comes first
    mean_median_key = coalesce(
      as.character(`resistant_group mean`),
      as.character(`resistant_group median`))
    |> str_replace_all("\u00A0", "") |> str_squish(),
    l1 = na_if(str_squish(cont_indicatorcategory_l1), ''),
    l2 = na_if(str_squish(cont_indicatorcategory_l2), ''),
    l3 = na_if(str_squish(cont_indicatorcategory_l3), ""),
    l4 = na_if(str_squish(cont_indicatorcategory_l4), ""),
    l5 = na_if(str_squish(cont_indicatorcategory_l5_unit), ""),
    l6 = na_if(str_squish(`cont_indicatorcategory_l6_Ab-type`),"")
)

#d.collapse the mapping to one row per (cov_clean, res_var_clean, res_def_clean, mean-median_key)
#set duplicates to disagree as NA so they can be viewed and dealt with. variables that have con_indicatorcategory_l1 and l2 empty coz need to be removed from here will be NA
cont_map_collapse <- cont_map_keys %>%
  group_by(cov_clean, res_var_clean, res_def_clean, mean_median_key) %>%
  summarize(
    n_rows = n(),
    n_l1 = n_distinct(na.omit(l1)),
    n_l2 = n_distinct(na.omit(l2)),
    n_l3 = n_distinct(na.omit(l3)),
    n_l4 = n_distinct(na.omit(l4)),
    n_l5 = n_distinct(na.omit(l5)),
    n_l6 = n_distinct(na.omit(l6)),
    cont_indicatorcategory_l1 = if (n_l1 <= 1) first(na.omit(l1)) else NA_character_,
    cont_indicatorcategory_l2 = if (n_l2 <= 1) first(na.omit(l2)) else NA_character_,
    cont_indicatorcategory_l3        = if (n_l3 <= 1) first(na.omit(l3)) else NA_character_,
    cont_indicatorcategory_l4        = if (n_l4 <= 1) first(na.omit(l4)) else NA_character_,
    cont_indicatorcategory_l5_unit   = if (n_l5 <= 1) first(na.omit(l5)) else NA_character_,
    cont_indicatorcategory_l6_Ab_type= if (n_l6 <= 1) first(na.omit(l6)) else NA_character_,
    .groups = "drop"
  )

#view conflicts
conflicts <- cont_map_collapse %>%  filter(n_l1 > 1 | n_l2 > 1)
if (nrow(conflicts) > 0) {
  warning(sprintf("Mapping has %d conflicting 4-keys (same key --> multiple categories - fix in excel later", nrow(conflicts)))
}

#e.build the same 4 matching keys in continuous_df
continuous_df_keys <- continuous_df %>%
  select(-starts_with("cont_indicatorcategory_")) %>%
  mutate(
    cov_clean = clean_text(`Covidence #`),
    res_var_clean = clean_text(`resistant_group variable`),
    res_def_clean = clean_text(`resistant_group definition`),
    mean_median_key = coalesce(
      as.character(`resistant_group mean`),
      as.character(`resistant_group median`))
    |> str_replace_all("\u00A0", "") |> str_squish()
  )

n_before <- nrow(continuous_df_keys)

# f.JOIN and CLEAN ONCE:
#inner_join - keeping only keys present in the mapping file (should give around 536 if all mapped)
#distinct on the 4-key - to guarantee 1 row per key after the join (deduplicate)
continuous_df <- continuous_df_keys %>%
  inner_join(
    cont_map_collapse %>%
      select(cov_clean, res_var_clean, res_def_clean, mean_median_key,
             cont_indicatorcategory_l1, cont_indicatorcategory_l2, cont_indicatorcategory_l3,
             cont_indicatorcategory_l4, cont_indicatorcategory_l5_unit, cont_indicatorcategory_l6_Ab_type),
    by = c("cov_clean", "res_var_clean", "res_def_clean", "mean_median_key")
  ) %>%
  distinct(cov_clean, res_var_clean, res_def_clean, mean_median_key, .keep_all = TRUE) %>%
  select(-cov_clean, -res_var_clean, -res_def_clean, -mean_median_key)

n_after <- nrow(continuous_df)
cat("Rows before join:", n_before, " | rows after inner join + 4-key distinct:", n_after, "\n")

#g.quality coantrol - prove there is no 4-key duplication after join (should be 0)
joined_keys <- continuous_df %>%
  mutate(
    cov_clean     = clean_text(`Covidence #`),
    res_var_clean = clean_text(`resistant_group variable`),
    res_def_clean = clean_text(`resistant_group definition`),
    mean_median_key = coalesce(as.character(`resistant_group mean`),
                               as.character(`resistant_group median`)) |>
      str_replace_all("\u00A0","") |> str_squish()
  )

dup_check <- joined_keys %>%
  count(cov_clean, res_var_clean, res_def_clean, mean_median_key, name = "n_per_key") %>%
  filter(n_per_key > 1)

cat("4-key duplicates after join:", nrow(dup_check), "\n")  # expect 0 which it is

#check a quick sample to see if it looks right
set.seed(1)
continuous_df %>%
  select(`Covidence #`, `resistant_group variable`, `resistant_group definition`,
         `resistant_group mean`, `resistant_group median`,
         cont_indicatorcategory_l1, cont_indicatorcategory_l2, cont_indicatorcategory_l3, cont_indicatorcategory_l4,
         cont_indicatorcategory_l5_unit, cont_indicatorcategory_l6_Ab_type) %>%
  sample_n(min(10, n()))

#check if any row has resistant_group variable that is empty/NA
any(is.na(continuous_df$`resistant_group variable`) | continuous_df$`resistant_group variable` == "")

#check the missing on L1 and L2 (kept empty because categorical or no p-value - non-sign exclude from analysis)
#others with no p-values still need to be identified from continuous_df and not ocunted in the final analysis
cont_missing_l1 <- continuous_df %>%
  filter(is.na(cont_indicatorcategory_l1) | cont_indicatorcategory_l1 == "") %>%
  select(`Covidence #`, `resistant_group variable`, `resistant_group definition`,
         `resistant_group mean`, `resistant_group median`, `cont_indicatorcategory_l1`)
#34 - validated the list - all OK

#remove those 34 from the analysis list (continuous_df)
continuous_df <- continuous_df %>%
  filter(!is.na(cont_indicatorcategory_l1) & trimws(cont_indicatorcategory_l1) != "")

#removing of duplicates was done without taking the difference on the AMR gruping into account (duplicates matched on main variables but grouping/model)
#verified against the source

#--------------------Prepare continuous list for calculation------------------------------------------------------------------
##starting from 500 observation (deduplicate + indicator_l1 empty because categorica/no-pvalue - not full list)
##indicators with no p-value on their mean or median and only keep sign differences

#1.clean p-values and keep <0.05 (remove empty and >0.05 we have 2 , but check them before remove)

#a.clean p-values to proper numerics & detect leading '<'
p_txt <- as.character(continuous_df$`resistant_group p-value`)
p_txt <- trimws(p_txt)
p_txt <- gsub(",", ".", p_txt, fixed = TRUE)     # decimal comma -> dot
p_txt <- gsub("\u00B7", ".", p_txt, fixed = TRUE) # middle dot -> dot (e.g., "0·001")

lt_flag      <- grepl("^\\s*<", p_txt)           # has leading "<"
lt_zero_flag <- grepl("^\\s*<\\s*0", p_txt)      # explicitly like "<0.05", "<0.001"

#strip any leading comparator for numeric parsing
p_for_num <- sub("^\\s*[<>=]\\s*", "", p_txt)

#take first numeric/scientific token; text like "ns" becomes NA
p_num <- suppressWarnings(as.numeric(
  sub("^.*?([0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?).*$", "\\1", p_for_num)
))

#attach cleaned p for audit; keep original text in place
continuous_df$p_clean <- p_num
continuous_df$p_had_lt <- lt_flag

#b.define "keep significant" and "remove NS or empty"
#keep if p_num < 0.05
#also keep if it was written as "<0.05" (lt_flag==TRUE) and p_num <= 0.05
alpha <- 0.05
keep_sig <- (!is.na(p_num) & p_num < alpha) |       # numeric < 0.05
  (lt_zero_flag & (is.na(p_num) | p_num <= alpha))  # written as "<0.xxx" -> keep

#c.split keep/remove and finalize continuous_df to allow for verification
pvalue_keep <- continuous_df[keep_sig, ]

pvalue_remove <- continuous_df[!keep_sig, ] %>%
  select(
    `Covidence #`, Study_ID,
    cont_indicatorcategory_l1, cont_indicatorcategory_l2,
    `resistant_group variable`, `resistant_group definition`,
    `resistant_group mean`, `resistant_group median`,
    `susceptible_comparator_group mean`, `susceptible_comparator_group median`,
    `resistant_group p-value`,  # original text
    p_clean, p_had_lt           # cleaned numeric + "<" flag
  ) %>%
 arrange(`Covidence #`, `resistant_group variable`)

#d.set working data to rows to keep
continuous_df <- pvalue_keep %>%
  mutate(`resistant_group p-value` = p_clean) %>%
  select(-p_clean, -p_had_lt)

#e.quick checks
cat("Kept (significant):", nrow(continuous_df), "\n")
cat("Removed (NA or >= 0.05):", nrow(pvalue_remove), "\n")

View(pvalue_remove) #all verified - all good

##check if any of the indicators have both mean and median
##both_mean_median <- continuous_df %>%
  ##filter(!is.na(`resistant_group mean`) &
          ##!is.na(`resistant_group median`))
##nrow(both_mean_median)
##View(both_mean_median)
#10 if them - when computing average consider them once - on the mean
#when both exist use one value
#continuous_df <- continuous_df %>%
#mutate(value_for_calc = coalesce(`resistant_group mean`, `resistant_group median`))

#2.check where resistant_group variable mismatch its susceptible comparator (many seen with eyeball)
#a.find the mismatches
mismatch_rs <- continuous_df %>%
  mutate(
    r_var = str_squish(as.character(`resistant_group variable`)),
    s_var = str_squish(as.character(`susceptible_comparator_group variable`))
  ) %>%
  filter(
    is.na(s_var) | s_var == "" |            # missing susceptible comparator
      str_to_lower(r_var) != str_to_lower(s_var)   # text differs
  ) %>%
  arrange(`Covidence #`, r_var) %>%
  select(`Covidence #`, Study_ID,
         `resistant_group variable`, `susceptible_comparator_group variable`,
         `resistant_group mean`, `resistant_group median`,
         everything())

#View(mismatch_rs)
#nrow(mismatch_rs)

#b.few minor mismatches ie "Age, year versus Age, years (with s), or cost IND vs cost INR
#in this list of mismatches, replace the text of the susceptible with the text of the continuous
#overwrite susceptible comparator when it differs from resistant text - after I validated all
continuous_df <- continuous_df %>%
  mutate(
    `susceptible_comparator_group variable` = if_else(
      is.na(`susceptible_comparator_group variable`) |
        str_squish(`susceptible_comparator_group variable`) == "" |
        str_to_lower(str_squish(`susceptible_comparator_group variable`)) !=
        str_to_lower(str_squish(`resistant_group variable`)),
      `resistant_group variable`,
      `susceptible_comparator_group variable`
    )
  )

#c.re-check mismatches (should now be 0)
mismatch_rs_after <- continuous_df %>%
  mutate(
    r_var = str_squish(as.character(`resistant_group variable`)),
    s_var = str_squish(as.character(`susceptible_comparator_group variable`))
  ) %>%
  filter(is.na(s_var) | s_var == "" | str_to_lower(r_var) != str_to_lower(s_var))

nrow(mismatch_rs_after)  # ideally 0 - it is zero

# -------------------SUMMARY CONTINUOS-----------------------------------------------------------
# -------------summarize the data to share with the team ----------------------------------------
#a.drop rows where indicator_l1 and l2 are empty
summary_cont_indicators <- continuous_df %>%
  filter(!is.na(cont_indicatorcategory_l1)) %>%
  mutate(
    # tidy the original indicator a bit for nicer lists
    continuous_indicator_clean = str_squish(as.character(`resistant_group variable`))
  )

#b.frequency per l1
freq_continuous_l1 <- summary_cont_indicators %>%
  group_by(cont_indicatorcategory_l1) %>%
  summarise(freq_continuous_l1 = n(), .groups = "drop")

#c.summary per (l1, l2) pair - indicators list + l2 frequency
collapse_list_continuous <- function(x) {
  #unique, sorted, comma-separated string
  paste(sort(unique(x[!is.na(x) & x != ""])), collapse = ", ")
}

pairs_l1_l2 <- summary_cont_indicators %>%
  group_by(cont_indicatorcategory_l1, cont_indicatorcategory_l2) %>%
  summarise(
    indicators_included = collapse_list_continuous(continuous_indicator_clean),
    frequency_category_l2 = n(),
    .groups = "drop"
  )

#d.attach the l1 frequency to each (l1,l2) row
summary_cont_indicators_out <- pairs_l1_l2 %>%
  left_join(freq_continuous_l1, by = "cont_indicatorcategory_l1") %>%
  arrange(cont_indicatorcategory_l1, desc(frequency_category_l2))

#e.view and export
print(summary_cont_indicators_out, n = 20)
write_xlsx(
  list(summary_cont_indicators = summary_cont_indicators_out),
  "continuous_indicators_summary.xlsx"
)

#f.bar chart for level_1
freq_continuous_l1 <- freq_continuous_l1 %>%
  arrange(desc(freq_continuous_l1)) %>%
  mutate(cont_indicatorcategory_l1 =
           factor(cont_indicatorcategory_l1,
                  levels = cont_indicatorcategory_l1))

p_l1 <- ggplot(freq_continuous_l1,
               aes(x = reorder(cont_indicatorcategory_l1, freq_continuous_l1),  # asc order
                   y = freq_continuous_l1)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(x = "Level 1 category",
       y = "Count",
       title = "Continuous-indicators_bar-chart_categories (first level_Level1)") +
  theme_minimal(base_size = 12)

print(p_l1)
ggsave(filename = "continuous_indicators_bar-chart_level1.jpeg", p_l1, width = 9, height = 6, dpi = 300) #export

#g.heatmap for the top 20 categories - otherwise it does not show well
top_L2_n <- 20

top_l2 <- summary_cont_indicators %>%
  filter(!is.na(cont_indicatorcategory_l2)) %>%
  count(cont_indicatorcategory_l2, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_L2_n) %>%
  pull(cont_indicatorcategory_l2)

heat_df <- summary_cont_indicators %>%
  mutate(L1 = cont_indicatorcategory_l1,
         L2 = ifelse(is.na(cont_indicatorcategory_l2),
                     "(Missing L2)",
                     ifelse(cont_indicatorcategory_l2 %in% top_l2,
                            cont_indicatorcategory_l2,
                            "Other (small)"))) %>%
  count(L1, L2, name = "count")

#order L1 by total count and L2 by overall count
l1_order <- heat_df %>%
  group_by(L1) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(L1)

l2_order <- heat_df %>%
  group_by(L2) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(L2)

heat_df <- heat_df %>%
  mutate(L1 = factor(L1, levels = l1_order),
         L2 = factor(L2, levels = l2_order))

p_heat <- ggplot(heat_df, aes(x = L2, y = L1, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), size = 3) +
  scale_fill_gradient(low = "#f0f0f0", high = "#08519c") +
  labs(x = "Level 2 (top & grouped)", y = "Level 1",
       fill = "Count",
       title = paste0("Counts by Level 1 × Level 2 (top ", top_L2_n, " L2 shown)")) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat)
ggsave(filename = "continuous_indicators_heatmap_categories_l1_l2.jpeg", p_heat, width = 12, height = 7, dpi = 300) #export

#h.interactive review table
# Use already built 'summary_cont_indicators_out' (L1,L2, indicators list, and freq)
datatable(
  summary_cont_indicators_out,
  options = list(
    pageLength = 25,
    order = list(list(1, "asc"), list(4, "desc")),
    autoWidth = TRUE
  ),
  rownames = FALSE,
  caption = "Continuous_indicators_categories_Level 1 × Level 2_details-and-frequencies"
)

# ------------- PREPARE CONTINUOUS FOR CALCULATION OF OVERALL mean and range of median ---------------------------------
#numeric mirror of resistance and susceptible - create analysis ones
#strip $, all commas (US + Indian), spaces
#accept integer/decimal/sci-notation; otherwise NA
num_clean <- function(x) {
  s <- gsub("\\$", "", as.character(x))
  s <- gsub(",",  "", s)                # removes US + Indian thousands commas
  s <- gsub("[[:space:]]", "", s)
  s <- str_squish(s)
  ok <- grepl("^[-+]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$", s)
  out <- rep(NA_real_, length(s))
  out[ok] <- suppressWarnings(as.numeric(s[ok]))
  out
}

#last numeric in a cell (for INR/USD: keeps USD 'y' in 'x/y') -
#one case where unit is INR/USD rows and value enteres as x/y where x = INR and y=USD, keep USD and set unit (l5) to USD
last_num_clean <- function(x) {
  s <- gsub("\\$", "", as.character(x))
  s <- gsub(",",  "", s)
  s <- gsub("[[:space:]]", "", s)
  toks <- str_extract_all(s, "[-+]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?")
  vapply(toks, function(v) if (length(v)) suppressWarnings(as.numeric(v[length(v)])) else NA_real_, 1.0)
}

#better display in View() - remove xxx decimals
options(scipen = 999, digits = 6)

#create a mirror of resistant_gtoup mean and median and susceptible_aomparator_group mean and median - conversion will be done there
#R_median, R_mean, S_median, S_mean will be the final used for analysis

continuous_df <- continuous_df %>%
  mutate(
    R_mean   = num_clean(`resistant_group mean`),
    R_median = num_clean(`resistant_group median`),
    S_mean   = num_clean(`susceptible_comparator_group mean`),
    S_median = num_clean(`susceptible_comparator_group median`),

#create unit with all conversion for analysis
unit_for_calc = as.character(`cont_indicatorcategory_l5_unit`)
)

#a.ECONOMIC indicators
#for cost related indicators, convert all to USD (majority)
#include the INR/USD case

#normailize unit and year
unit_low <- tolower(str_squish(as.character(continuous_df$`cont_indicatorcategory_l5_unit`)))
puby     <- suppressWarnings(as.integer(continuous_df$Publication_year))
l1_lc    <- tolower(str_squish(as.character(continuous_df$`cont_indicatorcategory_l1`)))
econ     <- grepl("economic", l1_lc) & grepl("resource", l1_lc) & grepl("continuous", l1_lc)
usd      <- unit_low %in% c("usd", "$")
inr_usd  <- econ & grepl("inr", unit_low) & grepl("usd", unit_low)

# per-row fix rate (only for NON-USD in scope - INR/USD -> 1 since we took USD side)
# rates matching the averages of the conversion to the year of the study
fx_usd <- case_when(
  inr_usd ~ 1.00,
  econ & !usd & unit_low %in% c("euro","eur","€")  & puby == 2019 ~ 1.1190,
  econ & !usd & unit_low %in% c("euro","eur","€")  & puby == 2023 ~ 1.0824,
  econ & !usd & unit_low %in% c("cny","rmb","renminbi") & puby == 2023 ~ 0.1410,
  econ & !usd & unit_low %in% c("cny","rmb","renminbi") & puby == 2024 ~ 0.1400,
  econ & !usd & unit_low %in% c("aud","au dollar") ~ 0.9675,
  econ & !usd & unit_low %in% c("jpy")            ~ 0.0091,
  econ & !usd & unit_low %in% c("sgd")            ~ 0.73,
  econ & !usd & unit_low %in% c("can")            ~ 0.7832,
  TRUE ~ NA_real_
)

#apply conversion only to economic rows
econ_idx <- which(econ & (inr_usd | (!usd & !is.na(fx_usd))))
if (length(econ_idx)) {
  rmean_base   <- ifelse(inr_usd[econ_idx],
                         last_num_clean(continuous_df$`resistant_group mean`  [econ_idx]),
                         num_clean     (continuous_df$`resistant_group mean`  [econ_idx]))
  rmedian_base <- ifelse(inr_usd[econ_idx],
                         last_num_clean(continuous_df$`resistant_group median`[econ_idx]),
                         num_clean     (continuous_df$`resistant_group median`[econ_idx]))
  smean_base   <- ifelse(inr_usd[econ_idx],
                         last_num_clean(continuous_df$`susceptible_comparator_group mean`  [econ_idx]),
                         num_clean     (continuous_df$`susceptible_comparator_group mean`  [econ_idx]))
  smedian_base <- ifelse(inr_usd[econ_idx],
                         last_num_clean(continuous_df$`susceptible_comparator_group median`[econ_idx]),
                         num_clean     (continuous_df$`susceptible_comparator_group median`[econ_idx]))

  # factor: INR/USD -> 1; else fx_usd (already computed)
  fx_vec <- ifelse(inr_usd[econ_idx], 1, fx_usd[econ_idx])

  # write to analysis columns (raw untouched)
  continuous_df$R_mean  [econ_idx] <- rmean_base   * fx_vec
  continuous_df$R_median[econ_idx] <- rmedian_base * fx_vec
  continuous_df$S_mean  [econ_idx] <- smean_base   * fx_vec
  continuous_df$S_median[econ_idx] <- smedian_base * fx_vec
}

#set analysis unit to USD for all economic indicators - alreasy USD or converted
set_usd <- econ & (usd | inr_usd | (!usd & !is.na(fx_usd)))
continuous_df$unit_for_calc[set_usd] <- "USD"

## quick spot check rows that still didn't parse to numeric
#still_na <- econ_idx[
  #is.na(continuous_df$R_mean[econ_idx]) &
    #(!is.na(continuous_df$`resistant_group mean`[econ_idx]))
#]
#if (length(still_na)) {
  #cat("ECON rows with non-numeric cells after cleaning:", length(still_na), "\n")
  #print(head(data.frame(
    #cov = continuous_df$`Covidence #`[still_na],
    #unit = continuous_df$`cont_indicatorcategory_l5_unit`[still_na],
    #raw_mean = continuous_df$`resistant_group mean`[still_na]
  #), 10))
#}

#b.BIOMARKERS - apply the conversion of the biomarkers (and others) to have them in the same units to calculate overall mean/median
#needed for Hb, creatinine, akbumin, bilirubin and survival time

#Hb - change g/dL to g/L
idx_hb <- with(continuous_df,
               cont_indicatorcategory_l3 == "Hb_value" &
                 tolower(ifelse(is.na(`cont_indicatorcategory_l5_unit`), "", `cont_indicatorcategory_l5_unit`)) == "g/dl"
)
ihb <- which(!is.na(idx_hb) & idx_hb)
continuous_df$R_mean  [ihb] <- num_clean(continuous_df$`resistant_group mean`  [ihb]) * 10
continuous_df$R_median[ihb] <- num_clean(continuous_df$`resistant_group median`[ihb]) * 10
continuous_df$S_mean  [ihb] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [ihb]) * 10
continuous_df$S_median[ihb] <- num_clean(continuous_df$`susceptible_comparator_group median`[ihb]) * 10
continuous_df$unit_for_calc[ihb] <- "g/L"


#creatinine - creatinine_value & µmol/L / convert umol/L -> mg/dL (by ÷ 88.4)
unit_lc <- tolower(ifelse(is.na(continuous_df$`cont_indicatorcategory_l5_unit`), "", continuous_df$`cont_indicatorcategory_l5_unit`))
unit_lc <- gsub("µ","u", unit_lc)
idx_cr <- with(continuous_df,
               cont_indicatorcategory_l3 == "creatinine_value" & unit_lc %in% c("umol/l","µmol/l")
)
icr <- which(!is.na(idx_cr) & idx_cr)
continuous_df$R_mean  [icr] <- num_clean(continuous_df$`resistant_group mean`  [icr]) / 88.4
continuous_df$R_median[icr] <- num_clean(continuous_df$`resistant_group median`[icr]) / 88.4
continuous_df$S_mean  [icr] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [icr]) / 88.4
continuous_df$S_median[icr] <- num_clean(continuous_df$`susceptible_comparator_group median`[icr]) / 88.4
continuous_df$unit_for_calc[icr] <- "mg/dL"


#albumin - albumin_value & g/dL -> convert to g/L (×10)
idx_alb <- with(continuous_df,
                cont_indicatorcategory_l3 == "albumin_value" &
                  tolower(ifelse(is.na(`cont_indicatorcategory_l5_unit`), "", `cont_indicatorcategory_l5_unit`)) == "g/dl"
)
ialb <- which(!is.na(idx_alb) & idx_alb)
continuous_df$R_mean  [ialb] <- num_clean(continuous_df$`resistant_group mean`  [ialb]) * 10
continuous_df$R_median[ialb] <- num_clean(continuous_df$`resistant_group median`[ialb]) * 10
continuous_df$S_mean  [ialb] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [ialb]) * 10
continuous_df$S_median[ialb] <- num_clean(continuous_df$`susceptible_comparator_group median`[ialb]) * 10
continuous_df$unit_for_calc[ialb] <- "g/L"


#bilirubin - bilirubin_value & mg/dL -> mmol/L (× 0.017104)
idx_bil <- with(continuous_df,
                cont_indicatorcategory_l3 == "bilirubin_value" &
                  tolower(ifelse(is.na(`cont_indicatorcategory_l5_unit`), "", `cont_indicatorcategory_l5_unit`)) == "mg/dl"
)
ibil <- which(!is.na(idx_bil) & idx_bil)
continuous_df$R_mean  [ibil] <- num_clean(continuous_df$`resistant_group mean`  [ibil]) * 0.017104
continuous_df$R_median[ibil] <- num_clean(continuous_df$`resistant_group median`[ibil]) * 0.017104
continuous_df$S_mean  [ibil] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [ibil]) * 0.017104
continuous_df$S_median[ibil] <- num_clean(continuous_df$`susceptible_comparator_group median`[ibil]) * 0.017104
continuous_df$unit_for_calc[ibil] <- "mmol/L"

#c.SURVIVAL TIME - survival_time & month(s) -> days (× 30)
days_per_month <- 30
idx_surv <- with(continuous_df,
                 cont_indicatorcategory_l2 == "survival_time" &
                   tolower(ifelse(is.na(`cont_indicatorcategory_l5_unit`), "", `cont_indicatorcategory_l5_unit`)) %in% c("month","months")
)
isurv <- which(!is.na(idx_surv) & idx_surv)
continuous_df$R_mean  [isurv] <- num_clean(continuous_df$`resistant_group mean`  [isurv]) * days_per_month
continuous_df$R_median[isurv] <- num_clean(continuous_df$`resistant_group median`[isurv]) * days_per_month
continuous_df$S_mean  [isurv] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [isurv]) * days_per_month
continuous_df$S_median[isurv] <- num_clean(continuous_df$`susceptible_comparator_group median`[isurv]) * days_per_month
continuous_df$unit_for_calc[isurv] <- "days"

#d.APPROPRIATE THERAPY time-to to same unit all (days). I have few in hours and minutes
#define rows to convert (target indicator + unit in hours/minutes)

#NA-safe lowercased unit
unit_lc2 <- tolower(ifelse(is.na(continuous_df$`cont_indicatorcategory_l5_unit`), "",
                           as.character(continuous_df$`cont_indicatorcategory_l5_unit`)))
idx_app <- with(continuous_df,
                `cont_indicatorcategory_l2` == "Appropriate_therapy_time-to" &
                  unit_lc2 %in% c("hour","hours","minute","minutes")
)
i_app <- which(!is.na(idx_app) & idx_app)
if (length(i_app)) {
  f_app <- ifelse(unit_lc2[i_app] %in% c("hour","hours"), 1/24, 1/1440)
  continuous_df$R_mean  [i_app] <- num_clean(continuous_df$`resistant_group mean`  [i_app]) * f_app
  continuous_df$R_median[i_app] <- num_clean(continuous_df$`resistant_group median`[i_app]) * f_app
  continuous_df$S_mean  [i_app] <- num_clean(continuous_df$`susceptible_comparator_group mean`  [i_app]) * f_app
  continuous_df$S_median[i_app] <- num_clean(continuous_df$`susceptible_comparator_group median`[i_app]) * f_app
  continuous_df$unit_for_calc[i_app] <- "days"
}


#audit - quality control on all conversions to verify them
  mismatches <- continuous_df %>%
    mutate(
      raw_R_mean   = num_clean(`resistant_group mean`),
      raw_R_median = num_clean(`resistant_group median`),
      raw_S_mean   = num_clean(`susceptible_comparator_group mean`),
      raw_S_median = num_clean(`susceptible_comparator_group median`)
    ) %>%
    filter(
      (is.na(R_mean)   != is.na(raw_R_mean))   | (!is.na(R_mean)   & !is.na(raw_R_mean)   & R_mean   != raw_R_mean)   |
        (is.na(R_median) != is.na(raw_R_median)) | (!is.na(R_median) & !is.na(raw_R_median) & R_median != raw_R_median) |
        (is.na(S_mean)   != is.na(raw_S_mean))   | (!is.na(S_mean)   & !is.na(raw_S_mean)   & S_mean   != raw_S_mean)   |
        (is.na(S_median) != is.na(raw_S_median)) | (!is.na(S_median) & !is.na(raw_S_median) & S_median != raw_S_median)
    ) %>%
    select(`Covidence #`, cont_indicatorcategory_l1,
           `resistant_group mean`,   R_mean,
           `resistant_group median`, R_median,
           `susceptible_comparator_group mean`,   S_mean,
           `susceptible_comparator_group median`, S_median)

  View(mismatches)
  nrow(mismatches)

inspect <- continuous_df %>%
  select(`resistant_group mean`, R_mean, `resistant_group median`, R_median,
         `susceptible_comparator_group mean`,  S_mean, `susceptible_comparator_group median`, S_median)
View(inspect)

#e.AGE - makes sure split between age_adults and age_peds based on the studypop - might not be needed
continuous_df <- continuous_df %>%
  mutate(
    cont_indicatorcategory_l3 = case_when(
      #Age for adults
      cont_indicatorcategory_l1 == "Demographics_continuous" &
      cont_indicatorcategory_l2 == "Age" &
      studypop == "Adults" ~ "Age_adults",

      #Age where population is unspecified
      cont_indicatorcategory_l1 == "Demographics_continuous" &
      cont_indicatorcategory_l2 == "Age" &
      studypop == "All ages or not specified" ~ "Age_unspecified",

      #Age for pediatrics and/or neonates (only one study with neonates, few others ped incl neonates)
      cont_indicatorcategory_l1 == "Demographics_continuous" &
      cont_indicatorcategory_l2 == "Age" &
      studypop %in% c("Neonates", "Pediatrics incl. neonates", "Pediatrics") ~ "Age_pediatrics_neonates",

      # leave others (including non-Age rows) unchanged
      studypop == "Adults & Pediatrics" ~ "Age_adults_pediatrics",
      TRUE ~ cont_indicatorcategory_l3)
)

      #check
      age_rows <- continuous_df %>%
        filter(cont_indicatorcategory_l1 == "Demographics_continuous",
               cont_indicatorcategory_l2 == "Age")

      table(age_rows$studypop, useNA = "ifany")

      table(age_rows$cont_indicatorcategory_l3, useNA = "ifany") #what i assigned

#for WBC - 2 values have no specified units (even in article) - exclude from overall median/mean
continuous_df <- continuous_df %>%
  mutate(across(
    c(R_mean, R_median, S_mean, S_median),
      ~ ifelse(
        cont_indicatorcategory_l3 == "WBC_value" &
          tolower(cont_indicatorcategory_l5_unit) %in% c("unspecified", "na", "", "none"),
        NA_real_, .x))
  )

#Before calculating overall mean/median check that values are correct (that no messing like with the p-value) sp for suscpetible
continuous_df %>%
  filter(
    (is.na(R_mean) & is.na(R_median)) |             # both R_mean and R_median are NA
      R_mean <= 0 | R_median <= 0 | S_mean <= 0 | S_median <= 0  # any of the 4 ≤ 0
  ) %>%
  select(
    `Covidence #`, cont_indicatorcategory_l2,
     R_mean, `resistant_group mean`, R_median, `resistant_group median`,
     S_mean,  `susceptible_comparator_group mean`, S_median,`susceptible_comparator_group median`
  ) %>%
  View()
# 20 studies - #637, #3754 both mean and median (zero values) - use mean, ignore median, 2577 - whz negative value OK,
#2595, 3975, 4247, 4740 LoS priorBSI zero valid, 3451 & 7504 pitt score zero valid, 3581 neutropenia days zero ok,
#3992 time to appropriate therapy, zero valid, 4264 verified in article valid, 4491 NA in initial data sign but no data provided in article
#others verified, include WBC no value specified so mean/median analyses values replaced by NA
#949, #6441 should be out already proportions, replace all by NA,

# -----------------CONTINUOUS - OVERALL mean and RANGE median---------------------------------------------
#a.for studies where both mean and median are there for l2, pick one - preferred mean
one_per_study_l2 <- continuous_df %>%
  mutate(
    L1 = cont_indicatorcategory_l1,
    L2 = cont_indicatorcategory_l2,
    mean_pair = !is.na(R_mean)   & !is.na(S_mean),
    med_pair  = !is.na(R_median) & !is.na(S_median),
    # prioritize means first, otherwise medians - leave rows where a pair does not exist
    pri = case_when(mean_pair ~ 1L,
                    med_pair  ~ 2L,
                    TRUE      ~ 3L),

    R_use = case_when(mean_pair ~ R_mean,
                      med_pair  ~ R_median,
                      TRUE      ~ NA_real_),
    S_use = case_when(mean_pair ~ S_mean,
                      med_pair  ~ S_median,
                      TRUE      ~ NA_real_),
    used_metric = case_when(mean_pair ~ "mean",
                            med_pair  ~ "median",
                            TRUE      ~ NA_character_)
  ) %>%
  # keep one record per study × L2 by our priority rule
  group_by(`Covidence #`, L1, L2) %>%
  arrange(pri, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

#b.summary at L1 × L2
l2_summary <- one_per_study_l2 %>%
  group_by(L1, L2) %>%
  summarise(
    n_studies = n_distinct(`Covidence #`),

    #overall means - only rows with mean pair
    R_overall_mean = mean(ifelse(used_metric == "mean", R_use, NA_real_), na.rm = TRUE),
    S_overall_mean = mean(ifelse(used_metric == "mean", S_use, NA_real_), na.rm = TRUE),
    diff_mean      = R_overall_mean - S_overall_mean,

    #median ranges - only rows with median pair
    R_median_min = {
      vals <- if_else(used_metric == "median", R_use, NA_real_)
      if (all(is.na(vals))) NA_real_ else min(vals, na.rm = TRUE)
    },
    R_median_max = {
      vals <- if_else(used_metric == "median", R_use, NA_real_)
      if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
    },
    S_median_min = {
      vals <- if_else(used_metric == "median", S_use, NA_real_)
      if (all(is.na(vals))) NA_real_ else min(vals, na.rm = TRUE)
    },
    S_median_max = {
      vals <- if_else(used_metric == "median", S_use, NA_real_)
      if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
    },

    # unit - single unit - otherwise show "mixed" (less likely) / NA
    unit = {
      u <- unique(na.omit(unit_for_calc))
      if (length(u) == 1) u else if (length(u) == 0) NA_character_ else "mixed"
    },
    .groups = "drop"
  )

#check - stop if something wrong
stopifnot(all(c("R_median_min","R_median_max","S_median_min","S_median_max") %in% names(l2_summary)))

#c.nice presentation columns
  #show only 2 decimals when exported
rnd  <- function(x, k = 2) { x[is.nan(x)] <- NA_real_; round(x, k) }
fmt2 <- function(x) ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = 1))

l2_summary_export <- l2_summary %>%
  mutate(
    median_range_R = if_else(is.na(R_median_min), NA_character_,
                                    paste0(fmt2(R_median_min), " to ", fmt2(R_median_max))),
    median_range_S = if_else(is.na(S_median_min), NA_character_,
                                    paste0(fmt2(S_median_min), " to ", fmt2(S_median_max))),
    R_overall_mean = rnd(R_overall_mean, 2),
    S_overall_mean = rnd(S_overall_mean, 2),
    diff_mean      = rnd(diff_mean,      2)
  ) %>%

  select(
    cont_indicatorcategory_l1 = L1,
    cont_indicatorcategory_l2 = L2,
    n_studies,
    unit,
    R_overall_mean, S_overall_mean, diff_mean,
    median_range_R, median_range_S
  ) %>%
  arrange(cont_indicatorcategory_l1, desc(n_studies))

#d.heatmap style visualization
df <- l2_summary_export %>%
  arrange(cont_indicatorcategory_l1, desc(n_studies), cont_indicatorcategory_l2)

#replace NA in *character* columns by empty string (so Excel shows blank, not #N/A)
df <- df %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))

#robust numeric parser (keeps numbers, strips thousands separators if any)
numify <- function(x) { x <- gsub("[,$]", "", as.character(x)); suppressWarnings(as.numeric(x)) }

df$n_studies      <- suppressWarnings(as.integer(df$n_studies))
df$R_overall_mean <- numify(df$R_overall_mean)
df$S_overall_mean <- numify(df$S_overall_mean)
df$diff_mean      <- numify(df$diff_mean)

#helpers - log-scaling to [0,1] as outliers and big difference between numbers are fading the colors very fast
rescale_log01 <- function(x_nonneg) {
  # x_nonneg must be >= 0 (pass abs(negatives))
  if (!any(is.finite(x_nonneg))) return(rep(NA_real_, length(x_nonneg)))
  y <- log1p(x_nonneg)                 # log(1+x) so 0 -> 0, 1->~0.69, 10,100,1000 step nicely
  r <- range(y, na.rm = TRUE)
  if (r[2] == r[1]) return(ifelse(is.finite(x_nonneg), 1, NA_real_))
  (y - r[1]) / (r[2] - r[1])
}

#color palettes
pal_ns  <- colorRampPalette(c("#FFFFFF", "#4682B4"))  # studies: white -> steelblue
pal_pos <- colorRampPalette(c("#FFFFFF", "#CB181D"))  # positives: white -> red
pal_neg <- colorRampPalette(c("#FFFFFF", "#2CA25F"))  # negatives: white -> green

#compute fill colors
d <- df$diff_mean
pos <- which(is.finite(d) & d > 0)
neg <- which(is.finite(d) & d < 0)
zer <- which(is.finite(d) & abs(d) < 1e-12)

#positive mean diff - shade by log distance from 0 to max - positively associated with AMR
pos_fill <- rep(NA_character_, nrow(df))
if (length(pos)) {
  pos01 <- rescale_log01(d[pos])                      # d[pos] > 0
  lut   <- pal_pos(101); idx <- 1 + floor(100 * pos01)
  pos_fill[pos] <- lut[pmax(1, pmin(101, idx))]
}

#negative mean diff - shade by log of magnitude - negatively associated with AMR (Not true though for all - need to manually check)
neg_fill <- rep(NA_character_, nrow(df))
if (length(neg)) {
  mag01 <- rescale_log01(abs(d[neg]))                 # abs(negatives) >= 0
  lut   <- pal_neg(101); idx <- 1 + floor(100 * mag01)
  neg_fill[neg] <- lut[pmax(1, pmin(101, idx))]
}

#combine (+) and (−) - keep exact zeros white
diff_fill <- ifelse(is.na(pos_fill), neg_fill, pos_fill)
if (length(zer)) diff_fill[zer] <- "#FFFFFF"

#studies shading
ns <- df$n_studies
ns_fill <- rep(NA_character_, nrow(df))
ok_ns <- which(is.finite(ns))
if (length(ok_ns)) {
  ns01 <- rescale(ns[ok_ns], to = c(0, 1))
  lut  <- pal_ns(101); idx <- 1 + floor(100 * ns01)
  ns_fill[ok_ns] <- lut[pmax(1, pmin(101, idx))]
}

#e.excel workbook - paint fills
wb  <- createWorkbook(); sh <- "L2 summary"
addWorksheet(wb, sh)

#header bold; *no* #N/A → keepNA = FALSE
hdr <- createStyle(textDecoration = "bold")
writeData(wb, sh, df, headerStyle = hdr, keepNA = FALSE)

#merge contiguous L1 blocks
col_L1 <- match("cont_indicatorcategory_l1", names(df))
runs   <- rle(as.character(df[[col_L1]]))
row    <- 2
for (len in runs$lengths) {
  mergeCells(wb, sh, cols = col_L1, rows = row:(row + len - 1))
  row <- row + len
}

#align merged L1 cells to right + top
l1_align <- createStyle(halign = "left", valign = "top")
addStyle(wb, sh, l1_align, rows = 2:(nrow(df) + 1), cols = col_L1,
         gridExpand = TRUE, stack = TRUE)

#numeric formatting
num_cols <- match(c("R_overall_mean","S_overall_mean","diff_mean"), names(df))
addStyle(wb, sh, createStyle(numFmt = "#,##0.00"),
         rows = 2:(nrow(df) + 1), cols = num_cols,
         gridExpand = TRUE, stack = TRUE)

#paint utility
paint_column <- function(fill_vec, col_name) {
  col_idx <- match(col_name, names(df))
  ok <- which(!is.na(fill_vec))
  if (!length(ok)) return(invisible())
  by_hex <- split(ok, fill_vec[ok])       # group rows by identical color
  for (hex in names(by_hex)) {
    addStyle(wb, sh, createStyle(fgFill = hex),
             rows = by_hex[[hex]] + 1, cols = col_idx,
             gridExpand = TRUE, stack = TRUE)
  }
}

#apply fills
paint_column(ns_fill,   "n_studies")
paint_column(diff_fill, "diff_mean")

setColWidths(wb, sh, cols = 1:ncol(df), widths = "auto")
freezePane(wb, sh, firstRow = TRUE)

saveWorkbook(wb, "continuous_L2_summary.xlsx", overwrite = TRUE)


#-------------- CONTINUOUS - ADD HIGHRISK AND AMR GROUPS ------------------------------------------------------
#a.small helpers
is_mean_pair   <- function(Rm, Sm)   !is.na(Rm)   & !is.na(Sm)
is_median_pair <- function(Rmd, Smd) !is.na(Rmd)  & !is.na(Smd)
fmt_range <- function(lo, hi, digits = 1) {
  ifelse(is.na(lo) | is.na(hi), NA_character_,
         paste0(formatC(lo, format = "f", digits = digits),
                " to ",
                formatC(hi, format = "f", digits = digits)))
}

#b.add ab_type per row one-per-study (initially cont_indicatorcategory_l6_Ab_type) ----
build_one_per <- function(df) {
  df %>%
    mutate(
      L1 = cont_indicatorcategory_l1,
      L2 = cont_indicatorcategory_l2,
      # ab_type only when L2 ends with "specific"
      ab_type = if_else(
        str_detect(L2, regex("specific\\b", ignore_case = TRUE)),
        as.character(`cont_indicatorcategory_l6_Ab_type`),  # may be NA - handled below
        ""
      ),
      ab_type = coalesce(ab_type, ""),
      mean_pair = !is.na(R_mean)   & !is.na(S_mean),
      med_pair  = !is.na(R_median) & !is.na(S_median),
      pri = case_when(mean_pair ~ 1L, med_pair ~ 2L, TRUE ~ 3L),
      R_use = case_when(mean_pair ~ R_mean,  med_pair ~ R_median, TRUE ~ NA_real_),
      S_use = case_when(mean_pair ~ S_mean,  med_pair ~ S_median, TRUE ~ NA_real_),
      used_metric = case_when(mean_pair ~ "mean", med_pair ~ "median", TRUE ~ NA_character_)
    ) %>%
    group_by(`Covidence #`, L1, L2, ab_type) %>%
    arrange(pri, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}

#c.generic summariser on a one-per-study input (by L1 × L2 × ab_type)
summarise_L2_from_oneper <- function(df) {
  df %>%
    group_by(L1, L2, ab_type) %>%
    summarise(
      n_studies = n_distinct(`Covidence #`),
      R_overall_mean = mean(ifelse(used_metric == "mean", R_use, NA_real_), na.rm = TRUE),
      S_overall_mean = mean(ifelse(used_metric == "mean", S_use, NA_real_), na.rm = TRUE),
      diff_mean      = R_overall_mean - S_overall_mean,
      R_median_min = { v <- if_else(used_metric == "median", R_use, NA_real_); if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE) },
      R_median_max = { v <- if_else(used_metric == "median", R_use, NA_real_); if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
      S_median_min = { v <- if_else(used_metric == "median", S_use, NA_real_); if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE) },
      S_median_max = { v <- if_else(used_metric == "median", S_use, NA_real_); if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
      unit = { u <- unique(na.omit(unit_for_calc)); if (length(u)==1) u else if (length(u)==0) NA_character_ else "mixed" },
      .groups = "drop"
    ) %>%
    transmute(
      cont_indicatorcategory_l1 = L1,
      cont_indicatorcategory_l2 = L2,
      ab_type = ifelse(is.na(ab_type), "", ab_type),
      n_studies = n_studies,
      mean_diff = round(diff_mean, 2),
      R_median_range = if_else(is.na(R_median_min), NA_character_,
                               paste0(formatC(R_median_min, 1, format="f"), " to ", formatC(R_median_max, 1, format="f"))),
      S_median_range = if_else(is.na(S_median_min), NA_character_,
                               paste0(formatC(S_median_min, 1, format="f"), " to ", formatC(S_median_max, 1, format="f")))
    )
}

#d. OVERALL (all included rows, L1 x L2) — split by ab_type when "specific"
overall_one  <- build_one_per(continuous_df)
cont_summary <- summarise_L2_from_oneper(overall_one)

#e. HIGH-RISK — by ab_type

hp_one <- continuous_df %>% filter(highrisk == "high risk") %>% build_one_per()
hi_summary_raw <- summarise_L2_from_oneper(hp_one) %>%
  rename(
    n_studies_highrisk      = n_studies,
    mean_diff_highrisk      = mean_diff,
    R_median_range_highrisk = R_median_range,
    S_median_range_highrisk = S_median_range
  )

#f. AMR GROUPS — by ab_type

COMBOS <- list(
  c3ge  = "C3G-resistant Enterobacterales",
  cre   = "carbapenem-resistant Enterobacterales",
  mrsa  = "methicillin-resistant S. aureus",
  crab  = "carbapenem-resistant A.baumanii",
  vre   = "vancomycin-resistant Enterococci",
  other = c("other/combination of multiple pathogens",
            "penicillin-resistant S.pneumoniae")
)

summarise_combo <- function(df, combos, alias) {
  tmp_one <- df %>%
    filter(pathogen_antibiotic_combination %in% combos) %>%
    build_one_per()

  summarise_L2_from_oneper(tmp_one) %>%
    rename(
      !!paste0("n_studies_", alias) := n_studies,
      !!paste0("mean_diff_", alias) := mean_diff,
      !!paste0("R_median_range_", alias) := R_median_range,
      !!paste0("S_median_range_", alias) := S_median_range
    )
}

combo_tables <- lapply(names(COMBOS), function(nm) summarise_combo(continuous_df, COMBOS[[nm]], nm))
names(combo_tables) <- names(COMBOS)

#g.put together (overall → highrisk → combos) by L1 × L2 × ab_type

subgroups_continuous_summary <- cont_summary %>%
  left_join(hi_summary_raw,
            by = c("cont_indicatorcategory_l1","cont_indicatorcategory_l2","ab_type"))

for (tbl in combo_tables) {
  subgroups_continuous_summary <- subgroups_continuous_summary %>%
    left_join(tbl, by = c("cont_indicatorcategory_l1","cont_indicatorcategory_l2","ab_type"))
}

#h.Excel copy - blank NA and blank L1 after first row in each L1 block
subgroups_continuous_summary_xlsx <- subgroups_continuous_summary %>%
  arrange(cont_indicatorcategory_l1, desc(n_studies), cont_indicatorcategory_l2, ab_type) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.x), "", .x))) %>%
  group_by(cont_indicatorcategory_l1) %>%
  mutate(cont_indicatorcategory_l1 = if_else(row_number() == 1, cont_indicatorcategory_l1, "")) %>%
  ungroup()

#Excel
wb <- createWorkbook()
addWorksheet(wb, "Continuous subgroups")
writeData(wb, "Continuous subgroups", subgroups_continuous_summary_xlsx, keepNA = FALSE)
setColWidths(wb, "Continuous subgroups",
             cols = 1:ncol(subgroups_continuous_summary_xlsx), widths = "auto")
freezePane(wb, "Continuous subgroups", firstRow = TRUE)
saveWorkbook(wb, "subgroup_summarytable_cont_indicators_highrisk_AMR.xlsx", overwrite = TRUE)

#i.HTML with spanner headers (L1 & L2 visible + ab_type)
labels_map <- list(
  overall = list(label = "All included studies",
                 cols  = c("n_studies","mean_diff","R_median_range","S_median_range")),
  highrisk = list(label = "High risk patients",
                  cols  = c("n_studies_highrisk","mean_diff_highrisk",
                            "R_median_range_highrisk","S_median_range_highrisk")),
  c3ge  = list(label = "C3G-resistant Enterobacterales",
               cols  = c("n_studies_c3ge","mean_diff_c3ge",
                         "R_median_range_c3ge","S_median_range_c3ge")),
  cre   = list(label = "carbapenem-resistant Enterobacterales",
               cols  = c("n_studies_cre","mean_diff_cre",
                         "R_median_range_cre","S_median_range_cre")),
  mrsa  = list(label = "methicillin-resistant S. aureus",
               cols  = c("n_studies_mrsa","mean_diff_mrsa",
                         "R_median_range_mrsa","S_median_range_mrsa")),
  crab  = list(label = "carbapenem-resistant A.baumanii",
               cols  = c("n_studies_crab","mean_diff_crab",
                         "R_median_range_crab","S_median_range_crab")),
  vre   = list(label = "vancomycin-resistant Enterococci",
               cols  = c("n_studies_vre","mean_diff_vre",
                         "R_median_range_vre","S_median_range_vre")),
  other = list(label = "other pathogen combinations",
               cols  = c("n_studies_other","mean_diff_other",
                         "R_median_range_other","S_median_range_other"))
)

for_gt <- subgroups_continuous_summary %>%
  arrange(cont_indicatorcategory_l1, desc(n_studies), cont_indicatorcategory_l2, ab_type) %>%
  group_by(cont_indicatorcategory_l1) %>%
  mutate(cont_indicatorcategory_l1 = if_else(row_number() == 1, cont_indicatorcategory_l1, "")) %>%
  ungroup()

tbl <- for_gt %>%
  gt() %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  cols_label(
    cont_indicatorcategory_l1 = "Indicators (L1)",
    cont_indicatorcategory_l2 = "Indicator (L2)",
    ab_type                   = "Ab type (L6)",
    n_studies = "n studies",
    mean_diff = "mean diff R–S",
    R_median_range = "R median range",
    S_median_range = "S median range",
    .list = setNames(rep(list("n studies"),    length(COMBOS)), paste0("n_studies_", names(COMBOS))) |>
      c(setNames(rep(list("mean diff R–S"), length(COMBOS)), paste0("mean_diff_", names(COMBOS)))) |>
      c(setNames(rep(list("R median range"), length(COMBOS)), paste0("R_median_range_", names(COMBOS)))) |>
      c(setNames(rep(list("S median range"), length(COMBOS)), paste0("S_median_range_", names(COMBOS))))
  ) %>%
  tab_spanner(label = "Indicators",
              columns = c(cont_indicatorcategory_l1, cont_indicatorcategory_l2, ab_type))

for (nm in names(labels_map)) {
  cols_present <- intersect(labels_map[[nm]]$cols, names(for_gt))
  if (length(cols_present)) {
    tbl <- tab_spanner(tbl, label = labels_map[[nm]]$label, columns = all_of(cols_present))
  }
}


#j.heatmap coloring for GT (counts = blue, diffs: red/green with log scale) ----------
#helpers
rescale01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(x))) return(rep(NA_real_, length(x)))
  r <- range(x, na.rm = TRUE)
  if (r[2] == r[1]) return(ifelse(is.finite(x), 1, NA_real_))
  (x - r[1]) / (r[2] - r[1])
}
rescale_log01 <- function(x_nonneg) {
  x <- suppressWarnings(as.numeric(x_nonneg))
  x[!is.finite(x) | x < 0] <- NA_real_
  if (!any(is.finite(x))) return(rep(NA_real_, length(x)))
  y <- log1p(x)
  r <- range(y, na.rm = TRUE)
  if (r[2] == r[1]) return(ifelse(is.finite(x), 1, NA_real_))
  (y - r[1]) / (r[2] - r[1])
}
pal_hex <- function(cols) grDevices::colorRampPalette(cols)

#build column sets from current table (matches overall, highrisk, and AMR blocks)
all_cols   <- names(for_gt)
count_cols <- all_cols[grepl("(^|_)n_studies($|_)", all_cols)]     # n_studies, n_studies_highrisk, n_studies_*
diff_cols  <- all_cols[grepl("^mean_diff($|_)",     all_cols)]          # mean_diff, mean_diff_highrisk, mean_diff_*

#color functions that return a vector of hex colors for cells, no NA (white)
color_counts <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  # If no finite numbers -> all white
  if (!any(is.finite(xnum))) return(rep("#FFFFFF", length(x)))
  s01 <- rescale01(xnum)
  lut <- pal_hex(c("#FFFFFF", "#4682B4"))(101)
  out <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  out[!is.finite(xnum)] <- "#FFFFFF"
  out
}
color_diffs <- function(x) {
  d <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(d))) return(rep("#FFFFFF", length(x)))
  out <- rep("#FFFFFF", length(d))  # default white

  pos <- which(is.finite(d) & d > 0)
  neg <- which(is.finite(d) & d < 0)
  zer <- which(is.finite(d) & abs(d) < 1e-12)

  if (length(pos)) {
    s01 <- rescale_log01(d[pos])
    lut <- pal_hex(c("#FFFFFF", "#CB181D"))(101)
    out[pos] <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  }
  if (length(neg)) {
    s01 <- rescale_log01(abs(d[neg]))
    lut <- pal_hex(c("#FFFFFF", "#2CA25F"))(101)
    out[neg] <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  }
  if (length(zer)) out[zer] <- "#FFFFFF"
  out
}


#apply to the gt table
if (length(count_cols)) {
  tbl <- tbl %>%
    data_color(
      columns = dplyr::all_of(count_cols),
      colors  = color_counts
    )
}
if (length(diff_cols)) {
  tbl <- tbl %>%
    data_color(
      columns = dplyr::all_of(diff_cols),
      colors  = color_diffs
    )
}

gtsave(tbl, "subgroup_summarytable_cont_indicators_highrisk_AMR.html")

# CONTINUOUS - BY STUDY POPULATION TABLE ------------------------------------------------

#a.build one-per-study with studypop carried along
one_per_studypop <- continuous_df %>%
  mutate(
    L1 = cont_indicatorcategory_l1,
    L2 = cont_indicatorcategory_l2,
    ab_type = if_else(
      str_detect(L2, regex("specific\\b", ignore_case = TRUE)),
      as.character(`cont_indicatorcategory_l6_Ab_type`),
      ""
    ),
    ab_type = coalesce(ab_type, ""),
    mean_pair = !is.na(R_mean)   & !is.na(S_mean),
    med_pair  = !is.na(R_median) & !is.na(S_median),
    pri = case_when(mean_pair ~ 1L, med_pair ~ 2L, TRUE ~ 3L),
    R_use = case_when(mean_pair ~ R_mean,  med_pair ~ R_median, TRUE ~ NA_real_),
    S_use = case_when(mean_pair ~ S_mean,  med_pair ~ S_median, TRUE ~ NA_real_),
    used_metric = case_when(mean_pair ~ "mean", med_pair ~ "median", TRUE ~ NA_character_)
  ) %>%
  group_by(`Covidence #`, L1, L2, ab_type, studypop) %>%
  arrange(pri, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

#b.summarise per population
studypop_summary <- one_per_studypop %>%
  group_by(L1, L2, ab_type, studypop) %>%
  summarise(
    n_studies = n_distinct(`Covidence #`),
    R_overall_mean = mean(ifelse(used_metric == "mean", R_use, NA_real_), na.rm = TRUE),
    S_overall_mean = mean(ifelse(used_metric == "mean", S_use, NA_real_), na.rm = TRUE),
    diff_mean      = R_overall_mean - S_overall_mean,
    R_median_min = { v <- if_else(used_metric == "median", R_use, NA_real_); if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE) },
    R_median_max = { v <- if_else(used_metric == "median", R_use, NA_real_); if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
    S_median_min = { v <- if_else(used_metric == "median", S_use, NA_real_); if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE) },
    S_median_max = { v <- if_else(used_metric == "median", S_use, NA_real_); if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
    .groups = "drop"
  ) %>%
  mutate(
    R_median_range = if_else(is.na(R_median_min), "", paste0(formatC(R_median_min, 1, format="f"), " to ", formatC(R_median_max, 1, format="f"))),
    S_median_range = if_else(is.na(S_median_min), "", paste0(formatC(S_median_min, 1, format="f"), " to ", formatC(S_median_max, 1, format="f"))),
    mean_diff = round(diff_mean, 2)
  ) %>%
  select(
    cont_indicatorcategory_l1 = L1,
    cont_indicatorcategory_l2 = L2,
    ab_type,
    studypop,
    n_studies, mean_diff, R_median_range, S_median_range
  ) %>%
  arrange(cont_indicatorcategory_l1, cont_indicatorcategory_l2, ab_type, studypop)

#c.choose the display order + create safe suffixes (column name parts)
studypop_levels <- c("Neonates",
                     "Pediatrics",
                     "Pediatrics incl. neonates",
                     "Adults",
                     "Adults & Pediatrics",
                     "All ages or not specified"
                     )

safe_suffix <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace("^_|_$", "")
}

#map each population to a short, safe suffix
pop_map <- setNames(safe_suffix(studypop_levels), studypop_levels)

#d.pivot the long studypop table → wide with one 4-col block per population
studypop_wide <- studypop_summary %>%
  mutate(studypop = factor(studypop, levels = studypop_levels),
         pop_sfx  = pop_map[as.character(studypop)]) %>%
  pivot_wider(
    id_cols    = c(cont_indicatorcategory_l1, cont_indicatorcategory_l2, ab_type),
    names_from = pop_sfx,
    values_from = c(n_studies, mean_diff, R_median_range, S_median_range),
    names_glue  = "{.value}_{pop_sfx}"
  )

#e. build GT table: keep L1 & L2 visible (+ ab_type), blank repeated L1, add spanners
for_gt_sp <- studypop_wide %>%
  arrange(cont_indicatorcategory_l1, cont_indicatorcategory_l2, ab_type) %>%
  group_by(cont_indicatorcategory_l1) %>%
  mutate(cont_indicatorcategory_l1 = if_else(row_number()==1, cont_indicatorcategory_l1, "")) %>%
  ungroup()

#f.label map for spanners (pretty label -> its 4 columns)
# dynamic labels: only label columns that actually exist (adults and )
present_cols <- names(for_gt_sp)

#which studypop suffixes actually have any columns present?
present_sfx <- Filter(function(sfx) {
  any(c(paste0("n_studies_",      sfx),
        paste0("mean_diff_",      sfx),
        paste0("R_median_range_", sfx),
        paste0("S_median_range_", sfx)) %in% present_cols)
}, unname(pop_map))

#build the label list only for present columns
label_list <- list(
  cont_indicatorcategory_l1 = "Indicators (L1)",
  cont_indicatorcategory_l2 = "Indicator (L2)",
  ab_type                   = "Ab type (L6)"
)

#add sublabels for each present population block
for (sfx in present_sfx) {
  label_list[[paste0("n_studies_",      sfx)]] <- "n studies"
  label_list[[paste0("mean_diff_",      sfx)]] <- "mean diff R–S"
  label_list[[paste0("R_median_range_", sfx)]] <- "R median range"
  label_list[[paste0("S_median_range_", sfx)]] <- "S median range"
}

#apply labels (only for existing columns) — build gt first, then do.call on it
tbl_studypop <- for_gt_sp %>%
  gt() %>%
  fmt_missing(columns = everything(), missing_text = "")

#dynamic cols_label: do.call needs the gt object as first arg inside a list
tbl_studypop <- do.call(
  cols_label,
  c(list(tbl_studypop), label_list)   # first element is the gt table, then named labels
)

#spanner over the indicator columns
tbl_studypop <- tbl_studypop %>%
  tab_spanner(label = "Indicators",
              columns = c(cont_indicatorcategory_l1, cont_indicatorcategory_l2, ab_type))

#g.build spanners only for present populations
# figure out which pretty labels (in order) are present
present_labels <- studypop_levels[
  unname(pop_map[studypop_levels]) %in% present_sfx
]

#add one spanner per *present* population block (order preserved)
for (lbl in present_labels) {
  sfx <- pop_map[[lbl]]
  cols_this <- c(paste0("n_studies_",      sfx),
                 paste0("mean_diff_",      sfx),
                 paste0("R_median_range_", sfx),
                 paste0("S_median_range_", sfx))
  cols_present <- intersect(cols_this, names(for_gt_sp))
  if (length(cols_present)) {
    tbl_studypop <- tab_spanner(
      tbl_studypop,
      label   = lbl,
      columns = all_of(cols_present)
    )
  }
}

#h.heatmap coloring (same palettes + log scale, no NAs returned)
rescale01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(x))) return(rep(NA_real_, length(x)))
  r <- range(x, na.rm = TRUE)
  if (r[2] == r[1]) return(ifelse(is.finite(x), 1, NA_real_))
  (x - r[1]) / (r[2] - r[1])
}
rescale_log01 <- function(x_nonneg) {
  x <- suppressWarnings(as.numeric(x_nonneg))
  x[!is.finite(x) | x < 0] <- NA_real_
  if (!any(is.finite(x))) return(rep(NA_real_, length(x)))
  y <- log1p(x)
  r <- range(y, na.rm = TRUE)
  if (r[2] == r[1]) return(ifelse(is.finite(x), 1, NA_real_))
  (y - r[1]) / (r[2] - r[1])
}
pal_hex <- function(cols) grDevices::colorRampPalette(cols)

all_cols   <- names(for_gt_sp)
count_cols <- grep("(^|_)n_studies_", all_cols, value = TRUE)
diff_cols  <- grep("^mean_diff_",     all_cols, value = TRUE)

color_counts <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(xnum))) return(rep("#FFFFFF", length(x)))
  s01 <- rescale01(xnum)
  lut <- pal_hex(c("#FFFFFF", "#4682B4"))(101)
  out <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  out[!is.finite(xnum)] <- "#FFFFFF"
  out
}
color_diffs <- function(x) {
  d <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(d))) return(rep("#FFFFFF", length(x)))
  out <- rep("#FFFFFF", length(d))
  pos <- which(is.finite(d) & d > 0)
  neg <- which(is.finite(d) & d < 0)
  zer <- which(is.finite(d) & abs(d) < 1e-12)
  if (length(pos)) {
    s01 <- rescale_log01(d[pos])
    lut <- pal_hex(c("#FFFFFF", "#CB181D"))(101)
    out[pos] <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  }
  if (length(neg)) {
    s01 <- rescale_log01(abs(d[neg]))
    lut <- pal_hex(c("#FFFFFF", "#2CA25F"))(101)
    out[neg] <- lut[1 + floor(100 * pmin(pmax(s01, 0), 1))]
  }
  if (length(zer)) out[zer] <- "#FFFFFF"
  out
}

if (length(count_cols)) {
  tbl_studypop <- tbl_studypop %>%
    data_color(columns = all_of(count_cols), colors = color_counts)
}
if (length(diff_cols)) {
  tbl_studypop <- tbl_studypop %>%
    data_color(columns = all_of(diff_cols), colors = color_diffs)
}

#save
gtsave(tbl_studypop, "continuous_L2_by_studypop.html")

#### CONTINOUS ANALYSIS END  __________________________________________________________________________________________________


# #BI - initial code - continuous indicators
# # create categories to group reported indicators -> continuous indicators reported in "resistant_group variable"
# continuous_df <- continuous_df %>%  mutate(indicatorcategory = case_when(
#   str_detect(`resistant_group variable`, regex("age.*years", ignore_case = TRUE)) ~ "Age",
#   str_detect(`resistant_group variable`, regex("gestation|birthweight|crib", ignore_case = TRUE)) ~ "Preterm birth/low birth weight",
#   str_detect(`resistant_group variable`, regex("apache|mccabe|sofa|pitt|pneumonia severity index|psis|saps|Severity grade", ignore_case = TRUE)) ~ "Clinical severity score",
#   #    str_detect(`resistant_group variable`, regex("hospital stay", ignore_case = TRUE)) ~ "Prior hospital stay (categorical)",
#   str_detect(`resistant_group variable`, regex("duration of hospitalization prior to having bacteremia|hospital stay.*days|Length of stay|LOS,days|Length of hospital|Time from admission|Length of hospital stay, total days|Days of admission before infection", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)",
#   str_detect(`resistant_group variable`, regex("Time at risk|Days from admission to positive culture|Days from hospital admission to BSI|Total LOS|Duration of time from hospital admission to positive blood culture", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)",
#   str_detect(`resistant_group variable`, regex("LOS in preceding yr|Total days of hospitalization in the 6 months prior to current hospitalization|Admission days prior to index culture|Index hospital stay|Sequential time to positivity|Time to positivity ratio", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)", # double check Sequential time to positivity and Time to positivity ratio
#   str_detect(`resistant_group variable`, regex("ICU", ignore_case = TRUE)) ~ "Prior ICU stay (categorical)",
#   str_detect(`resistant_group variable`, regex("ICU stay days", ignore_case = TRUE)) ~ "Prior ICU stay (ordinal)",
#   str_detect(`resistant_group variable`, regex("Quantitative indices of antibiotic usage|Total antibiotic treatment|Antibiotic-days|Duration of exposure to antimicrobial agent, days", ignore_case = TRUE)) ~ "Antibiotic exposure: duration (numeric)", # check Total antibiotic treatment
#   str_detect(`resistant_group variable`, regex("DDDs|Days of extended-spectrum|Days of third-|Days of carbapenem|Days of aminoglyc|Total days of antibiotic", ignore_case = TRUE)) ~ "Antibiotic exposure: duration (numeric)", # might have to break up between dose (daptomycin) and duration, also check Total antibiotic treatment
#   str_detect(`resistant_group variable`, regex("number of different antibiotics used|no. of prior antibiotics", ignore_case = TRUE)) ~ "Antibiotic exposure: different antibiotics (numeric)", # might have to break up between dose (daptomycin) and duration, also check Total antibiotic treatment
#   str_detect(`resistant_group variable`, regex("Therapy with antibiotics prior 30 days of infection - |Types of antibiotics", ignore_case = TRUE)) ~ "Antibiotic exposure (categorical)",
#   str_detect(`resistant_group variable`, regex("LOS from culture to discharge|Survival time|time to first negative blood culture|Survival|LOS after bacteremia|antibiotic administration postculture", ignore_case = TRUE)) ~ "Patient outcomes", # still check if survival is defined as time until death or time until discharge
#   `resistant_group variable`== "Hospital days" ~ "Patient outcomes", # there's a note that states it is likely total admission, and not just prior or after BSI
#   str_detect(`resistant_group variable`, regex("charlson|absi|no. of comorbidities", ignore_case = TRUE)) ~ "Comorbidity score",
#   str_detect(`resistant_group variable`, regex("absi", ignore_case = TRUE)) ~ "Burn severity",
#   str_detect(`resistant_group variable`, regex("ntiss| feeding tube", ignore_case = TRUE)) ~ "Invasive procedures",
#   str_detect(`resistant_group variable`, regex("temperatu|blood pressure|apgar", ignore_case = TRUE)) ~ "Vital signs", # apgar score is largely based on vital signs
#   str_detect(`resistant_group variable`, regex("creatinine|bilirubin|cholinest|total protein|albumin|LDH|CKMB|urea nitrogen|uric acid", ignore_case = TRUE)) ~ "Kidney/liver lab values",
#   str_detect(`resistant_group variable`, regex("monocyte|neutropenia|wbc|hemoglobin|neutrophil|platelet|International normalized ratio|Hb|Haematocr", ignore_case = TRUE)) ~ "Blood lab values",
#   str_detect(`resistant_group variable`, regex("tnf|procalciton|crp", ignore_case = TRUE)) ~ "Inflammatory lab values",
#   str_detect(`resistant_group variable`, regex("blood transfusion", ignore_case = TRUE)) ~ "Blood transfusion",
#   str_detect(`resistant_group variable`, regex("comorbidity|diabetes|cirrhosis|hypertens|Liver Disease", ignore_case = TRUE)) ~ "NCD",
#   str_detect(`resistant_group variable`, regex("cost|economic|burden|usd|eur|cny|sgd|jpy", ignore_case = TRUE)) ~ "Costs / economic",
#   str_detect(`resistant_group variable`, regex("days to active therapy|duration from bacteremia to receiving appropriate antibiotic|Hours to appropriate therapy", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
#   str_detect(`resistant_group variable`, regex("Time to appropriate therapy|Time to adequate antibiotic therapy|Overall time to first dose of appropriate antibiotic therapy", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
#   str_detect(`resistant_group variable`, regex("Hours to active antibiotic therapy|Time to microbiologically appropriate antibiotic therapy|no. of days to active", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
#   # for the remaining (which could not be assigned to one of the groups, or were wrongly categorised), enter the exact string
#   `resistant_group variable` %in% c(
#     "Days of hospital stay", # to check if as outcome or as exposure
#     "hospital stay", # to check if as outcome or as exposure
#     "Overall - Hospital days before bacteremia",
#     "Hospital days before bacteremia",
#     "days hospitalization", # to check if as outcome or as exposure
#     "duration of hospitalization (days)",
#     "LOS prior to isolation of GNB, days",
#     "LOS before blood culture (days)",
#     "Number of hospitalization days in the 3 months before bacteremia",
#     "Overall admission, days", # to check if as outcome or as exposure
#     "Time between hospital admission and BSI onset (days)",
#     "time from hospitalisation to KPBSI (days)",
#     "Admission days till bacteraemia",
#     "Duration of hospitalization before bacteremia (days)",
#     "no. of days of hospital residency prior to\r\nculture"
#   ) ~ "Duration hospital stay before BSI (ordinal)",
#   `resistant_group variable` %in% c(
#     "no. of days to active therapy",
#     "no. of days to active\r\ntherapy ",
#     "Days to appropriate therapy",
#     "Appropriate antimicrobial therapy within 2 days",
#     "Appropriate antimicrobial therapy within 3 days",
#     "Clinical course and outcomes - Time to appropriate treatment in days"
#   ) ~ "Duration to appropriate therapy (ordinal)",
#   `resistant_group variable` %in% c(
#     "Prior no. of days of antibiotic therapy",
#     "Patients with previous antibiotic treatment - Days on antimicrobial treatment",
#     "Exposure to antibiotics-Antibiotic days",
#     "Fluoroquinolone therapy (days)",
#     "Piperacillin-tazobactam (days)",
#     "Meropenem (days)",
#     "Vancomycin (days)",
#     "duration of antibiotic therapy (days)",
#     "Duration of previous antibiotic use (days)",
#     "Previous antibiotic duration (days)",
#     "Duration of therapy (days)",
#     "Combined treatment days", # to check if as outcome or as exposure
#     "Length of use (days)", # unclear if as outcome or as exposure
#     "Antibiotic use with carbapenem in previous 84 days",
#     "Quantitative indices of antibiotic usage - Antibiotic-days/patient"
#   ) ~ "Antibiotic exposure: duration (numeric)",
#   `resistant_group variable` %in% c(
#     "Patients with previous antibiotic treatment - Types of antimicrobials",
#     "Patients with previous antibiotic treatment - Antimicrobial families",
#     "Patients with previous antibiotic treatment - Different antimicrobial families",
#     "Quantitative indices of antibiotic usage - number of different antibiotics used/patient",
#     "All patients - Types of antimicrobials",
#     "All patients - Different antimicrobial families",
#     "Number of antibiotic agent"
#   ) ~ "Antibiotic exposure: different antibiotics (numeric)",
#   `resistant_group variable` %in% c(
#     "Days of positive blood culture",
#     "time to resolution of BSI - days (first of the 2 consecutively negative blood cultures after infection) - Pediatric patients",
#     "Duration of bacteremia (days)",
#     "Duration of Bacteremia, days",
#     "Duration of bacteremia",
#     "Only patients who had hospital-acquired bacteremia - time to bacteremia from admission date",
#     "Number of sets of positive blood cultures",
#     "LOS after GNB bacteremia, days",
#     "LOS after BSI, days",
#     "Hospital stay after diagnosis",
#     "Hospital stay after bacteremia",
#     "Post Infection LOS (>30 day survivors)",
#     "duration of hospital stay postinfection for survivors",
#     "no. of days of hospital stay postinfection for survivors",
#     "no. of days of hospital\r\nstay postinfection for survivors",
#     "no. of days of antibiotic administration postculture",
#     "no. of days of\r\nantibiotic administration\r\npostculture",
#     "hospital days among patients who did not receive effective empirical antibiotic treatment"
#   ) ~ "Patient outcomes",
#   `resistant_group variable` %in% c(
#     "Leukocytes (cells/mm3)",
#     "Laboratory examination - White blood cell"
#   ) ~ "Blood lab values",
#   `resistant_group variable` %in% c(
#     "Predisposing factors - CD4 count, cells/ml",
#     "Chronic Liver Failure-Consortium Acute-on-Chronic Liver Failure (CLIF-C ACLF)",
#     "BMI",
#     "Organ failure at admission",
#     "Model of End-Stage Liver Disease-Na"
#   ) ~ "Comorbidity score",
#   `resistant_group variable` %in% c(
#     "Total numbers of invasive procedures",
#     "Length of mechanical ventilation at onset of bacteraemia",
#     "Time to start enteral feeds (days)",
#     "Duration of central venous device (days)",
#     "Mechanical ventilation duration (days)",
#     "Encounter-specific risk factors (prior to development of bacteraemia) - Urinary catheter, days",
#     "Arterial catheter (AC) - Overall duration of AC"
#   ) ~ "Invasive procedures",
#   `resistant_group variable` %in% c(
#     "Outcome - alive (days)",
#     "Days to discharge",
#     "Duration of Follow-Up, patient days",
#     "Duration of Fever after Hospital Admission, days"
#   ) ~ "Patient outcomes",
#   `resistant_group variable` %in% c(
#     "Disseminated intravascular coagulation (DIC) score"
#   ) ~ "Clinical severity score",
#   `resistant_group variable` %in% c("Age", "Age (yr)", "Age (yrs)", "Age (months)", "Age at onset of LOS (days)") ~ "Age",
#   `resistant_group variable` %in% c("weight-for-age z-score") ~ "Preterm birth/low birth weight",
#   `resistant_group variable` %in% c("Days since last hospitalization",
#                                     "Duration of Symptoms on Presentation, days", # surprised there are not more of this
#                                     "days to death after admission",
#                                     "Monotherapy - daptomycin dose (mg/kg)" # check if this could be a separate category - surprisingly seems to be the only dose/dosage metric
#   ) ~ "Other",
#   `resistant_group variable` %in% c("Infection or colonization of K. pneumoniae in previous 84 days") ~ "Prior infection/colonisation",
#   TRUE ~ "Other"))

# # display all numerical indicators
# numindicators <- continuous_df %>%
#   filter(!is.na(`resistant_group variable`)) %>%
#   select(`resistant_group variable`, indicatorcategory, `resistant_group notes`) %>%
#   distinct()
# numindicators
# write_xlsx(numindicators, "numindicators.xlsx")
#
# # keep only essential variables, filter out the sets without numbers entered, then remove duplicates
# print(colnames(continuous_df), max = 1900)
# continuous_dfshort <- continuous_df %>% select(`Study ID`, Study_ID, Study_country, Publication_year, Study_setting, Model, set, amr, Resistant_group_tot_nb,
#                              Susceptible_group_definition, Susceptible_group_tot_nb, indicatorcategory, `resistant_group variable`, #`Proportion Variable_description`,
#                             `resistant_group p-value` #,
#                              # set2, `Number Resistant_group_value`, `Number Susceptible_comparator_group_value`, `Number p-value`, `Number Total`
#                             ) %>%
#   filter(!is.na(`resistant_group variable`)) %>%
#   distinct()

# DESCRIPTIVE_dichotomous_proxy-indicators -> reshape the second part of df_long in a longer format, in which all variables containing the categorical indicators Number_1, Number_2, etc. are brought together in the same variable
categorical_df <- df_long %>%
  select(1:32, studypop, highrisk, amr, pathogen_antibiotic_combination, 394:965, Model, Resistant_group_tot_nb, Susceptible_group_tot_nb)

#join categorical info - 6 additional studies
#read relevant sheet
# new_cat_info <- read.xlsx("additional_studies_extract_info.xlsx", sheet = "categorical_ind")
# #check column alignment
# setdiff(names(categorical_df), names(new_cat_info))
# setdiff(names(new_cat_info), names(categorical_df))
# #append
# categorical_df <- bind_rows(categorical_df, new_cat_info)
# #dinal check
# nrow(categorical_df)

categorical_df <- categorical_df %>%
  rename_with(~ str_replace(.x, "^(Number|Proportion|95%CI)_(\\d+) (.*)$", "\\1 \\3_\\2")) # put the sequential numbers at the end

categorical_df <- categorical_df %>% mutate(across(contains("Number Resistant_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Number Susceptible_comparator_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Resistant_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Susceptible_comparator_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Total_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion p-value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("95%CI Resistant_group_value_"), as.character)) # same format of variables that are combined in the longer df

categorical_df <- categorical_df %>%  pivot_longer(
  cols = matches("_(?:[1-9]|1[0-9]|2[0-9]|3[0-8])$"),   # matches _1 to _38
  names_to = c(".value", "set2"),             # .value keeps base var names
  names_pattern = "(.*)_(\\d+)$")
colnames(categorical_df)

# get rid of empty rows (per study model up to 38 indicators reported. for most studies less, so many rows are empty)
categorical_df <- categorical_df %>% filter(!is.na(`Number Variable_description`))

# regroup the categorical exposures of interest/indicators in `Proportion Variable_description` -> NEED TO BE FURTHER CLEANED - some labels might be added that are mistakenly added
# I would also break up 'invasive procedures' and 'comorbidities'
# invasive procedures subcategories: surgery, airways (incl ventilator/intubation), implanted devices/protheses, injections/infusions, urinary or gastro-intestinal tubes, vascular access (incl dialysis, IV cannules, CVC)
categorical_df <- categorical_df %>%  mutate(
    indicatorcategory_level2 = case_when(
      str_detect(`Proportion Variable_description`, regex("ICU|intensive care", ignore_case = TRUE)) ~ "Prior ICU stay",
      str_detect(`Proportion Variable_description`, regex("Clinical severity - All-cause 30-day mortality|Overall in-hospital mortality", ignore_case = TRUE)) ~ "Patient outcomes, mortality", # is an outcome, so avoid that it's labelled 'severity' or 'prior hospi'
      str_detect(`Proportion Variable_description`, regex("Overall LOS, days - 16+|Infection-associated LOS, days - 16+ ", ignore_case = TRUE)) ~ "Patient outcomes, duration of hospitalisation or treatment", # is an outcome, so avoid that it's labelled 'severity' or 'prior hospi'
      # hospital-acquired vs healthcare associated are tricky. sometimes (primary) healthcare exposure prior to hospitalisation is reported, which is quite different and needs its own category
      str_detect(`Proportion Variable_description`, regex("Route of infection - Non nosocomial healthcare associated|Source of infection-Healthcare|Healthcare associated|Healthcare-associated\r\ninfections", ignore_case = TRUE)) ~ "Healthcare associated, other than hospital-acquired or non specified if hospital",
      `Proportion Variable_description` %in% c("Healthcare-associated infection", "Healthcare-associated", "Health-care associated BSI", "Community/health care associated - Health care associated|Background parameters - Health care-associated acquisition of pathogen|Acquisition of bacteraemia-Healthcare-associated|Acquisition and onset setting -  healthcare-associated community-onset|Acquisition-Nosocomial and healthcare acquired") ~ "Healthcare associated, other than hospital-acquired or non specified if hospital",
      str_detect(`Proportion Variable_description`, regex("Community|Route of infection - Non-health care associated|Length of hospital stay before positive blood culture - <48h|Community or ambulatory health care|Hospitalization never or not within 1 yr", ignore_case = TRUE)) ~ "Community-acquired",
      str_detect(`Proportion Variable_description`, regex("Hospital-acquired|nosocomial|Currently resident in an intermediate level health care facility|Healthcare-asscociated >=48h", ignore_case = TRUE)) ~ "Hospital-acquired",
      str_detect(`Proportion Variable_description`, regex("Prior hospital admission|hospitalisation|hospital stay|readmission|Hospital-acquired|Recent international healthcare exposure|Prior healthcare abroad|Inpatient in previous month", ignore_case = TRUE)) ~ "Prior hospitalisation", # check if nosocomial or hospital/healthcare associated should be a separate category
      str_detect(`Proportion Variable_description`, regex("Hospitalization|Hospitalisation|Previous admission|Admission history|Prior hospital", ignore_case = TRUE)) ~ "Prior hospitalisation",
      str_detect(`Proportion Variable_description`, regex("Hospital", ignore_case = TRUE)) ~ "Prior hospitalisation",
      str_detect(`Proportion Variable_description`, regex("Living in a care home|long-term care|long term care|long-term-care|nursing home|Long-term acute care facility residence", ignore_case = TRUE)) ~ "Long-term care facility",
      str_detect(`Proportion Variable_description`, regex("primary infection site|cellulitis|chest infection|Catheter-associated", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("Prior ESBL|Previous .*infection|Previous .*isolate|History of .*infection", ignore_case = TRUE)) ~ "Prior infection",
      str_detect(`Proportion Variable_description`, regex("colonizat|Colonisation|Colonization|infection or\r\ncolonization of", ignore_case = TRUE)) ~ "Prior colonization",
      str_detect(`Proportion Variable_description`, regex("Urological manipulation history|Thoracentesis|Tracheal|Cannula|Aspiration|Nutrition|pacemaker|catheter|surgery|surgical proced|caesarian|cesarian|Caesarean|intubat|foley|catheter|central line|ventilator|surgery|invasive|hemodialys|mechanical ventilat|central venous line|gastric tube|parenteral nutrit", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("Device|Catheter|Intubation|Surgery|History of genitourinary intervention|Operation|Bronchoscopy|Drain|Tube|Endoscopy|Tracheo|Puncture|Previous surgery |dialysis|blood purification", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("leukocytes|lymphocytopenia|coagulation|low hemoglobin|low wbc|neutropenia|thrombocytopenia|Hypoproteinemia|monocyte|neutropenia|wbc|hemoglobin|neutrophil|platelet|International normalized ratio|Hb|Haematocr", ignore_case = TRUE)) ~ "Low blood values",
      str_detect(`Proportion Variable_description`, regex("Requirement of blood transfusion(s)|sepsis|shock|clinical severity|Pitt", ignore_case = TRUE)) ~ "Clinical severity",
      str_detect(`Proportion Variable_description`, regex("blood transfusion", ignore_case = TRUE)) ~ "Blood transfusion",
      str_detect(`Proportion Variable_description`, regex("transplant|Organ transplantation", ignore_case = TRUE)) ~ "Transplant",
      str_detect(`Proportion Variable_description`, regex("burn", ignore_case = TRUE)) ~ "Burns",
      str_detect(`Proportion Variable_description`, regex("crp|procalcitonin|biomarker", ignore_case = TRUE)) ~ "Biomarker positive",
      str_detect(`Proportion Variable_description`, regex("congestive heart failur|myocardial infarc|cerebral infarction|stroke|vascular disease|cardiac|hypertens|hypotens", ignore_case = TRUE)) ~ "Comorbidities, cardiovascular",
      str_detect(`Proportion Variable_description`, regex("diabet|renal|kidney|liver|urological disease", ignore_case = TRUE)) ~ "Comorbidities, metabolic/liver/kidney/urological",
      str_detect(`Proportion Variable_description`, regex("solid organ tumor|cancer|malignanc|leukaemia|leukemia|lymphoma|haematological disease|neuroblastoma|tumou?r|neoplasia|malignant", ignore_case = TRUE)) ~ "Comorbidities, cancer/hematologic",
      str_detect(`Proportion Variable_description`, regex("copd|asthma", ignore_case = TRUE))     ~ "Comorbidities, respiratory",
      str_detect(`Proportion Variable_description`, regex("dementia|hemipleg|cognitive decline|charlson|underlying|frailty|rheumatic|peptic ulcer disease|chronic neurological|connective tissue disease|Sickle cell|immunosuppressi|autoimmune", ignore_case = TRUE))       ~ "Comorbidities, cognitive/auto-immune/other",
      str_detect(`Proportion Variable_description`, regex("Severe underweight-for-age|severe underweight for age|malnutr|malnourish", ignore_case = TRUE)) ~ "Comorbidities, underweight/malnutrition",
      str_detect(`Proportion Variable_description`, regex("age >|age>|age - adults|Age - 65 plus|Age of >60 yr|Age(years) - 80+|Age - â‰¥80|Age category - >= 80|Age(years) - 80+|Age â‰¥ 75 ys|Age â‰¥ 65", ignore_case = TRUE)) ~ "Older age (cutoffs >/= 60 years)",
      str_detect(`Proportion Variable_description`, regex("preterm|low birth weight|prematurity|Gestation|Birth weight|Birthweight", ignore_case = TRUE)) ~ "Preterm birth/low birth weight",
      str_detect(`Proportion Variable_description`, regex("infant|neonate|child|young age|newborn|Inborn|Age - < 5|Age - <5|Age group (years) - 0-4", ignore_case = TRUE)) ~ "Young age (cutoffs </= 5 years)",
      str_detect(`Proportion Variable_description`, regex("cellulitis|pneumonia|wound|Endocarditis|infection site|Source of |focus|Combined infection site|pulmonary infection|Mucosal barrier damage at the time of BSI", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("Pyelonephritis|Primary site|Skin|Soft tissue|Urinary tract|Biliary|Bone|Joint|Abdominal|Hepato|Lung|Respiratory|Gastro", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("resistant|susceptible|intermediate|isolated|Resistance to | resistance", ignore_case = TRUE)) ~ "Resistance profile",
      str_detect(`Proportion Variable_description`, regex("ESBL|MDR|Resistance|Susceptibility|Resistant", ignore_case = TRUE)) ~ "Resistance profile",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|cephalosporin|carbapenem|vancomycin|fluoroquinolone", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("duration", ignore_case = TRUE))) ~ "Prior antibiotic exposure, duration",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|treatment|therapy", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("inappropriate|inadequate|incorrect|failure|wrong|inopportune", ignore_case = TRUE))) ~ "Inappropriate empirical antibiotic treatment",
      str_detect(`Proportion Variable_description`, regex("Therapeutic management - Adequate targeted |Therapeutic management - Adequate initial |Appropriate directed therapy|Appropriate Empiric therapy|Appropriate empirical therapy| appropriate empiric antibiotic therapy|Appropriate antimicrobial therapy - Appropriate therapy administered overall|Appropriate antibiotic therapy", ignore_case = TRUE)) ~ "Appropriate empirical antibiotic choice",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|chemotherapy|therapy|treatment|use|exposure", ignore_case = TRUE))&str_detect(`Proportion Variable_description`, regex("tigecycline|polymixin|cephalosporin|meropenem|carbapenem|vancomycin|fluoroquinolone|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide|vancomycin|polymyxin", ignore_case = TRUE))) ~ "Prior antibiotic exposure, specific choice",
      str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|chemotherapy", ignore_case = TRUE)) ~ "Prior antibiotic exposure, non specific",
      # str_detect(`Proportion Variable_description`, regex("cephalosporin|carbapenem|vancomycin|fluoroquinolone|therapy|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide", ignore_case = TRUE)) ~ "Prior antibiotic exposure",
      str_detect(`Proportion Variable_description`, regex("corticoid|steroid", ignore_case = TRUE)) ~ "Prior corticosteroid use",
      (str_detect(`Proportion Variable_description`, regex("therapy|treatment|administration|Outpatient intravenous therapy in past year", ignore_case = TRUE))&str_detect(`Proportion Variable_description`, regex("IV|intravenous", ignore_case = TRUE))) ~ "Prior IV therapy",
      str_detect(`Proportion Variable_description`, regex("male|women", ignore_case = TRUE)) ~ "Sex",
      `Proportion Variable_description` %in% c("men", "Men", "Sex - men") ~ "Sex",
      str_detect(`Proportion Variable_description`, regex("mortality|death|fatality|30-day outcome|Outcome - Dead", ignore_case = TRUE)) ~ "Patient outcomes, mortality",
      str_detect(`Proportion Variable_description`, regex("Recovered|Response|Overall survival|clinical cure", ignore_case = TRUE)) ~ "Patient outcomes, cure",
      str_detect(`Proportion Variable_description`, regex("Failure|7-day clinical treatment failure", ignore_case = TRUE)) ~ "Patient outcomes, treatment failure",
      str_detect(`Proportion Variable_description`, regex("complication", ignore_case = TRUE)) ~ "Patient outcomes, complications",
      str_detect(`Proportion Variable_description`, regex("race|ethnicity", ignore_case = TRUE)) ~ "Ethnicity",
      (str_detect(`Proportion Variable_description`, regex("recurrent", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("BSI", ignore_case = TRUE))) ~ "Prior infection",
      str_detect(`Proportion Variable_description`, regex("organism|species|Enterobacter|Serratia", ignore_case = TRUE)) ~ "Pathogen",
      str_detect(`Proportion Variable_description`, regex("Streptococcus spp.|Escherichia coli|Klebsiella|Enterococcus|Staphylococcus|MRSA|MSSA|E\\. faecalis|E\\. faecium|Pathogen|Subtype|Phylogenetic|genes|E\\.faecalis|A\\. baumannii|KP detection|Gram-negative|Gram-positive", ignore_case = TRUE)) ~ "Pathogen",
      str_detect(`Proportion Variable_description`, regex("charlson|Charlston|Elixhauser Comorbidity Score - 14+", ignore_case = TRUE)) ~ "Comorbidity score, high (Charlson >2;Elixhauser>13)",
      str_detect(`Proportion Variable_description`, regex("SOFA|APACHE|qSOFA|Severity|ICU|Critical|Fatal|Coma scale|pSOFA", ignore_case = TRUE)) ~ "Clinical severity",
      str_detect(`Proportion Variable_description`, regex("Temperature >= 38 degrees C|Fever|Respiratory|Pleural|Manifestations|Dyspnoea|Micturition syndrome|Clinical characteristics|Admission Diagnosis -", ignore_case = TRUE)) ~ "Clinical presentation, other than infection site",
      str_detect(`Proportion Variable_description`, regex("Region|Residence|Insurance|Income", ignore_case = TRUE)) ~ "Geography",
      str_detect(`Proportion Variable_description`, regex("ampicillin|amoxicillin|imipenem|ceftriaxone|cefotaxime|ceftazidim|cefepim|amikacin|gentamycin|cephalosporins|tigecycline|polymixin|cephalosporin|meropenem|carbapenem|vancomycin|fluoroquinolone|therapy|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide|vancomycin|polymyxin", ignore_case = TRUE)) ~ "Prior antibiotic exposure, specific choice",
      TRUE ~ "Other"
    ))
# after check of allocation, correcting some specific mislabelled categories
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Outcomes - Sepsis attributable mortality"] <- "Patient outcomes, mortality"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Stay in hemato-oncology wards"] <- "Prior hospitalisation"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Community infection - Previous antibiotic use within 90 days"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Community infection - Indwelling biliary drainage"] <- "Invasive procedures"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Outcome parameters - Appropriate therapy administered in <48 h"] <- "Duration to appropriate therapy (binary)"
# categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Effective antimicrobials within 48 hr"] <- "Duration to appropriate therapy (binary)"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Comorbid conditions - Positive A. baumannii blood culture after 72 h of admission"] <- "Hospital-acquired"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Underlying condition - Prior admissions > 2"] <- "Prior hospitalisation"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Underlying condition - Prior antibiotic therapy"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Underlying condition - Crude mortality rate 30 days"] <- "Patient outcomes, mortality"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Recent ventilator-associated pneumonia due to CRAB"] <- "Primary/specific infection site" # not sure. CHECK
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Comorbidity - Malnutrition"] <- "Comorbidities, underweight/malnutrition"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Baseline comorbidities - hematological"] <- "Comorbidities, cancer/hematologic"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Outcome parameters - Discharged to long-term care facilities"] <- "Other"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Clinical outcomes - Disseminated intravascular coagulation"] <- "Other" # CHECK
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Clinical outcomes - â‰¥ 10 days of hospital stay from culture to discharge"] <- "Patient outcomes, duration of hospitalisation or treatment"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Healthcare-Associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Infection type - Healthcare-associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Epidemiologic classification - Healthcare-associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Index culture >48 h"] <- "Hospital-acquired"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Unreasonable empirical treatment"] <- "Inappropriate empirical antibiotic treatment"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Assessment models - Appropriateness of empirical therapy, no"] <- "Inappropriate empirical antibiotic treatment"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Unnecessary use of carbapenems"] <- "Inappropriate empirical antibiotic treatment"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Carbapenem administration history"] <- "Prior antibiotic exposure, specific choice"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Received antimicorbials during previous month"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="LOS to the first positive culture - â‰¥20 days"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory_level2[grepl("delays of more than 72 h", categorical_df$`Proportion Variable_description`)] <- "Other"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Transition to oral therapy"] <- "Other"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="In appropriate empiric therapy"] <- "Inappropriate empirical antibiotic treatment"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Pre-infection healthcare interventions - Previous surgery (During the 30 days preceding infection onset)"] <- "Invasive procedures"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Previous treatments administered - Corticosteroids (During the 30 days preceding infection onset)"] <- "Prior corticosteroid use"
categorical_df$indicatorcategory_level2[categorical_df$`Proportion Variable_description`=="Previous treatments administered - Inadequate empirical antibiotic therapy (During the 30 days preceding infection onset)"] <- "Inappropriate empirical antibiotic treatment"


# all specific "healthcare-associated" checked, relabel remaining as non specified
categorical_df$indicatorcategory_level2[grepl("ealthcare-associated", categorical_df$`Proportion Variable_description`)==T|grepl("ealthcare associated", categorical_df$`Proportion Variable_description`)==T] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"


# indicator added both as 'yes' and 'no', or with multiple categories, of which some are more reference categories
categorical_df <- categorical_df[!grepl(" No$", categorical_df$`Proportion Variable_description`), ] # take all observations with a string value ending with " No"
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Shock - No")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Length of hospital stay before positive blood culture - <48h")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Length of hospital stay before positive blood culture - 2-14 days") # there is a category >14 days, this one is an intermediate. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Healthcare-associated condition - Hospitalization never or not within 1 yr") # there is a category hospi in last 90 days, this one is an intermediate. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Healthcare-associated condition - Hospitalization >180 day <= 1 yr") # there is a category hospi in last 90 days, this one is an intermediate. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Healthcare-associated condition - Hospitalization >90 <= 180 day") # there is a category hospi in last 90 days. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Beta-lactam or fluoroquinolone treatment - Timing of treatment - Never or not within 90 day") # there is a category treatment in last 30 days. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Beta-lactam or fluoroquinolone treatment - Timing of treatment - > 30 day, â‰¤ 90 day") # there is a category treatment in last 30 days, this one is an intermediate. CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Beta-lactam or fluoroquinolone treatment - Number of courses â‰¤ 90 day - 1 ") # overlap with other ABU indicator in same study (prior AMU in last 30 and 90 days).CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Beta-lactam or fluoroquinolone treatment - Number of courses â‰¤ 90 day - â‰¥ 2") # overlap with other ABU indicator in same study (prior AMU in last 30 and 90 days).CHECK
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Stage of haematological disease - Remission") # vs. active
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Laboratory examination - ANC < 500/mmc") # vs. < 100/mmc
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Laboratory examination - ANC < 500/mmc for at least 10d")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Admission - Non-Surgical")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Charlston index -  <3")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Charlson Co-morbidity Index â‰¥4") # same study also reports >/=3, which is used in other studies as well
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="McCabe Index - 1") # same study also reports 2 and 3. not even sure if needed to keep multiple
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Bacterial isolate - Other bacteria") # same study also reports 2 and 3. not even sure if needed to keep multiple
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Correct treatment")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Comorbidity - No comorbidity")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Site of infection - Primary bacteraemia") # is the reference category for site of infection
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Admission type - Information not available")
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Outcomes - 14-day mortality"|Study_ID!="#2216") # also reports 30 day mort, avoid double counting
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Outcome - Survival"|Study_ID!="#8208") # also reports mort, so alive is ref
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Outcome - Favorable (cure or improvement)"|Study_ID!="#4049") # also reports mort, so alive is ref
categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="history of previous invasive procedure or infection focus in the 3 months before IE onset") # too non specific. muoltiple indicator categories together


categorical_df <- categorical_df[!grepl("^Department-", categorical_df$`Proportion Variable_description`) & # many random departments, but unclear what the exposure was there
                                   !grepl("Inpatient service-", categorical_df$`Proportion Variable_description`) &
                                   !grepl("Isolates period -", categorical_df$`Proportion Variable_description`) &
                                   !grepl("Year enrolled -", categorical_df$`Proportion Variable_description`) &
                                   !grepl("Discharge status -", categorical_df$`Proportion Variable_description`),] # multiple categories, which overlap but are non specific for patient outcome

categorical_df <- categorical_df[!categorical_df$`Proportion Variable_description` %in% c("Age - 5 to 12", "Age - 13 to 18", "Age - 19 to 35", "Age - 36 to 50", "Age - 51 to 65",
                                                                                          "Age(years) - 40-59", "Age(years) - 18-39", "Age(years) - 60-79", "Age category - 40-49",
                                                                                          "Age category - 50-59", "Age category - 60-69", "Age category - 70-79", "Age - 18-39",
                                                                                          "Age - 40-59", "Age - 60-79"),]

categorical_df <- categorical_df %>% filter(`Proportion Variable_description`!="Age") # unspecified how age is categorical
categorical_df <- categorical_df %>% filter(grepl("Elixhauser",`Proportion Variable_description`)==F|indicatorcategory_level2!="Other") # high scores have been labelled, the low/reference ones out
categorical_df <- categorical_df %>% filter(grepl("LOS",`Proportion Variable_description`)==F|indicatorcategory_level2!="Other") # long LOS has been labelled, the low/reference ones out


table(categorical_df$indicatorcategory_level2)

check <- categorical_df %>% filter(grepl("Prior colonization|Prior infection",indicatorcategory_level2)==T) %>% select(`Proportion Variable_description`, indicatorcategory_level2, Study_ID)


# create two levels of indicators (general categories, then more specific)
categorical_df$indicatorcategory_level1 <- dplyr::case_when(
  categorical_df$indicatorcategory_level2 %in% c(
    "Healthcare associated, other than hospital-acquired or non specified if hospital",
    "Hospital-acquired", "Long-term care facility", "Prior hospitalisation", "Prior ICU stay",
    "Community-acquired"
  ) ~ "Healthcare exposure",

  categorical_df$indicatorcategory_level2 %in% c(
    "Prior colonization", "Prior infection"
  ) ~ "Prior colonization or infection",

  categorical_df$indicatorcategory_level2 %in% c(
    "Invasive procedures", "Prior IV therapy",
    "Transplant", "Blood transfusion"
  ) ~ "Invasive procedures",

  categorical_df$indicatorcategory_level2 %in% c(
    "Prior antibiotic exposure, specific choice",
    "Inappropriate empirical antibiotic treatment",
    "Prior antibiotic exposure, duration",
    "Prior antibiotic exposure, non specific",
    "Prior corticosteroid use", "Appropriate empirical antibiotic choice"
  ) ~ "Antibiotic exposure",

  categorical_df$indicatorcategory_level2 %in% c(
    "Comorbidities, cancer/hematologic", "Comorbidities, cardiovascular", "Comorbidity score, high (Charlson >2;Elixhauser>13)",
    "Comorbidities, cognitive/auto-immune/other", "Comorbidities, metabolic/liver/kidney/urological", "Comorbidities, respiratory",
    "Comorbidities, underweight/malnutrition"
  ) ~ "Comorbidities",

  categorical_df$indicatorcategory_level2 %in% c(
    "Clinical presentation, other than infection site", "Clinical severity", "Preterm birth/low birth weight"
    ) ~ "Clinical presentation",

  categorical_df$indicatorcategory_level2 %in% c("Primary/specific infection site") ~ "Primary/specific infection site",

  categorical_df$indicatorcategory_level2 %in% c(
    "Older age (cutoffs >/= 60 years)", "Young age (cutoffs </= 5 years)",
    "Ethnicity", "Sex", "Geography"
  ) ~ "Demographics",

  categorical_df$indicatorcategory_level2 %in% c(
    "Low blood values", "Biomarker positive"
  ) ~ "Biomedical results",

  categorical_df$indicatorcategory_level2 %in% c(
    "Pathogen", "Resistance profile", "Duration to appropriate therapy (binary)"
  ) ~ "Microbiological indicators",

  categorical_df$indicatorcategory_level2 %in% c(
    "Patient outcomes, cure", "Patient outcomes, mortality",
    "Patient outcomes, duration of hospitalisation or treatment",
    "Patient outcomes, treatment failure", "Patient outcomes, complications"
  ) ~ "Patient outcomes",
  TRUE ~ "Other")

# add level 2 for specific subgroups of interest that already have level 2
# primary infection site
categorical_df <- categorical_df %>%  mutate(indicatorcategory_level2 = case_when(
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("pyelonephritis|urinary tract|uti", ignore_case = TRUE)) ~ "Urinary tract",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("lung|respiratory|pneumonia|pulmonary infection|ventilator-associated|chest infection", ignore_case = TRUE)) ~ "Respiratory tract/pulmonary",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("abdominal|biliary|hepato|gastro", ignore_case = TRUE)) ~ "Intra-abdominal/hepatobiliary",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("skin|soft tissue|cellulitis|wound", ignore_case = TRUE)) ~ "Skin/soft tissue",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("bone|joint", ignore_case = TRUE)) ~ "Bone/joint",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("endocarditis", ignore_case = TRUE)) ~ "Endocarditis",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("catheter-associated|catheter-related|catheter associat|catheter related", ignore_case = TRUE)) ~ "IV catheter-related",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("mucosal barrier damage", ignore_case = TRUE)) ~ "Mucosal barrier injury",
  indicatorcategory_level2 == "Primary/specific infection site" & str_detect(`Proportion Variable_description`, regex("combined infection site|primary site|primary infection site|infection site|source of|focus", ignore_case = TRUE)) ~ "Mixed/unspecified site",
  TRUE ~ indicatorcategory_level2))

# check those not assigned to a level 2
check <- categorical_df %>% filter(categorical_df$indicatorcategory_level2=="Primary/specific infection site") %>% select(`Proportion Variable_description`)

# recode level2 for invasive procedures, since that is already level 1
categorical_df <- categorical_df %>%  mutate(indicatorcategory_level2 = case_when(
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("tracheal|tracheo|intubat|bronchoscopy|ventilator|mechanical ventilat|aspiration", ignore_case = TRUE)) ~ "Airway/respiratory procedures",
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("central line|central venous line|hemodialys|dialysis|blood purification", ignore_case = TRUE)) ~ "Central venous access/dialysis",
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("cannula|peripheral line", ignore_case = TRUE)) ~  "Peripheral venous access",
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("pacemaker|device|implant", ignore_case = TRUE)) ~ "Internal device / implant",
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("endoscopy|foley|catheter(?!.*central)|urinary|gastric tube|drain|tube|nutrition|parenteral nutrit|urological manipulation|genitourinary intervention|indwelling biliary drainage", ignore_case = TRUE)) ~
    "Gastrointestinal/urinary tubes or drains", # I added endoscopy here, but not sure if it should be. CHECK
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("surgery|surgical proced|operation|caesarian|cesarian|caesarean|thoracentesis|puncture|invasive", ignore_case = TRUE)) ~ "Surgery / Invasive procedure",
  indicatorcategory_level2 == "Invasive procedures" & str_detect(`Proportion Variable_description`, regex("invasive", ignore_case = TRUE)) ~ "Unspecified invasive procedure",
  TRUE ~ indicatorcategory_level2))

# rename level 2 of comorbidities, since level 1 has "Comorbidities"
categorical_df <- categorical_df %>%  mutate(
    indicatorcategory_level2 = case_when(
      indicatorcategory_level2 == "Comorbidities, cancer/hematologic" ~ "Cancer/hematologic",
      indicatorcategory_level2 == "Comorbidities, cardiovascular" ~ "Cardiovascular",
      indicatorcategory_level2 == "Comorbidity score, high (Charlson >2;Elixhauser>13)" ~ "High comorbidity score (Charlson >2; Elixhauser >13)",
      indicatorcategory_level2 == "Comorbidities, cognitive/auto-immune/other" ~ "Cognitive/auto-immune/other",
      indicatorcategory_level2 == "Comorbidities, metabolic/liver/kidney/urological" ~ "Metabolic/liver/kidney/urological",
      indicatorcategory_level2 == "Comorbidities, respiratory" ~ "Respiratory",
      indicatorcategory_level2 == "Comorbidities, underweight/malnutrition" ~ "Underweight/malnutrition",
      TRUE ~ indicatorcategory_level2))

# add level 3 for specific subgroups of interest that already have level 2
# antibiotic class exposed to - CHECK, some might still not be categorised
categorical_df <- categorical_df %>%  mutate(indicatorcategory_level3 = case_when(
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("tigecycline", ignore_case = TRUE)) ~ "Tigecycline",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("ceftriaxone|cefotaxime|ceftazidim|cefepim|cephalosporin|ceph|cefurox|cefazol", ignore_case = TRUE)) ~ "Cephems",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("carbapenem|meropenem|imipenem", ignore_case = TRUE)) ~ "Carbapenems",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("amikacin|gentamycin", ignore_case = TRUE)) ~ "Aminoglycosides",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("piperacillin|ampicillin|amoxicillin|beta-?lactam|penicillin", ignore_case = TRUE)) ~ "Beta-lactams",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("glycopeptide|vancomycin|linezolid", ignore_case = TRUE)) ~ "Glycopeptides/oxazolidinones",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("polymyxin|polymixin", ignore_case = TRUE)) ~ "Polymyxins",
      indicatorcategory_level2 == "Prior antibiotic exposure, specific choice" & str_detect(`Proportion Variable_description`, regex("fluoroquinolone|ciprofloxacin|quinolone", ignore_case = TRUE)) ~ "Fluoroquinolones",
      TRUE ~ NA_character_  ))
# regroup the classes of which there are <10 studies
categorical_df$indicatorcategory_level3[categorical_df$indicatorcategory_level3=="Polymyxins"] <- "Other classes"
categorical_df$indicatorcategory_level3[categorical_df$indicatorcategory_level3=="Aminoglycosides"] <- "Other classes"
categorical_df$indicatorcategory_level3[is.na(categorical_df$indicatorcategory_level3)&categorical_df$indicatorcategory_level2 == "Prior antibiotic exposure, specific choice"] <- "Other classes"
# the remaining "Other" are indeed sparadic other classes (macrolides, polymyxins,...)

# since measures of association are not reported by all studies, and those reported are in the separate 'predictors' part of the database, calculate crude odds ratios based on the reported counts exposed vs unexposed in the AMR and S groups

# check reference categories between studies, making sure the same are used between studies that are analysed together
# check indicator categories and if a reference is specified
ref <- categorical_df %>% group_by(indicatorcategory_level2, `Proportion Variable_description`, Study_ID) %>% summarise(n=n()) # STILL NEED TO ADD REFERENCE FOR OTHER CATEGORICAL VARIABLES THAT ARE NOT YES VS NO
# add a variable to indicate which value is used as the reference
categorical_df$ref[categorical_df$indicatorcategory_level2=="Sex"] <- "male"
categorical_df$ref[categorical_df$indicatorcategory_level2=="Sex"&grepl("female",categorical_df$`Proportion Variable_description`, ignore.case = T)] <- "female"

# reformat number variables (now often containing non numerical characters)
# is not a categorical variable
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="39.0 (27.6-55.8)") # mean economic cost in usd
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="834 (316-2506)") # mean economic cost in usd
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="974 (394-1779)") # mean economic cost in usd
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="2472 (986-4914)") # mean economic cost in usd
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="1075 (107-2397)") # mean economic cost in usd
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="114.65") # cerebrospinal fluid (CSF) (chlorine (CL)) (mmol/L) - MEDIAN
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="116.2") # cerebrospinal fluid (CSF) (chlorine (CL)) (mmol/L) - MEDIAN
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="2.0299999999999998") # cerebrospinal fluid (CSF) (Glucose (GLU)) (mmol/L) - MEDIAN
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="0.26") # PCT value
categorical_df <- categorical_df %>% filter(`Number Resistant_group_value`!="0/101") # ANC (neutrophils/ul)- >500 values don't make sense; the count in the comparator group is 19541

# correct values with characters in, after checking each
categorical_df$Resistant_group_tot_nb[categorical_df$`Number Resistant_group_value`=="98/101"] <- "101" # different denominator for this specific test done
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="98/101"] <- "98"
categorical_df$`Number Variable_description`[categorical_df$`Number Resistant_group_value`=="37/33"] <- "inborn (vs. outborn)" # the variable description distinguished inborn vs outborn
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="37/33"] <- "37" # the variable description distinguished inborn vs outborn
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="210/96"] <- "210" # the variable description distinguished inborn vs outborn
categorical_df$Resistant_group_tot_nb[categorical_df$`Number Resistant_group_value`=="2/101"] <- "101" # different denominator for this specific test done
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="2/101"] <- "2"
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="19419"] <- "5" # probably a typo. deducted the number based on the proportion reported (0.057*89)
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="not  mentioned"&categorical_df$`Proportion Resistant_group_value`=="19.2"] <- "59"
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="not  mentioned"&categorical_df$`Proportion Resistant_group_value`=="38.6"] <- "119"
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="not  mentioned"&categorical_df$`Proportion Resistant_group_value`=="56.2"] <- "174"
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="not  mentioned"&categorical_df$`Proportion Susceptible_comparator_group_value`=="7.4"] <- "67" # deducted the number based on the proportion reported
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="not  mentioned"&categorical_df$`Proportion Susceptible_comparator_group_value`=="22.8"] <- "207" # deducted the number based on the proportion reported
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="not  mentioned"&categorical_df$`Proportion Susceptible_comparator_group_value`=="32.6"] <- "296" # deducted the number based on the proportion reported
categorical_df$`Number Resistant_group_value`[categorical_df$`Number Resistant_group_value`=="not mentioned"&categorical_df$`Proportion Resistant_group_value`=="21.59"] <- "19"
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="not mentioned"&categorical_df$`Proportion Susceptible_comparator_group_value`=="11.76"] <- "16" # deducted the number based on the proportion reported
categorical_df$Resistant_group_tot_nb[categorical_df$Resistant_group_tot_nb=="91.77"&categorical_df$Study_ID=="#4740"] <- "87" # corrected based on check in the full text
categorical_df$Susceptible_group_tot_nb[categorical_df$Susceptible_group_tot_nb=="314.44"&categorical_df$Study_ID=="#4740"] <- "321" # corrected based on check in the full text
categorical_df$Susceptible_group_tot_nb[categorical_df$`Number Susceptible_comparator_group_value`=="43/53"] <- "53"
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$`Number Susceptible_comparator_group_value`=="43/53"] <- "43"

categorical_df$Resistant_group_tot_nb[is.na(categorical_df$Resistant_group_tot_nb)&categorical_df$Study_ID=="#3295"] <- "286" # looked up in the extraction table, under col 8 (Note)
categorical_df$Susceptible_group_tot_nb[is.na(categorical_df$Susceptible_group_tot_nb)&categorical_df$Study_ID=="#3295"] <- "409" # looked up in the extraction table, under col 8 (Note)
categorical_df$Resistant_group_tot_nb[is.na(categorical_df$Resistant_group_tot_nb)&categorical_df$Study_ID=="#2039"] <- "13" # looked up in the extraction table, entered by one extractor but not the second, therefore missing
categorical_df$Susceptible_group_tot_nb[is.na(categorical_df$Susceptible_group_tot_nb)&categorical_df$Study_ID=="#1684"] <- "543" # looked up in the extraction table, different values between extractors, and double checked in full paper

# also #4688 and #1412 have no `Number Susceptible_comparator_group_value`, because they were not reported in the article -> exclude from the analysis of measures of association?

# make numeric
categorical_df$`Number Resistant_group_value` <- as.numeric(categorical_df$`Number Resistant_group_value`)
categorical_df$`Number Susceptible_comparator_group_value` <- as.numeric(categorical_df$`Number Susceptible_comparator_group_value`)
categorical_df$Resistant_group_tot_nb <- as.numeric(categorical_df$Resistant_group_tot_nb)
categorical_df$Susceptible_group_tot_nb <- as.numeric(categorical_df$Susceptible_group_tot_nb)

table(categorical_df$Resistant_group_tot_nb, useNA = "always")
table(categorical_df$Susceptible_group_tot_nb, useNA = "always")
table(categorical_df$`Number Resistant_group_value`, useNA = "always")
table(categorical_df$`Number Susceptible_comparator_group_value`, useNA = "always")

# CHECK CHECK - the below need to be verified in the articles. the number is supposed to be a subset of the total
check <- categorical_df %>% filter(is.na(categorical_df$`Number Susceptible_comparator_group_value`)) %>% select(Study_ID, 33:61) # check those with missing counts
check2 <- categorical_df %>% filter(categorical_df$`Number Susceptible_comparator_group_value`>categorical_df$Susceptible_group_tot_nb) %>% select(Study_ID, 33:61) # check if the exposed group is less than the total group => else, need to verify
check3 <- categorical_df %>% filter(categorical_df$`Number Resistant_group_value`>categorical_df$Resistant_group_tot_nb) %>% select(Study_ID, 33:61) # check if the exposed group is less than the total group => else, need to verify
# mark those, so they can be excluded for now from the analysis
categorical_df$error[categorical_df$`Number Susceptible_comparator_group_value`>categorical_df$Susceptible_group_tot_nb] <- "error_in_exposed_or_total_counts_extracted"
categorical_df$error[categorical_df$`Number Resistant_group_value`>categorical_df$Resistant_group_tot_nb] <- "error_in_exposed_or_total_counts_extracted"
# check to see if this is because these are case-control studies
table(categorical_df$Study_design, categorical_df$error, useNA = "always")  # only ~10% in case control studies
# export a table to check the errors
check_count_errors <- categorical_df %>% filter(!is.na(error))
write_xlsx(check_count_errors, "check_count_errors.xlsx")

# make sure the same reference is used when comparing associations
# to make sure "male" is compared against "female" as the reference category (most frequent comparison):
# if sex is "female", replace the number of female resistant by the number of male resistance and the n female susceptible by male
categorical_df$`Number Resistant_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- categorical_df$Resistant_group_tot_nb[categorical_df$ref=="female"&!is.na(categorical_df$ref)] - categorical_df$`Number Resistant_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- categorical_df$Susceptible_group_tot_nb[categorical_df$ref=="female"&!is.na(categorical_df$ref)] - categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$indicatorcategory_level2[categorical_df$indicatorcategory_level2=="Sex"] <- "Male sex"

#KM - Output the list of indicators L1 x L2 with raw indicators table to share with WHO
#a.drop rows where indicator_l1 and l2 are empty
summary_cat_indicators <- categorical_df %>%
  filter(!is.na(indicatorcategory_level1)) %>%
  mutate(
    # tidy the original indicator a bit for nicer lists
    categorical_indicator_clean = str_squish(as.character(`Number Variable_description`))
  )

#b.frequency per l1
freq_cat_l1 <- summary_cat_indicators %>%
  group_by(indicatorcategory_level1) %>%
  summarise(freq_categorical_l1 = n(), .groups = "drop")

#c.summary per (l1, l2) pair - indicators list + l2 frequency
collapse_list_categorical <- function(x) {
  #unique, sorted, comma-separated string
  paste(sort(unique(x[!is.na(x) & x != ""])), collapse = ", ")
}

pairs_l1_l2 <- summary_cat_indicators %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2) %>%
  summarise(
    indicators_included = collapse_list_categorical(categorical_indicator_clean),
    frequency_category_l2 = n(),
    .groups = "drop"
  )

#d.attach the l1 frequency to each (l1,l2) row
summary_cat_indicators_out <- pairs_l1_l2 %>%
  left_join(freq_cat_l1, by = "indicatorcategory_level1") %>%
  arrange(indicatorcategory_level1, desc(frequency_category_l2))

summary_cat_indicators_out

#e.view and export
print(summary_cat_indicators_out, n = 20)
write_xlsx(
  list(summary_cat_indicators = summary_cat_indicators_out),
  "categorical_indicators_summary.xlsx"
)

#f.bar chart for categorical level_1
freq_cat_l1 <- freq_cat_l1 %>%
  arrange(desc(freq_categorical_l1)) %>%
  mutate(indicatorcategory_level1 =
           factor(indicatorcategory_level1,
                  levels = indicatorcategory_level1))

p_l1 <- ggplot(freq_cat_l1,
               aes(x = reorder(indicatorcategory_level1, freq_categorical_l1),  # asc order
                   y = freq_categorical_l1)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(x = "Level 1 category - Categorical indicators",
       y = "Count",
       title = "Categorical-indicators_bar-chart_categories (first level_Level1)") +
  theme_minimal(base_size = 12)

print(p_l1)
ggsave(filename = "categorical_indicators_bar-chart_level1.jpeg", p_l1, width = 9, height = 6, dpi = 300) #export

#g.heatmap for the top 20 categories - otherwise it does not show well
top_L2_n <- 20

top_l2 <- summary_cat_indicators %>%
  filter(!is.na(indicatorcategory_level2)) %>%
  count(indicatorcategory_level2, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_L2_n) %>%
  pull(indicatorcategory_level2)

heat_df <- summary_cat_indicators %>%
  mutate(L1 = indicatorcategory_level1,
         L2 = ifelse(is.na(indicatorcategory_level2),
                     "(Missing L2)",
                     ifelse(indicatorcategory_level2 %in% top_l2,
                            indicatorcategory_level2,
                            "Other (small)"))) %>%
  count(L1, L2, name = "count")

#order L1 by total count and L2 by overall count
l1_order <- heat_df %>%
  group_by(L1) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(L1)

l2_order <- heat_df %>%
  group_by(L2) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(L2)

heat_df <- heat_df %>%
  mutate(L1 = factor(L1, levels = l1_order),
         L2 = factor(L2, levels = l2_order))

p_heat <- ggplot(heat_df, aes(x = L2, y = L1, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), size = 3) +
  scale_fill_gradient(low = "#f0f0f0", high = "#08519c") +
  labs(x = "Level 2 (top & grouped)", y = "Level 1",
       fill = "Count",
       title = paste0("Categorical indicators - Counts by Level 1 × Level 2 (top ", top_L2_n, " L2 shown)")) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat)
ggsave(filename = "categorical_indicators_heatmap_categories_l1_l2.jpeg", p_heat, width = 12, height = 7, dpi = 300) #export

#h.interactive review table - categorical indicators
# Use already built 'summary_cont_indicators_out' (L1,L2, indicators list, and freq)
datatable(
  summary_cat_indicators_out,
  options = list(
    pageLength = 25,
    order = list(list(1, "asc"), list(4, "desc")),
    autoWidth = TRUE
  ),
  rownames = FALSE,
  caption = "Categorical_indicators_categories_Level 1 × Level 2_details-and-frequencies"
)

# 1/categorical_df$or[categorical_df$ref=="female"&!is.na(categorical_df$ref)] # instead of inversing the OR, now the counts are recalculated in the rows above
# categorical_df$ci_low[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- 1/categorical_df$ci_low[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
# categorical_df$ci_high[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- 1/categorical_df$ci_high[categorical_df$ref=="female"&!is.na(categorical_df$ref)]

# calculate crude odds ratio based on reported numbers
# categorical_df$or <- (categorical_df$`Number Resistant_group_value`/categorical_df$Resistant_group_tot_nb)/(categorical_df$`Number Susceptible_comparator_group_value`/categorical_df$Susceptible_group_tot_nb)
a <- categorical_df$`Number Resistant_group_value`
b <- categorical_df$Resistant_group_tot_nb - a
c <- categorical_df$`Number Susceptible_comparator_group_value`
d <- categorical_df$Susceptible_group_tot_nb - c

# odds ratio with 95% CI
categorical_df$or <- round((a * d) / (b * c),2)
se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
categorical_df$ci_low  <- round(exp(log(categorical_df$or) - 1.96 * se_log_or),2)
categorical_df$ci_high <- round(exp(log(categorical_df$or) + 1.96 * se_log_or),2)

# remove values of studies with errors in the extracted counts (e.g., nominator is higher than denominator)
categorical_df$or[!is.na(categorical_df$error)] <- NA
categorical_df$ci_low[!is.na(categorical_df$error)] <- NA
categorical_df$ci_high[!is.na(categorical_df$error)] <- NA

# combine lower an upper CI limits in a single variable
categorical_df$ci_label <- sprintf("%.1f-%.1f", categorical_df$ci_low, categorical_df$ci_high) # combining confidence intervals

# display all categorical indicators
catindicators <- categorical_df %>%
  filter(!is.na(`Proportion Variable_description`)) %>%
  select(`Proportion Variable_description`, indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3, ref, or, ci_low, ci_high, studypop, highrisk, amr, pathogen_antibiotic_combination)  %>%
  distinct()
catindicators
write_xlsx(catindicators, "catindicators.xlsx")
# display all "other"
othercatindicators <- categorical_df %>%
  filter(!is.na(`Proportion Variable_description`)&indicatorcategory_level2=="Other") %>%
  select(`Proportion Variable_description`, indicatorcategory_level2, ref, or, ci_low, ci_high, studypop, highrisk, amr, pathogen_antibiotic_combination)  %>%
  distinct()
othercatindicators
write_xlsx(othercatindicators, "othercatindicators.xlsx")

#### 1. DESCRIPTION OF STUDIES ####
# 1.1 count of studies & designs
count(df)
table(df$analysisdesign_simplified, useNA = "always")

# 1.2 publication year
table(df$Publication_year, useNA = "always")
hist(df$Publication_year, breaks = seq(2008.5, 2024.5, by = 1),  # ensures one bin per year
     xlab = "Publication Year",
     ylab = "Number of studies",
     col = "lightblue",
     border = "white",
     xlim = c(2009, 2024),
     xaxt = "n")
axis(1, at = 2009:2024, labels = 2009:2024)

# 1.3 create a map with the frequency of studies by country
countries <- df %>%
  select(Study_country) %>%
  mutate(Study_country = str_split(Study_country, ",|;")) %>%  # split multi-country entries into separate rows
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
    fill = "n studies" ) +
  scale_fill_distiller(
  palette = "YlOrRd",  # yellow → orange → red
  direction = 1,
  na.value = "grey90")
ggsave(filename = "map.jpeg",  width = 8, height = 5, dpi = 300)
# income levels
incomesumm <- df %>% group_by(incomegroup) %>% summarise(n=n()) %>% mutate(pct = round(100*n/sum(n),1))
incomesumm

# 1.4 level of healthcare
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

df_summary <- df_summary %>%
  mutate(facilitylevel = reorder(facilitylevel, n))

ggplot(df_summary, aes(x = facilitylevel, y = n, fill = facilitylevel)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Tertiary" = "#555555",
    "Primary" = "#00008B",
    "Secondary" = "#8B0000",
    "Unknown/unclear" = "#006400",
    "Mixed/Other" = "#999999"
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
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 12),   
    axis.text.y = element_text(size = 12),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  
  )
  
ggsave(filename = "bar_chart_healthcare_level.jpeg",  width = 8, height = 5, dpi = 300)

# 1.5 patient population - highrisk
highrisk_summary <- df_long %>%
  count(highrisk, sort = TRUE) %>% 
mutate(pct = n / sum(n) * 100) 

highrisk_summary

my_colors <- c(
  "high risk" = "#8B0000",
  "mixed or unspecified" = "#999999"
)

ggplot(highrisk_summary, aes(x = fct_reorder(highrisk, n), y = n, fill = highrisk)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),    
            hjust = -0.1, size = 4) +  
  coord_flip() +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "popuation type",
    y = "Number",
    title = "Distribution of type of population"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(highrisk_summary$n) * 1.15)


ggsave(filename = "bar_chart_highrisk.jpeg",  width = 8, height = 5, dpi = 300)

#age group - study population

studypop_levels <- c(
  "Neonates",
  "Pediatrics incl. neonates",
  "Pediatrics",
  "Adults & pediatrics",
  "Adults",
  "All ages or not specified"
)

studypop_summary <- df_long %>%
  mutate(
    studypop = str_trim(studypop),
    studypop = factor(studypop, levels = studypop_levels)
  ) %>%
  count(studypop) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(studypop_summary, aes(x = studypop, y = n, fill = studypop)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Adults" = "#191970",
    "Pediatrics" = "#005AB5",
    "Neonates" = "#333333",
    "Adults & pediatrics" = "#006400",
    "Pediatrics incl. neonates" = "#CCCCCC",
    "All ages or not specified" = "#8B0000"
  )) +
  labs(
    title = "Studies by age group",
    x = NULL,
    y = "Number of studies",
    caption = "*If multiple patient populations are included, each subpopulation is counted"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(studypop_summary$n) * 1.15)

ggsave(filename = "bar_chart_study_age_group.jpeg",  width = 8, height = 5, dpi = 300)

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
ggsave(filename = "bar_chart_bacterial_isolates_analysed.jpeg",  width = 8, height = 5, dpi = 300)

# summarize AMR profiles
counts <- df_long %>%
  count(amr, sort = TRUE)
counts
ggplot(counts, aes(x = fct_reorder(amr, n), y = n)) +
  geom_col(fill = "#800000") +
  coord_flip() +
  labs(
    x = "Resistance profile",
    y = "Number of analyses",
    title = "Distribution of reported AMR profiles in the analysis"
  ) +
  theme_minimal(base_size = 13)
ggsave(filename = "bar_chart_AMR_profiles.jpeg",  width = 8, height = 5, dpi = 300)

# summarize pathogen-antibiotic combination

pathogen_ab <- df_long %>%
  count(pathogen_antibiotic_combination, sort = TRUE) %>% 
mutate(pct = n / sum(n) * 100)

pathogen_ab

my_colors <- c(
  "other/combination of multiple pathogens" = "#8B0000",
  "C3G-resistant Enterobacterales" = "#00008B",
  "carbapenem-resistant Enterobacterales" = "#006400",
  "methicillin-resistant S. aureus" = "#555555",
  "carbapenem-resistant A.baumanii" = "#228B22",
  "vancomycin-resistant Enterococci" = "#A52A2A",
  "carbapenem-resistant P.aeruginosa" = "#191970",
  "penicillin-resistant S.pneumoniae" = "#999999"
)

ggplot(pathogen_ab, aes(x = fct_reorder(pathogen_antibiotic_combination, n), y = n, fill = pathogen_antibiotic_combination)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Resistance profile",
    y = "Number of analyses",
    title = "Distribution of reported pathogen - \nAMR profiles in the analysis"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(studypop_summary$n) * 1.15)

ggsave(filename = "bar_chart_AMR_profiles.jpeg",  width = 8, height = 5, dpi = 300)



#### 2. SUMMARY OF PROXY INDICATORS ####
# 2.1. CONTINUOUS VARIABLES
# count continuous indicators reported
summary_num_indicators <- continuous_df %>%
  filter(!is.na(`resistant_group variable`)) %>%
  select(`resistant_group variable`) %>%
  summarise(n=n())
summary_num_indicators # 484 indicators reported
# for numerical variables, summarize the number of analyses they have been reported in
summary_indicator_frequencies <- continuous_dfshort %>%
  group_by(indicatorcategory_level2) %>%
  filter(!is.na(`resistant_group p-value`)) %>%
  summarise(n=n())
summary_indicator_frequencies

# 2.2 CATEGORICAL VARIABLES
# exclude the microbiological indicators, since that was not the aim of the analysis
categorical_df <- categorical_df %>% filter(indicatorcategory_level1!="Microbiological indicators")

# count categorical indicators reported
length(unique(categorical_df$`Number Variable_description`)) # 2157 cat indicators reported, `Number Variable_description` and `Proportion Variable_description` are the same
length(unique(categorical_df$`Proportion Variable_description`))

# summarize the reported indicators, overall
categorical_summary <- categorical_df %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies = n_distinct(Study_ID),
    exposed_R = round(sum(`Number Resistant_group_value`, na.rm = TRUE),0),
    total_R = sum(Resistant_group_tot_nb, na.rm = TRUE),
    exposed_S = round(sum(`Number Susceptible_comparator_group_value`, na.rm = TRUE),0),
    total_S = sum(Susceptible_group_tot_nb, na.rm = TRUE),
    median_or = round(median(or, na.rm = TRUE),2),
    q25_or = round(quantile(or, 0.25, na.rm = TRUE),2),               # 25th percentile
    q75_or = round(quantile(or, 0.75, na.rm = TRUE),2),                # 75th percentile
  ) %>%
  ungroup()
#reorder so the most frequent are on top
categorical_summary <- categorical_summary %>%
  group_by(indicatorcategory_level1, indicatorcategory_level3) %>%
  arrange(desc(n_studies), .by_group = TRUE) %>%
  ungroup()
categorical_summary
# keep just once the label of the level 1 categories
categorical_summary <- categorical_summary %>% mutate(indicatorcategory_level1 = ifelse(
  duplicated(indicatorcategory_level1), "", indicatorcategory_level1)) %>%
  ungroup()
# export to a spreadsheet
write_xlsx(categorical_summary, "categorical_summary.xlsx")

# present in a heatmap-style table
categorical_summary$indicatorcategory_level3[is.na(categorical_summary$indicatorcategory_level3)] <- ""  #replace NAs by blanks
summarytable_cat_indicators <- categorical_summary %>%
  gt() %>%
  data_color(
    columns = n_studies,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(1, 125)
    )
  ) %>%
  data_color(
    columns = median_or,
    colors = function(x) {
      sapply(x, function(v) {
        if (v <= 1) {
          scales::col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          scales::col_numeric(c("white", "darkred"), domain = c(1, 13))(v)
        }
      })
    }
  )
summarytable_cat_indicators <- summarytable_cat_indicators %>%
  cols_label(
    `indicatorcategory_level1` = "",
    `indicatorcategory_level2` = "",
    `indicatorcategory_level3` = "",
    n_studies = "n studies",
    `exposed_R` = "exposed resistant",
    total_R = "total resistant",
    exposed_S = "exposed susceptible",
    `total_S` = "total suscep",
    median_or = "median OR")

gtsave(summarytable_cat_indicators, "summarytable_cat_indicators.docx")
gtsave(summarytable_cat_indicators, "summarytable_cat_indicators.html")  # as HTML to open in browser

# with SUBGROUP analyses
# in high risk populations
# redo summary overall, then in the high risk group only
categorical_summary <- categorical_df %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies = n_distinct(Study_ID),
    exposed_R = round(sum(`Number Resistant_group_value`, na.rm = TRUE),0),
    total_R = sum(Resistant_group_tot_nb, na.rm = TRUE),
    exposed_S = round(sum(`Number Susceptible_comparator_group_value`, na.rm = TRUE),0),
    total_S = sum(Susceptible_group_tot_nb, na.rm = TRUE),
    median_or = round(median(or, na.rm = TRUE),2),
    q25_or = round(quantile(or, 0.25, na.rm = TRUE),2),               # 25th percentile
    q75_or = round(quantile(or, 0.75, na.rm = TRUE),2),                # 75th percentile
  ) %>%
  ungroup()
categorical_summary <- categorical_summary %>%
  group_by(indicatorcategory_level1, indicatorcategory_level3) %>%
  arrange(desc(n_studies), .by_group = TRUE) %>%
  ungroup()
highriskcategorical_summary <- categorical_df %>%
  filter(highrisk=="high risk") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_highrisk = n_distinct(Study_ID),
    median_or_highrisk = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge high risk with the overall table
highriskcategorical_summary <- highriskcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(categorical_summary, highriskcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# key pathogen-antibiotic combinations
# C3G-R Enterobacterales
C3GEcategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="C3G-resistant Enterobacterales") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_c3ge = n_distinct(Study_ID),
    median_or_c3ge = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
C3GEcategorical_summary <- C3GEcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, C3GEcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# CR Enterobacterales
CREcategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="carbapenem-resistant Enterobacterales") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_cre = n_distinct(Study_ID),
    median_or_cre = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
CREcategorical_summary <- CREcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, CREcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# MRSA
MRSAcategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="methicillin-resistant S. aureus") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_mrsa = n_distinct(Study_ID),
    median_or_mrsa = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
MRSAcategorical_summary <- MRSAcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, MRSAcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# CRAB
CRABcategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="carbapenem-resistant A.baumanii") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_crab = n_distinct(Study_ID),
    median_or_crab = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
CRABcategorical_summary <- CRABcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, CRABcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# vancomycin-resistant Enterococci
VREcategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="vancomycin-resistant Enterococci") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_vre = n_distinct(Study_ID),
    median_or_vre = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
VREcategorical_summary <- VREcategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, VREcategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))

# other pathogen combinations
otherpathogenscategorical_summary <- categorical_df %>%
  filter(pathogen_antibiotic_combination=="other/combination of multiple pathogens" | pathogen_antibiotic_combination=="penicillin-resistant S.pneumoniae") %>%
  group_by(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3) %>%
  summarise(
    n_studies_other = n_distinct(Study_ID),                           
    median_or_other = round(median(or, na.rm = TRUE),2)) %>%
  ungroup()
# merge with the overall table
otherpathogenscategorical_summary <- otherpathogenscategorical_summary %>% select(-indicatorcategory_level1)
subgroups_categorical_summary <- left_join(subgroups_categorical_summary, otherpathogenscategorical_summary, by = c("indicatorcategory_level2", "indicatorcategory_level3"))


# one HTML table with overall and subgroup analyses
#reorder so the most frequent are on top
subgroups_categorical_summary <- subgroups_categorical_summary %>%
  group_by(indicatorcategory_level1, indicatorcategory_level3) %>%
  arrange(desc(n_studies), .by_group = TRUE) %>%
  ungroup()
# keep just once the label of the level 1 categories
subgroups_categorical_summary <- subgroups_categorical_summary %>% mutate(indicatorcategory_level1 = ifelse(
  duplicated(indicatorcategory_level1), "", indicatorcategory_level1)) %>%
  ungroup()
subgroups_categorical_summary
# take out NA or values that mess up lay out
subgroups_categorical_summary$indicatorcategory_level3[is.na(subgroups_categorical_summary$indicatorcategory_level3)] <- ""  #replace NAs by blanks
subgroups_categorical_summary$n_studies_highrisk[is.na(subgroups_categorical_summary$n_studies_highrisk)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_c3ge[is.na(subgroups_categorical_summary$n_studies_c3ge)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_cre[is.na(subgroups_categorical_summary$n_studies_cre)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_mrsa[is.na(subgroups_categorical_summary$n_studies_mrsa)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_crab[is.na(subgroups_categorical_summary$n_studies_crab)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_vre[is.na(subgroups_categorical_summary$n_studies_vre)] <- 0  #replace NAs by 0
subgroups_categorical_summary$n_studies_other[is.na(subgroups_categorical_summary$n_studies_other)] <- 0  #replace NAs by 0
subgroups_categorical_summary$median_or_cre[is.infinite(subgroups_categorical_summary$median_or_cre)] <- NA # replace Inf values by NA

subgroup_summarytable_cat_indicators <- subgroups_categorical_summary %>%
  gt() %>%
  data_color(
    columns = n_studies,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(1, 125))) %>%
  data_color(
    columns = median_or,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 13))(v)
        }})}) %>%
  data_color(
    columns = n_studies_highrisk,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 39))) %>%
  data_color(
    columns = median_or_highrisk,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 19))(v)
        }})}) %>%
  data_color(
    columns = n_studies_c3ge,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 43))) %>%
  data_color(
    columns = median_or_c3ge,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 23))(v)
        }})}) %>%
  data_color(
    columns = n_studies_cre,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 26))) %>%
  data_color(
    columns = median_or_cre,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 48))(v)
        }})}) %>%
  data_color(
    columns = n_studies_mrsa,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 19))) %>%
  data_color(
    columns = median_or_mrsa,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 15))(v)
        }})}) %>%
  data_color(
    columns = n_studies_crab,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 8))) %>%
  data_color(
    columns = median_or_crab,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 51))(v)
        }})}) %>%
  data_color(
    columns = n_studies_vre,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 10))) %>%
  data_color(
    columns = median_or_vre,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 13))(v)
        }})}) %>%
  data_color(
    columns = n_studies_other,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(0, 34))) %>%
  data_color(
    columns = median_or_other,
    colors = function(x) {
      sapply(x, function(v) {
        if (is.na(v)) {
          "white"  # make NA values white
        } else if (v <= 1) {
          col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          col_numeric(c("white", "darkred"), domain = c(1, 20))(v)
        }})}) %>%
  tab_spanner(
    label = "All included studies",
    columns = vars(n_studies, exposed_R, total_R, exposed_S, total_S, median_or, q25_or, q75_or)) %>%
  tab_spanner(
    label = "Indicators",
    columns = vars(indicatorcategory_level1, indicatorcategory_level2, indicatorcategory_level3))  %>%
  tab_spanner(
    label = "High risk patients",
    columns = vars(n_studies_highrisk, median_or_highrisk))  %>%
  tab_spanner(
    label = "C3G-resistant Enterobacterales",
    columns = vars(n_studies_c3ge, median_or_c3ge))    %>%
  tab_spanner(
    label = "carbapenem-resistant Enterobacterales",
    columns = vars(n_studies_cre, median_or_cre))    %>%
  tab_spanner(
    label = "methicillin-resistant S. aureus",
    columns = vars(n_studies_mrsa, median_or_mrsa))    %>%
  tab_spanner(
    label = "carbapenem-resistant A.baumanii",
    columns = vars(n_studies_crab, median_or_crab))    %>%
  tab_spanner(
    label = "vancomycin-resistant Enterococci",
<<<<<<< HEAD
    columns = vars(n_studies_vre, median_or_vre))      %>%
  tab_spanner(
    label = "other pathogen combinations",
    columns = vars(n_studies_other, median_or_other))   
=======
    columns = vars(n_studies_vre, median_or_vre))
>>>>>>> c803724 (save local work before rebase)

# rename colnames
subgroup_summarytable_cat_indicators <- subgroup_summarytable_cat_indicators %>%
  cols_label(
    `indicatorcategory_level1` = "",
    `indicatorcategory_level2` = "",
    `indicatorcategory_level3` = "",
    n_studies = "n studies",
    `exposed_R` = "exposed resistant",
    total_R = "total resistant",
    exposed_S = "exposed susceptible",
    `total_S` = "total suscep",
    median_or = "median OR",
    n_studies_highrisk = "n studies",
    median_or_highrisk = "median OR",
    n_studies_c3ge     = "n studies",
    median_or_c3ge     = "median OR",
    n_studies_cre      = "n studies",
    median_or_cre      = "median OR",
    n_studies_mrsa     = "n studies",
    median_or_mrsa     = "median OR",
    n_studies_crab     = "n studies",
    median_or_crab     = "median OR",
    n_studies_vre      = "n studies",
    median_or_vre      = "median OR",
    n_studies_other     = "n studies",
    median_or_other     = "median OR")

#export in large html or word table
gtsave(subgroup_summarytable_cat_indicators, "subgroup_summarytable_cat_indicators.docx")
gtsave(subgroup_summarytable_cat_indicators, "subgroup_summarytable_cat_indicators.html")  # as HTML to open in browser

# visualize all odds ratios reported by studies on a plot, excluding those with a missing value
# first remove extreme values of values with a
categorical_df_or <- categorical_df %>% filter(!is.na(or), !is.na(ci_low), !is.na(ci_high), or > 0, ci_low > 0, ci_high > 0, or>0.1 & or <20)
scale_y_log10(labels = scales::label_number())
orplot <- ggplot(categorical_df_or, aes(x = reorder(Study_ID, or), y = or, color = amr)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_y_log10(
    labels = scales::label_number(accuracy = 0.1),  # show decimals, not exponents, even if scale is log transformed
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)) +
  labs(
    x = "",
    y = "Odds Ratio (log scale)",
    title = "Odds Ratios with 95% Confidence Intervals") +
  theme_minimal(base_size = 13) +
  facet_grid(rows = vars(indicatorcategory_level2), scales = "free_y", space = "free_y") +
  theme(strip.text.y.left = element_text(angle = 90, hjust = 1, face = "bold"))
orplot
ggsave(orplot, filename = "OR_summary.jpeg",  width = 12, height = 49, dpi = 250)

# subset only studies looking at sex
categorical_df_or_sex <- categorical_df_or %>% filter(indicatorcategory_level2=="Sex")
ggplot(categorical_df_or_sex, aes(x = reorder(Study_ID, or), y = or, color = as.factor(amr))) +
  # geom_point(size = 3) +
  geom_beeswarm(size = 3) +  # spreads points if they are overlapping
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_y_log10(
    labels = scales::label_number(accuracy = 0.1),  # show decimals, not exponents, even if scale is log transformed
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)) +
  scale_color_discrete(name = "AMR") +
  labs(
    x = "",
    y = "Odds Ratio (log scale)",
    title = "Odds Ratios with 95% Confidence Intervals",
    color = "AMR") +
  theme_minimal(base_size = 13)
