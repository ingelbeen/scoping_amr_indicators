###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# install/load packages
pacman::p_load(readxl, writexl, lubridate, haven, dplyr, tidyr, stringr, countrycode, ggplot2, forcats, rnaturalearth, rnaturalearthdata, RColorBrewer, ggbeeswarm)

#### 0. IMPORT/CLEAN DATA #### 
df <- read_excel("20250830 WHO_Extracted-data_Consensed_review_457022_20250830231950.xlsx")
# see that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."
# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)
# remove all empty columns
df <- df %>%  select(where(~ any(!is.na(.) & . != "")))
# some variable names have twice the '_n' that refers to the nth proxy indicator in that article. keep only the '_n' at the end of the column name
names(df) <- gsub("(?<=total)_[0-9]+", "", names(df), perl = TRUE)
names(df) <- gsub("resistant_20 ", "resistant_group ", names(df))

# TO CHECK: should analyses which analyze another outcome than AMR (mortality, cost) be reported?
table(df$Outcome_measure_repoted)

# excluding analyses that do not analyze AMR as an outcome measure
df <- df %>% filter(str_detect(Outcome_measure_repoted, regex("AMR \\(ie development/ incidence/ emergence / change in AMR\\)", ignore_case = TRUE)))

# we need three databases for the planned analyses: 
# 1) one with one row/observation per study, to summarize study characteristics -> df
# 2) one with a row per analysis (i.e, comparison AMR vs susceptible) done, to report on the associations measured -> df_long with one row per comparison (so-called 'model')
# 3) one with a row per variable of interest (indicator/predictor) reported, to summarize proxy indicators -> three different df_longer: one numeric, one categorical, one ?

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

# clean resistance profiles & group resistance profiles in a variable amr
table(df_long$Resistant_grp_definition, useNA = "always")
# replace non specific 'resistance' by other variable values that have the specific resistance profile studied
df_long <- df_long %>%
  mutate(
    Resistant_grp_definition = case_when(
      Resistant_grp_definition %in% c("AMR", "Resistant", "Resistance +", "MRO", "MDRO", "Resistance to First-line Antibiotics") ~ 
        coalesce(AMR_mechanism_1, AMR_mechanism_2, Resistant_grp_definition),
      TRUE ~ Resistant_grp_definition))
# resist_models <- as.data.frame(table(df_long$Resistant_grp_definition, useNA = "always"))
# write.table(resist_models, "resist_models.txt")

df_long <- df_long %>%  mutate(amr = case_when(
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
table(df_long$amr)

# check columns
print(colnames(df_long), max = 1900)

# create an even longer df, with one row per variable/exposure of interest reported
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
df_long <- df_long %>% mutate(across(contains("comparator_group median_"), as.character))
df_long <- df_long %>% mutate(across(contains("compartor_group SD_"), as.character))

# DESCRIPTIVE_continuous_proxy-indicators -> exclude columns beyond the first proxy-indicators part of the data extraction table (up to the notes of proxy indicator 20)
continuous_df <- df_long %>% 
  select(1:393, amr, Model, Resistant_grp_definition, Resistant_group_tot_nb,
         Susceptible_group_definition, Susceptible_group_tot_nb)

# reshape continuous_df to into a longer format, in which all variables ending with _1, _2, up to _20 (the numerical indicators) are brought together in the same variable, so that for each number there is one row
continuous_df <- continuous_df %>%  pivot_longer(
    cols = matches("_(?:[1-9]|1[0-9]|20)$"),   # matches _1 to _20
    names_to = c(".value", "set"),             # .value keeps base var names
    names_pattern = "(.*)_(\\d+)$")
colnames(continuous_df)

# get rid of empty rows (per study model, up to 20 indicators reported, but usually les, so many rows are empty)
continuous_df <- continuous_df %>% filter(!is.na(`resistant_group definition`))

# create categories to group reported indicators -> continuous indicators reported in "resistant_group variable"
continuous_df <- continuous_df %>%  mutate(indicatorcategory = case_when(
  str_detect(`resistant_group variable`, regex("age.*years", ignore_case = TRUE)) ~ "Age",
  str_detect(`resistant_group variable`, regex("gestation|birthweight|crib", ignore_case = TRUE)) ~ "Preterm birth/low birth weight",
  str_detect(`resistant_group variable`, regex("apache|mccabe|sofa|pitt|pneumonia severity index|psis|saps|Severity grade", ignore_case = TRUE)) ~ "Clinical severity score",
  #    str_detect(`resistant_group variable`, regex("hospital stay", ignore_case = TRUE)) ~ "Prior hospital stay (categorical)",
  str_detect(`resistant_group variable`, regex("duration of hospitalization prior to having bacteremia|hospital stay.*days|Length of stay|LOS,days|Length of hospital|Time from admission|Length of hospital stay, total days|Days of admission before infection", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)",
  str_detect(`resistant_group variable`, regex("Time at risk|Days from admission to positive culture|Days from hospital admission to BSI|Total LOS|Duration of time from hospital admission to positive blood culture", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)",
  str_detect(`resistant_group variable`, regex("LOS in preceding yr|Total days of hospitalization in the 6 months prior to current hospitalization|Admission days prior to index culture|Index hospital stay|Sequential time to positivity|Time to positivity ratio", ignore_case = TRUE)) ~ "Duration hospital stay before BSI (ordinal)", # double check Sequential time to positivity and Time to positivity ratio
  str_detect(`resistant_group variable`, regex("ICU", ignore_case = TRUE)) ~ "Prior ICU stay (categorical)",
  str_detect(`resistant_group variable`, regex("ICU stay days", ignore_case = TRUE)) ~ "Prior ICU stay (ordinal)",
  str_detect(`resistant_group variable`, regex("Quantitative indices of antibiotic usage|Total antibiotic treatment|Antibiotic-days|Duration of exposure to antimicrobial agent, days", ignore_case = TRUE)) ~ "Antibiotic exposure: duration (numeric)", # check Total antibiotic treatment
  str_detect(`resistant_group variable`, regex("DDDs|Days of extended-spectrum|Days of third-|Days of carbapenem|Days of aminoglyc|Total days of antibiotic", ignore_case = TRUE)) ~ "Antibiotic exposure: duration (numeric)", # might have to break up between dose (daptomycin) and duration, also check Total antibiotic treatment
  str_detect(`resistant_group variable`, regex("number of different antibiotics used|no. of prior antibiotics", ignore_case = TRUE)) ~ "Antibiotic exposure: different antibiotics (numeric)", # might have to break up between dose (daptomycin) and duration, also check Total antibiotic treatment
  str_detect(`resistant_group variable`, regex("Therapy with antibiotics prior 30 days of infection - |Types of antibiotics", ignore_case = TRUE)) ~ "Antibiotic exposure (categorical)",
  str_detect(`resistant_group variable`, regex("LOS from culture to discharge|Survival time|time to first negative blood culture|Survival|LOS after bacteremia|antibiotic administration postculture", ignore_case = TRUE)) ~ "Patient outcomes", # still check if survival is defined as time until death or time until discharge 
  `resistant_group variable`== "Hospital days" ~ "Patient outcomes", # there's a note that states it is likely total admission, and not just prior or after BSI
  str_detect(`resistant_group variable`, regex("charlson|absi|no. of comorbidities", ignore_case = TRUE)) ~ "Comorbidity score",
  str_detect(`resistant_group variable`, regex("absi", ignore_case = TRUE)) ~ "Burn severity",
  str_detect(`resistant_group variable`, regex("ntiss| feeding tube", ignore_case = TRUE)) ~ "Invasive procedures",
  str_detect(`resistant_group variable`, regex("temperatu|blood pressure|apgar", ignore_case = TRUE)) ~ "Vital signs", # apgar score is largely based on vital signs
  str_detect(`resistant_group variable`, regex("creatinine|bilirubin|cholinest|total protein|albumin|LDH|CKMB|urea nitrogen|uric acid", ignore_case = TRUE)) ~ "Kidney/liver lab values",
  str_detect(`resistant_group variable`, regex("monocyte|neutropenia|wbc|hemoglobin|neutrophil|platelet|International normalized ratio|Hb|Haematocr", ignore_case = TRUE)) ~ "Blood lab values",
  str_detect(`resistant_group variable`, regex("tnf|procalciton|crp", ignore_case = TRUE)) ~ "Inflammatory lab values",
  str_detect(`resistant_group variable`, regex("blood transfusion", ignore_case = TRUE)) ~ "Blood transfusion",
  str_detect(`resistant_group variable`, regex("comorbidity|diabetes|cirrhosis|hypertens|Liver Disease", ignore_case = TRUE)) ~ "NCD",
  str_detect(`resistant_group variable`, regex("cost|economic|burden|usd|eur|cny|sgd|jpy", ignore_case = TRUE)) ~ "Costs / economic",
  str_detect(`resistant_group variable`, regex("days to active therapy|duration from bacteremia to receiving appropriate antibiotic|Hours to appropriate therapy", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
  str_detect(`resistant_group variable`, regex("Time to appropriate therapy|Time to adequate antibiotic therapy|Overall time to first dose of appropriate antibiotic therapy", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
  str_detect(`resistant_group variable`, regex("Hours to active antibiotic therapy|Time to microbiologically appropriate antibiotic therapy|no. of days to active", ignore_case = TRUE)) ~ "Duration to appropriate therapy (ordinal)",
  # for the remaining (which could not be assigned to one of the groups, or were wrongly categorised), enter the exact string 
  `resistant_group variable` %in% c(
    "Days of hospital stay", # to check if as outcome or as exposure
    "hospital stay", # to check if as outcome or as exposure
    "Overall - Hospital days before bacteremia",
    "Hospital days before bacteremia",
    "days hospitalization", # to check if as outcome or as exposure
    "duration of hospitalization (days)",
    "LOS prior to isolation of GNB, days",
    "LOS before blood culture (days)",
    "Number of hospitalization days in the 3 months before bacteremia",
    "Overall admission, days", # to check if as outcome or as exposure
    "Time between hospital admission and BSI onset (days)",
    "time from hospitalisation to KPBSI (days)",
    "Admission days till bacteraemia",
    "Duration of hospitalization before bacteremia (days)",
    "no. of days of hospital residency prior to\r\nculture"
  ) ~ "Duration hospital stay before BSI (ordinal)",
  `resistant_group variable` %in% c(
    "no. of days to active therapy",
    "no. of days to active\r\ntherapy ",
    "Days to appropriate therapy",
    "Appropriate antimicrobial therapy within 2 days",
    "Appropriate antimicrobial therapy within 3 days",
    "Clinical course and outcomes - Time to appropriate treatment in days"
  ) ~ "Duration to appropriate therapy (ordinal)",
  `resistant_group variable` %in% c(
    "Prior no. of days of antibiotic therapy",
    "Patients with previous antibiotic treatment - Days on antimicrobial treatment",
    "Exposure to antibiotics-Antibiotic days",
    "Fluoroquinolone therapy (days)",
    "Piperacillin-tazobactam (days)",
    "Meropenem (days)",
    "Vancomycin (days)",
    "duration of antibiotic therapy (days)",
    "Duration of previous antibiotic use (days)",
    "Previous antibiotic duration (days)",
    "Duration of therapy (days)",
    "Combined treatment days", # to check if as outcome or as exposure
    "Length of use (days)", # unclear if as outcome or as exposure
    "Antibiotic use with carbapenem in previous 84 days",
    "Quantitative indices of antibiotic usage - Antibiotic-days/patient"
  ) ~ "Antibiotic exposure: duration (numeric)",
  `resistant_group variable` %in% c(
    "Patients with previous antibiotic treatment - Types of antimicrobials",
    "Patients with previous antibiotic treatment - Antimicrobial families",
    "Patients with previous antibiotic treatment - Different antimicrobial families",
    "Quantitative indices of antibiotic usage - number of different antibiotics used/patient",
    "All patients - Types of antimicrobials",
    "All patients - Different antimicrobial families",
    "Number of antibiotic agent"
  ) ~ "Antibiotic exposure: different antibiotics (numeric)",
  `resistant_group variable` %in% c(
    "Days of positive blood culture",
    "time to resolution of BSI - days (first of the 2 consecutively negative blood cultures after infection) - Pediatric patients",
    "Duration of bacteremia (days)",
    "Duration of Bacteremia, days",
    "Duration of bacteremia",
    "Only patients who had hospital-acquired bacteremia - time to bacteremia from admission date",
    "Number of sets of positive blood cultures",
    "LOS after GNB bacteremia, days",
    "LOS after BSI, days",
    "Hospital stay after diagnosis",
    "Hospital stay after bacteremia",
    "Post Infection LOS (>30 day survivors)",
    "duration of hospital stay postinfection for survivors",
    "no. of days of hospital stay postinfection for survivors",
    "no. of days of hospital\r\nstay postinfection for survivors",
    "no. of days of antibiotic administration postculture",
    "no. of days of\r\nantibiotic administration\r\npostculture",
    "hospital days among patients who did not receive effective empirical antibiotic treatment"
  ) ~ "Patient outcomes",
  `resistant_group variable` %in% c(
    "Leukocytes (cells/mm3)",
    "Laboratory examination - White blood cell"
  ) ~ "Blood lab values",
  `resistant_group variable` %in% c(
    "Predisposing factors - CD4 count, cells/ml",
    "Chronic Liver Failure-Consortium Acute-on-Chronic Liver Failure (CLIF-C ACLF)",
    "BMI",
    "Organ failure at admission",
    "Model of End-Stage Liver Disease-Na"
  ) ~ "Comorbidity score",
  `resistant_group variable` %in% c(
    "Total numbers of invasive procedures",
    "Length of mechanical ventilation at onset of bacteraemia",
    "Time to start enteral feeds (days)",
    "Duration of central venous device (days)",
    "Mechanical ventilation duration (days)",
    "Encounter-specific risk factors (prior to development of bacteraemia) - Urinary catheter, days",
    "Arterial catheter (AC) - Overall duration of AC"
  ) ~ "Invasive procedures",
  `resistant_group variable` %in% c(
    "Outcome - alive (days)",
    "Days to discharge",
    "Duration of Follow-Up, patient days",
    "Duration of Fever after Hospital Admission, days"
  ) ~ "Patient outcomes",
  `resistant_group variable` %in% c(
    "Disseminated intravascular coagulation (DIC) score"
  ) ~ "Clinical severity score",
  `resistant_group variable` %in% c("Age", "Age (yr)", "Age (yrs)", "Age (months)", "Age at onset of LOS (days)") ~ "Age",
  `resistant_group variable` %in% c("weight-for-age z-score") ~ "Preterm birth/low birth weight",
  `resistant_group variable` %in% c("Days since last hospitalization",
                                    "Duration of Symptoms on Presentation, days", # surprised there are not more of this
                                    "days to death after admission",
                                    "Monotherapy - daptomycin dose (mg/kg)" # check if this could be a separate category - surprisingly seems to be the only dose/dosage metric
  ) ~ "Other",
  `resistant_group variable` %in% c("Infection or colonization of K. pneumoniae in previous 84 days") ~ "Prior infection/colonisation",
  TRUE ~ "Other"))

table(continuous_df$`resistant_group variable`[continuous_df$indicatorcategory=="Other"]) # the remaning "Other" seem fine now, each reported just once

# display all numerical indicators
numindicators <- continuous_df %>%
  filter(!is.na(`resistant_group variable`)) %>%
  select(`resistant_group variable`, indicatorcategory, `resistant_group notes`) %>%
  distinct()
numindicators
write_xlsx(numindicators, "numindicators.xlsx")

# keep only essential variables, filter out the sets without numbers entered, then remove duplicates
print(colnames(continuous_df), max = 1900)
continuous_dfshort <- continuous_df %>% select(`Study ID`, Study_ID, Study_country, Publication_year, Study_setting, Model, set, amr, Resistant_group_tot_nb, 
                             Susceptible_group_definition, Susceptible_group_tot_nb, indicatorcategory, `resistant_group variable`, #`Proportion Variable_description`,
                            `resistant_group p-value` #, 
                             # set2, `Number Resistant_group_value`, `Number Susceptible_comparator_group_value`, `Number p-value`, `Number Total`
                            ) %>%
  filter(!is.na(`resistant_group variable`)) %>%
  distinct()

# DESCRIPTIVE_dichotomous_proxy-indicators -> reshape the second part of df_long in a longer format, in which all variables containing the categorical indicators Number_1, Number_2, etc. are brought together in the same variable
print(colnames(df_long), max = 1900)
# check if the order of descriptive dichotomous and the univariate dichotomous variables correspond
check <- df_long %>% group_by(`Proportion_1 Variable_description`, `Number_1 Variable_description`, `Predictor_1 Definition...1468`, `Predictor_1 Definition...1717`) %>% summarise((n=n()))

categorical_df <- df_long %>% 
  select(1:32, 394:965, amr, Model, Resistant_group_tot_nb, Susceptible_group_tot_nb)
check <- categorical_df %>% group_by(`Number_1 Resistant_group_definition`, Resistant_grp_definition, amr, Susceptible_group_definition) %>% summarise(n=n())

categorical_df <- categorical_df %>%
  rename_with(~ str_replace(.x, "^(Number|Proportion|95%CI)_(\\d+) (.*)$", "\\1 \\3_\\2")) # put the sequential numbers at the end

categorical_df <- categorical_df %>% mutate(across(contains("Number Resistant_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Number Susceptible_comparator_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Resistant_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Susceptible_comparator_group_value_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion Total_"), as.character)) # same format of variables that are combined in the longer df
categorical_df <- categorical_df %>% mutate(across(contains("Proportion p-value_"), as.character)) # same format of variables that are combined in the longer df

categorical_df <- categorical_df %>%  pivot_longer(
  cols = matches("_(?:[1-9]|1[0-9]|2[0-9]|3[0-8])$"),   # matches _1 to _38
  names_to = c(".value", "set2"),             # .value keeps base var names
  names_pattern = "(.*)_(\\d+)$")
colnames(categorical_df)

# get rid of empty rows (per study model, up to 38 indicators reported, but usually les, so many rows are empty)
categorical_df <- categorical_df %>% filter(!is.na(`Number Variable_description`))

# regroup the categorical exposures of interest/indicators in `Proportion Variable_description` -> NEED TO BE FURTHER CLEANED - some labels might be added that are mistakenly added
categorical_df <- categorical_df %>%  mutate(
    indicatorcategory = case_when( # replace by indicatorcategory when done
      str_detect(`Proportion Variable_description`, regex("ICU|intensive care", ignore_case = TRUE)) ~ "Prior ICU stay",
      str_detect(`Proportion Variable_description`, regex("Clinical severity - All-cause 30-day mortality|Overall in-hospital mortality", ignore_case = TRUE)) ~ "Patient outcomes", # is an outcome, so avoid that it's labelled 'severity' or 'prior hospi'
      str_detect(`Proportion Variable_description`, regex("Prior hospital admission|hospitalisation|hospital stay|readmission|healthcare-associated|Hospital-acquired|Recent international healthcare exposure|nosocomial", ignore_case = TRUE)) ~ "Prior hospitalisation", # check if nosocomial or hospital/healthcare associated should be a separate category
      str_detect(`Proportion Variable_description`, regex("Hospitalization|Hospitalisation|Previous admission|Admission history|Prior hospital", ignore_case = TRUE)) ~ "Prior hospitalisation",
      str_detect(`Proportion Variable_description`, regex("Hospital", ignore_case = TRUE)) |
      str_detect(`Proportion Variable_description`, ">48h") ~ "Hospital-acquired",
      str_detect(`Proportion Variable_description`, regex("Community", ignore_case = TRUE)) ~ "Community-acquired",
      str_detect(`Proportion Variable_description`, regex("long-term care|long term care|long-term-care|nursing home|Long-term acute care facility residence", ignore_case = TRUE)) ~ "Long-term care facility",
      str_detect(`Proportion Variable_description`, regex("primary infection site|cellulitis", ignore_case = TRUE)) ~ "Specific primary infection site",
      str_detect(`Proportion Variable_description`, regex("colonizat|Prior ESBL", ignore_case = TRUE)) ~ "Prior colonization or infection",
      str_detect(`Proportion Variable_description`, regex("Colonisation|Colonization|Previous .*infection|Previous .*isolate|History of .*infection", ignore_case = TRUE)) ~ "Prior colonization or infection",
      str_detect(`Proportion Variable_description`, regex("Thoracentesis|Tracheal|Cannula|Aspiration|Nutrition|pacemaker|catheter|surgery|surgical proced|caesarian|intubat|foley|catheter|central line|ventilator|surgery|invasive|hemodialys|mechanical ventilat|central venous line|gastric tube|parenteral nutrit", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("Device|Catheter|Intubation|Surgery|Operation|Bronchoscopy|Drain|Tube|Endoscopy|Tracheo|Puncture", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("leukocytes|lymphocytopenia|coagulation|low hemoglobin|low wbc|neutropenia|thrombocytopenia|Hypoproteinemia|monocyte|neutropenia|wbc|hemoglobin|neutrophil|platelet|International normalized ratio|Hb|Haematocr", ignore_case = TRUE)) ~ "Low blood values",
      str_detect(`Proportion Variable_description`, regex("Requirement of blood transfusion(s)|sepsis|shock|severe|clinical severity", ignore_case = TRUE)) ~ "Clinical severity",
      str_detect(`Proportion Variable_description`, regex("blood transfusion", ignore_case = TRUE)) ~ "Blood transfusion",
      str_detect(`Proportion Variable_description`, regex("burn", ignore_case = TRUE)) ~ "Burns",
      str_detect(`Proportion Variable_description`, regex("crp|procalcitonin|biomarker", ignore_case = TRUE)) ~ "Biomarker positive",
      str_detect(`Proportion Variable_description`, regex("diabetes|hypertension|copd|asthma", ignore_case = TRUE)) ~ "NCDs",
      str_detect(`Proportion Variable_description`, regex("solid organ tumor|comorbidity|cancer|renal|liver|hiv|malignancy|dementia|hemipleg|congestive heart failur|myocardial infarc|chronic neurological|vascular disease", ignore_case = TRUE)) ~ "Comorbidities",
      str_detect(`Proportion Variable_description`, regex("Chronic|Underlying|Comorbid|Charlson|NCD|Kidney|Cardiac|Tumou?r|Neoplasia|Pulmonary|Rheumatic|Immunosuppress|Autoimmune|Malignant|Comorbid|Cancer", ignore_case = TRUE)) ~ "Comorbidities",
      str_detect(`Proportion Variable_description`, regex("preterm|low birth weight|prematurity|Gestation|Birth weight|Birthweight", ignore_case = TRUE)) ~ "Preterm birth/low birth weight",
      str_detect(`Proportion Variable_description`, regex("infant|neonate|child|young age|newborn|Inborn", ignore_case = TRUE)) ~ "Young age",
      str_detect(`Proportion Variable_description`, regex("cellulitis|pneumonia|uti|bacteremia|wound|infection site|Source of |focus", ignore_case = TRUE)) ~ "Primary infection site",
      str_detect(`Proportion Variable_description`, regex("resistant|susceptible|intermediate|isolated|Resistance to | resistance", ignore_case = TRUE)) ~ "Resistance profile",
      str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|cephalosporin|carbapenem|vancomycin|fluoroquinolone", ignore_case = TRUE)) ~ "Prior antibiotic exposure",
      str_detect(`Proportion Variable_description`, regex("Antibiotic|Antimicrobial|therapy|Previous .*therapy|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide", ignore_case = TRUE)) ~ "Prior antibiotic exposure",
      str_detect(`Proportion Variable_description`, regex("ESBL|MDR|Resistance|Susceptibility|Resistant", ignore_case = TRUE)) ~ "Resistance profile",
      str_detect(`Proportion Variable_description`, regex("male|women", ignore_case = TRUE)) ~ "Sex",
      `Proportion Variable_description` %in% c("men", "Men", "Sex - men") ~ "Preterm birth/low birth weight",
      str_detect(`Proportion Variable_description`, regex("Outcome - Survival|mortality|death|fatality|30-day outcome|7-day clinical treatment failure|clinical cure", ignore_case = TRUE)) ~ "Patient outcomes",
      str_detect(`Proportion Variable_description`, regex("Outcome|Survival|Death|Mortality|Recovered|Failure|Response|Overall survival", ignore_case = TRUE)) ~ "Patient outcomes",
      str_detect(`Proportion Variable_description`, regex("race|ethnicity", ignore_case = TRUE)) ~ "Ethnicity",
      (str_detect(`Proportion Variable_description`, regex("recurrent", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("BSI", ignore_case = TRUE))) ~ "Prior colonization or infection",
      str_detect(`Proportion Variable_description`, regex("organism|species", ignore_case = TRUE)) ~ "Pathogen",
      str_detect(`Proportion Variable_description`, regex("Streptococcus spp.|Escherichia coli|Klebsiella|Enterococcus|Staphylococcus|MRSA|MSSA|E\\. faecalis|E\\. faecium|Pathogen||Subtype|ST |Phylogenetic|genes|E\\.faecalis|A\\. baumannii|KP detection|Gram-negative|Gram-positive|EPE", ignore_case = TRUE)) ~ "Pathogen",
      str_detect(`Proportion Variable_description`, regex("charlson", ignore_case = TRUE)) ~ "Comorbidity score",
      str_detect(`Proportion Variable_description`, regex("SOFA|APACHE|qSOFA|Severity|ICU|Critical|Fatal|Coma scale|pSOFA", ignore_case = TRUE)) ~ "Clinical severity",
      str_detect(`Proportion Variable_description`, regex("Fever|Respiratory|Pleural|Manifestations|Clinical characteristics", ignore_case = TRUE)) ~ "Clinical manifestations",
      str_detect(`Proportion Variable_description`, regex("Region|Residence|Insurance|Income", ignore_case = TRUE)) ~ "Geography",
      str_detect(`Proportion Variable_description`, regex("Primary site|Skin|Soft tissue|Urinary tract|Biliary|Bone|Joint|Abdominal|Hepato|Lung|Respiratory|Gastro", ignore_case = TRUE)) ~ "Primary infection site",
      TRUE ~ "Other"
    ))
table(categorical_df$indicatorcategory)

# display all categorical indicators
catindicators <- categorical_df %>%
  filter(!is.na(`Proportion Variable_description`)) %>%
  select(`Proportion Variable_description`, indicatorcategory, `resistant_group notes`)  %>%
  distinct()
catindicators
write_xlsx(catindicators, "catindicators.xlsx")

# only those labelled as "Other" -> none left
catindicatorsother <- df_longer %>%
  filter(!is.na(`Proportion Variable_description`), indicatorcategory=="Other") %>%
  select(`Proportion Variable_description`, `resistant_group notes`)  %>%
  distinct()
catindicatorsother
write_xlsx(catindicatorsother, "catindicatorsother.xlsx")

# since measures of association are not reported by all studies, and those reported are in the separate 'predictors' part of the database, calculate crude odds ratios based on the reported counts exposed vs unexposed in the AMR and S groups

# check reference categories between studies, making sure the same are used between studies that are analysed together
# check indicator categories and if a reference is specified
ref <- categorical_df %>% group_by(indicatorcategory, `Proportion Variable_description`, Study_ID) %>% summarise(n=n()) # STILL NEED TO ADD REFERENCE FOR OTHER CATEGORICAL VARIABLES THAT ARE NOT YES VS NO
# add a variable to indicate which value is used as the reference
categorical_df$ref[categorical_df$indicatorcategory=="Sex"] <- "male" 
categorical_df$ref[categorical_df$indicatorcategory=="Sex"&grepl("female",categorical_df$`Proportion Variable_description`, ignore.case = T)] <- "female" 

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

check <- categorical_df %>% filter(is.na(categorical_df$`Number Susceptible_comparator_group_value`)) %>% select(Study_ID, 33:61)

# calculate crude odds ratio based on reported numbers
# categorical_df$or <- (categorical_df$`Number Resistant_group_value`/categorical_df$Resistant_group_tot_nb)/(categorical_df$`Number Susceptible_comparator_group_value`/categorical_df$Susceptible_group_tot_nb)
a <- categorical_df$`Number Resistant_group_value`
b <- categorical_df$Resistant_group_tot_nb - a
c <- categorical_df$`Number Susceptible_comparator_group_value`
d <- categorical_df$Susceptible_group_tot_nb - c

# odds ratio with 95% CI
categorical_df$or <- (a * d) / (b * c)
se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
categorical_df$ci_low  <- exp(log(categorical_df$or) - 1.96 * se_log_or)
categorical_df$ci_high <- exp(log(categorical_df$or) + 1.96 * se_log_or)

# inverse if sex is "female", to make sure "male" is compared against "female" as the reference category (most frequent comparison)
categorical_df$or[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- 1/categorical_df$or[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$ci_low[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- 1/categorical_df$ci_low[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$ci_high[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- 1/categorical_df$ci_high[categorical_df$ref=="female"&!is.na(categorical_df$ref)]

# combine lower an upper CI limits in a single variable
categorical_df$ci_label <- sprintf("%.1f-%.1f", categorical_df$ci_low, categorical_df$ci_high) # combining confidence intervals

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
    fill = "n studies" ) + 
  scale_fill_distiller(
  palette = "YlOrRd",  # yellow → orange → red
  direction = 1,
  na.value = "grey90")
ggsave(filename = "map.jpeg",  width = 8, height = 5, dpi = 300)

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
ggsave(filename = "bar_chart_healthcare_level.jpeg",  width = 8, height = 5, dpi = 300)

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
ggsave(filename = "bar_chart_study_population.jpeg",  width = 8, height = 5, dpi = 300)

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
ggsave(filename = "bar_chart_AMR_profiles.jpeg",  width = 8, height = 5, dpi = 300)

# create a variable for each pathogen-antibiotic combination





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
  group_by(indicatorcategory) %>%
  filter(!is.na(`resistant_group p-value`)) %>%
  summarise(n=n())
summary_indicator_frequencies

# 2.2 CATEGORICAL VARIABLES
# count categorical indicators reported
length(unique(df_longer$`Number Variable_description`)) # 2111 cat indicators reported, `Number Variable_description` and `Proportion Variable_description` are the same
length(unique(df_longer$`Proportion Variable_description`))
# summarize the reported indicators 
categorical_summary <- categorical_df %>%
  group_by(indicatorcategory, amr) %>%
  summarise(
    n_studies = n_distinct(Study_ID),                           
    sum_resistant_tot = sum(Resistant_group_tot_nb, na.rm = TRUE), 
    sum_susceptible_tot = sum(Susceptible_group_tot_nb, na.rm = TRUE), 
    sum_resistant_value = sum(`Number Resistant_group_value`, na.rm = TRUE), 
    sum_susceptible_value = sum(`Number Susceptible_comparator_group_value`, na.rm = TRUE), 
    median_or = median(or, na.rm = TRUE),                    
    q25_or = quantile(or, 0.25, na.rm = TRUE),               # 25th percentile
    q75_or = quantile(or, 0.75, na.rm = TRUE)                # 75th percentile
  ) %>%
  ungroup()
categorical_summary
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
  facet_grid(rows = vars(indicatorcategory), scales = "free_y", space = "free_y") +
  theme(strip.text.y.left = element_text(angle = 90, hjust = 1, face = "bold"))
orplot
ggsave(orplot, filename = "OR_summary.jpeg",  width = 12, height = 49, dpi = 250) 

# subset only studies looking at sex
categorical_df_or_sex <- categorical_df_or %>% filter(indicatorcategory=="Sex")
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
