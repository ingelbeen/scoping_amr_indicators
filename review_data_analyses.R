###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# last update: 20/10/2025 17h23

# install/load packages
pacman::p_load(here, readxl, writexl, openxlsx, lubridate, haven, dplyr, tidyr, stringr, countrycode, ggplot2, forcats, rnaturalearth, rnaturalearthdata, RColorBrewer, ggbeeswarm, DT, gt, scales, formattable)

#### 0. IMPORT/CLEAN DATA #### 
df <- read_excel("db/data_raw/20251011 review_457022_20251011204039_ALLminus6.xlsx") # note that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."

# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)
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
df$studypop <- dplyr::case_when(
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
df$highrisk <- dplyr::case_when(
  (grepl("neonat", df$Population_admitting_ward, ignore.case = TRUE)==T & grepl("general ", df$Population_admitting_ward, ignore.case = TRUE)==F) ~ "high risk",
  (grepl("Immunocompromised", df$Population_admitting_ward, ignore.case = TRUE)==T & grepl("general ", df$Population_admitting_ward, ignore.case = TRUE)==F) ~ "high risk",  
  (grepl("Critical", df$Population_admitting_ward, ignore.case = TRUE)==T & grepl("general ", df$Population_admitting_ward, ignore.case = TRUE)==F) ~ "high risk",  
  TRUE ~ "regular, several popualtions, or unspecified")
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

# one study is an intervention study and therefore part of a different analysis - to be removed
df <- df %>% filter(Study_ID!="#11818")

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
    select(`resistant_group variable` = ind, `Covidence #`, `Model`, `resistant_group definition`, `resistant_group mean`, `resistant_group median`, `Notes`, `General notes_AMR`, `General notes - if any...48`),  
  "list_all_continuous_indicators.xlsx"
)

#export list of UNIQUE continuous proxy indicators keeping unique indicator names
continuous_df %>% 
  select('resistant_group variable') %>% 
  filter(!is.na(`resistant_group variable`),
         `resistant_group variable` != "") %>%
  distinct() %>% 
  arrange('resistant_group variable') %>% 
  write_xlsx("list_unique_continuous_indicators.xlsx")

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
              "cont_indicatorcategory_l1", "cont_indicatorcategory_l2", "resistant_group mean", "resistant_group median") %in% names(cont_map_df)))

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
    l2 = na_if(str_squish(cont_indicatorcategory_l2), "")
)

#d.collapse the mapping to one row per (cov_clean, res_var_clean, res_def_clean, mean-median_key)
#set duplicates to disagree as NA so they can be viewed and dealt with. variables that have con_indicatorcategory_l1 and l2 empty coz need to be removed from here will be NA
cont_map_collapse <- cont_map_keys %>% 
  group_by(cov_clean, res_var_clean, res_def_clean, mean_median_key) %>% 
  summarize(
    n_rows = n(),
    n_l1 = n_distinct(na.omit(l1)),
    n_l2 = n_distinct(na.omit(l2)),
    cont_indicatorcategory_l1 = if (n_l1 <= 1) first(na.omit(l1)) else NA_character_,
    cont_indicatorcategory_l2 = if (n_l2 <= 1) first(na.omit(l2)) else NA_character_,
    .groups = "drop"
  )

#view conflicts
conflicts <- cont_map_collapse %>%  filter(n_l1 > 1 | n_l2 > 1)
if (nrow(conflicts) > 0) {
  warning(sprintf("Mapping has %d conflicting 4-keys (same key --> multiple categories - fix in excel later", nrow(conflicts)))
}

#e.remove any blank or NA in the predictor (resistant_group variable) in continuous_df
continuous_df <- continuous_df %>% 
  filter(
    !(is.na(`resistant_group variable`) |
      str_squish(as.character(`resistant_group variable`)) == "")
  )

#verify no blank remains
stopifnot(
  !any(is.na(continuous_df$`resistant_group variable`) |
         str_squish(as.character(continuous_df$`resistant_group variable`)) == "")
)

#f.build the same 4 matching keys in continuous_df
stopifnot(all(c(
              "Covidence #", "resistant_group variable", "resistant_group definition", 
              "resistant_group mean", "resistant_group median") %in% names(continuous_df)))

continuous_df_keys <- continuous_df %>% 
  mutate(
    cov_clean = clean_text(`Covidence #`),
    res_var_clean = clean_text(`resistant_group variable`),
    res_def_clean = clean_text(`resistant_group definition`),
    mean_median_key = coalesce(
      as.character(`resistant_group mean`),
      as.character(`resistant_group median`))
    |> str_replace_all("\u00A0", "") |> str_squish()
  )

#g.safe left join on the 4 keys - no row multiplication for those doubles 
n_before <- nrow(continuous_df_keys)

continuous_df <- continuous_df_keys %>% 
  left_join(
    cont_map_collapse %>% 
      select(cov_clean, res_var_clean, res_def_clean, mean_median_key,
             cont_indicatorcategory_l1, cont_indicatorcategory_l2),
    by = c("cov_clean", "res_var_clean", "res_def_clean", "mean_median_key")
  ) %>% 
select(-cov_clean, -res_var_clean, -res_def_clean, -mean_median_key)

n_after <- nrow(continuous_df)
if (n_after != n_before) {
  warning(sprintf("row count changed after; check in any not intended many-to-many matches", n_before, n_after))
}

#------ ---- START-START-START --- CHECK the joint Excel of continuous variables for QA ---- START
joined_keys <- continuous_df %>%
  mutate(
    cov_clean = str_squish(str_to_lower(`Covidence #`)),
    res_var_clean = str_squish(str_to_lower(`resistant_group variable`)),
    res_def_clean = str_squish(str_to_lower(`resistant_group definition`)),
    mean_median_key = coalesce(as.character(`resistant_group mean`),
                         as.character(`resistant_group median`)) %>%
      str_replace_all("\u00A0","") %>% str_squish()
  )

#every row should map to exactly 1 record of that key (such as no 1→many explosion after the join)
dup_check <- joined_keys %>%
  count(cov_clean, res_var_clean, res_def_clean, mean_median_key, name = "n_per_key") %>%
  filter(n_per_key > 1)

nrow(dup_check)  # should be 0
# If >0, print few
dup_check %>% head(10)

dup_check %>% tally()                  # how many duplicated 4-keys
dup_check %>% arrange(desc(n_per_key)) # which keys have the most repeats

#check for one 
one <- dup_check %>% slice(1)

joined_keys %>%
  filter(
    cov_clean  == one$cov_clean,
    res_var_clean == one$res_var_clean,
    res_def_clean == one$res_def_clean,
    mean_median_key == one$mean_median_key
  ) %>%
  select(`Covidence #`, `resistant_group variable`, `resistant_group definition`,
         `resistant_group mean`, `resistant_group median`,
         Model, set, cont_indicatorcategory_l1, cont_indicatorcategory_l2) %>%
  print(n = Inf)
#actual duplictae 

#count duplicates
# Count total number of duplicate rows (identical across ALL columns)
total_duplicates <- continuous_df %>%
  duplicated() %>%        # logical vector: TRUE for duplicated rows
  sum()                   # count how many TRUEs
total_duplicates #they are zero

#count duplicates based on my 4 keys
key_cols <- c("Covidence #",
              "resistant_group variable",
              "resistant_group definition",
              "resistant_group mean",
              "resistant_group median")

dup_by_key <- continuous_df %>%
  count(across(all_of(key_cols)), name = "n_per_key") %>%
  filter(n_per_key > 1)

#number of duplicated 4 keys
n_dupl_keys <- nrow(dup_by_key)

#number of duplicated rows (counting all repeats)
n_dupl_rows <- sum(dup_by_key$n_per_key) - n_dupl_keys

n_dupl_keys   # how many unique 4 keys are duplicated
n_dupl_rows   # how many extra rows that represent duplicates

dup_summary <- continuous_df %>%
  mutate(is_dup_4key = duplicated(select(., all_of(key_cols))) |
           duplicated(select(., all_of(key_cols)), fromLast = TRUE)) %>%
  summarise(
    total_rows = n(),
    unique_rows = sum(!is_dup_4key),
    duplicated_rows = sum(is_dup_4key),
    duplicated_percent = round(mean(is_dup_4key) * 100, 2)
  )
dup_summary #GIVE totals of duplicates - random check show that they actually match what I have ticked as "double" in excel - to check with Brecht from wher eit is coming

dup_by_key %>% arrange(desc(n_per_key)) %>% head(30)

joined_keys %>%
  summarise(
    rows_total = n(),
    level1_assigned = sum(!is.na(cont_indicatorcategory_l1)),
    level2_assigned = sum(!is.na(cont_indicatorcategory_l2)),
    both_missing = sum(is.na(cont_indicatorcategory_l1) & is.na(cont_indicatorcategory_l2))
  ) #almost match the initial excel (35 empty because they need to be in categorical or have no p-value)

#check of any rowa that did not match the mapping
unmatched <- joined_keys %>%
  filter(is.na(cont_indicatorcategory_l1) & is.na(cont_indicatorcategory_l2)) %>%
  distinct(`Covidence #`, `resistant_group variable`, `resistant_group definition`,
           `resistant_group mean`, `resistant_group median`) %>%
  arrange(`Covidence #`, `resistant_group variable`)
#view(unmatched)  
nrow(unmatched) 

#unmatched show 27 - check if those are true duplicates as they need to be zero
#define the 4 keys exactly sed in the join
key_cols <- c("Covidence #",
              "resistant_group variable",
              "resistant_group definition",
              "resistant_group mean",
              "resistant_group median")
#find which showe more than once and view them
dup_keys <- continuous_df %>%
  count(across(all_of(key_cols)), name = "n_per_key") %>%
  filter(n_per_key > 1)
nrow(dup_keys)        # should print 27
View(dup_keys)        # they are NOT actual duplicates - they need to be all kept - so checked

#check a quick sample to see if it looks right
set.seed(1)
joined_keys %>%
  select(`Covidence #`, `resistant_group variable`, `resistant_group definition`,
         `resistant_group mean`, `resistant_group median`,
         cont_indicatorcategory_l1, cont_indicatorcategory_l2) %>%
  sample_n(min(10, n()))
#I see one that has "NA" for "resistance_group variable" - this cannot be

#count how many rows have an empty/missing resiatnce_group variable name
continuous_df %>%
  filter(is.na(`resistant_group variable`) |
           str_squish(`resistant_group variable`) == "") %>%
  summarise(n_missing = n())
missing_var_rows <- continuous_df %>%
  filter(is.na(`resistant_group variable`) |
           str_squish(`resistant_group variable`) == "")
View(missing_var_rows) #only 1

#check where this empty error is coming from
cont_map_df %>%
  filter(is.na(`resistant_group variable`) |
           str_squish(`resistant_group variable`) == "") %>%
  summarise(n_missing = n()) #it is zero means they appear after the join (none empty in my excel) 

#check if there are unmatched join results"
continuous_df %>%
  filter(is.na(`resistant_group variable`)) %>%
  select(`Covidence #`, starts_with("resistant_group"), starts_with("cont_")) %>%
  head(20) #looks like for this one row did not find match 

continuous_df %>%
  transmute(original = `resistant_group variable`,
            cleaned = clean_text(`resistant_group variable`)) %>%
  filter(is.na(cleaned) | cleaned == "")

#see if each 4 key maps t just one pair of categories
key_to_cat <- joined_keys %>%
  group_by(cov_clean, res_var_clean, res_def_clean, mean_median_key) %>%
  summarise(
    n_l1 = n_distinct(na.omit(cont_indicatorcategory_l1)),
    n_l2 = n_distinct(na.omit(cont_indicatorcategory_l2)),
    .groups = "drop"
  ) %>%
  filter(n_l1 > 1 | n_l2 > 1)

nrow(key_to_cat)  # should be 0
key_to_cat %>% head(10)

#some summaries
# Distribution of Level-1 categories
joined_keys %>% count(cont_indicatorcategory_l1, sort = TRUE)

#cross-tab of l1 by l22 (see top 30)
joined_keys %>%
  count(cont_indicatorcategory_l1, cont_indicatorcategory_l2, sort = TRUE) %>%
  head(30)

#------ ---- END-END_END - CHECK the joint Excel of continuous variables for QA ----

#SUMMARY - summarize the data to share with the team
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
ggsave("continuous_indicators_bar-chat_level1.png", p_l1, width = 9, height = 6, dpi = 300) #export

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
ggsave("continuous_indicators_heatmap_categories_l1_l2.png", p_heat, width = 12, height = 7, dpi = 300) #export

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
# # check if the order of descriptive dichotomous and the univariate dichotomous variables correspond
# check <- df_long %>% group_by(`Proportion_1 Variable_description`, `Number_1 Variable_description`, `Predictor_1 Definition...1468`, `Predictor_1 Definition...1717`) %>% summarise((n=n()))

categorical_df <- df_long %>% 
  select(1:32, studypop, highrisk, amr, pathogen_antibiotic_combination, 394:965, Model, Resistant_group_tot_nb, Susceptible_group_tot_nb)

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
    indicatorcategory = case_when( # replace by indicatorcategory when done
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
      str_detect(`Proportion Variable_description`, regex("primary infection site|cellulitis|chest infection", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("colonizat|Prior ESBL", ignore_case = TRUE)) ~ "Prior colonization or infection",
      str_detect(`Proportion Variable_description`, regex("Colonisation|Colonization|Previous .*infection|Previous .*isolate|History of .*infection|infection or\r\ncolonization of", ignore_case = TRUE)) ~ "Prior colonization or infection",
      str_detect(`Proportion Variable_description`, regex("Urological manipulation history|Thoracentesis|Tracheal|Cannula|Aspiration|Nutrition|pacemaker|catheter|surgery|surgical proced|caesarian|cesarian|Caesarean|intubat|foley|catheter|central line|ventilator|surgery|invasive|hemodialys|mechanical ventilat|central venous line|gastric tube|parenteral nutrit", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("Device|Catheter|Intubation|Surgery|History of genitourinary intervention|Operation|Bronchoscopy|Drain|Tube|Endoscopy|Tracheo|Puncture|Previous surgery |dialysis|blood purification", ignore_case = TRUE)) ~ "Invasive procedures",
      str_detect(`Proportion Variable_description`, regex("leukocytes|lymphocytopenia|coagulation|low hemoglobin|low wbc|neutropenia|thrombocytopenia|Hypoproteinemia|monocyte|neutropenia|wbc|hemoglobin|neutrophil|platelet|International normalized ratio|Hb|Haematocr", ignore_case = TRUE)) ~ "Low blood values",
      str_detect(`Proportion Variable_description`, regex("Requirement of blood transfusion(s)|sepsis|shock|clinical severity|Pitt", ignore_case = TRUE)) ~ "Clinical severity",
      str_detect(`Proportion Variable_description`, regex("blood transfusion", ignore_case = TRUE)) ~ "Blood transfusion",
      str_detect(`Proportion Variable_description`, regex("transplant|Organ transplantation", ignore_case = TRUE)) ~ "Transplant",
      str_detect(`Proportion Variable_description`, regex("burn", ignore_case = TRUE)) ~ "Burns",
      str_detect(`Proportion Variable_description`, regex("crp|procalcitonin|biomarker", ignore_case = TRUE)) ~ "Biomarker positive",
      str_detect(`Proportion Variable_description`, regex("diabetes|hypertension|copd|asthma", ignore_case = TRUE)) ~ "NCDs",
      str_detect(`Proportion Variable_description`, regex("solid organ tumor|cancer|renal|liver|hiv|malignanc|leukaemia|lymphoma|haematological disease|leukemia|dementia|hemipleg|congestive heart failur|myocardial infarc|Cerebral infarction|chronic neurological|vascular disease", ignore_case = TRUE)) ~ "Comorbidities",
      str_detect(`Proportion Variable_description`, regex("neuroblastoma|Peptic ulcer disease|Connective tissue disease|Urological disease|Cognitive decline|Chronic|Underlying|Charlson|NCD|Kidney|Cardiac|Tumou?r|Neoplasia|Rheumatic|Immunosuppressi|Autoimmune|Malignant|Cancer", ignore_case = TRUE)) ~ "Comorbidities",
      str_detect(`Proportion Variable_description`, regex("history of stroke", ignore_case = TRUE)) ~ "NCDs",
      str_detect(`Proportion Variable_description`, regex("age >|age>|age - adults|Age - 65 plus|Age of >60 yr|Age(years) - 80+|Age - â‰¥80|Age category - >= 80|Age(years) - 80+|Age â‰¥ 75 ys|Age â‰¥ 65", ignore_case = TRUE)) ~ "Older age (cutoffs >/= 60 years)",
      str_detect(`Proportion Variable_description`, regex("Severe underweight-for-age|severe underweight for age|Sickle cell", ignore_case = TRUE)) ~ "Comorbidities",
      str_detect(`Proportion Variable_description`, regex("preterm|low birth weight|prematurity|Gestation|Birth weight|Birthweight", ignore_case = TRUE)) ~ "Preterm birth/low birth weight",
      str_detect(`Proportion Variable_description`, regex("infant|neonate|child|young age|newborn|Inborn|Age - < 5|Age - <5|Age group (years) - 0-4", ignore_case = TRUE)) ~ "Young age (cutoffs </= 5 years)",
      str_detect(`Proportion Variable_description`, regex("cellulitis|pneumonia|uti|bacteremia|wound|Endocarditis|infection site|Source of |focus|Combined infection site - pulmonary infection|pulmonary infection|Mucosal barrier damage at the time of BSI", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("Pyelonephritis|Primary site|Skin|Soft tissue|Urinary tract|Biliary|Bone|Joint|Abdominal|Hepato|Lung|Respiratory|Gastro", ignore_case = TRUE)) ~ "Primary/specific infection site",
      str_detect(`Proportion Variable_description`, regex("resistant|susceptible|intermediate|isolated|Resistance to | resistance", ignore_case = TRUE)) ~ "Resistance profile",
      str_detect(`Proportion Variable_description`, regex("ESBL|MDR|Resistance|Susceptibility|Resistant", ignore_case = TRUE)) ~ "Resistance profile",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|cephalosporin|carbapenem|vancomycin|fluoroquinolone", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("duration", ignore_case = TRUE))) ~ "Prior antibiotic exposure, duration",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|treatment|therapy", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("inappropriate|inadequate|incorrect|failure|wrong|inopportune", ignore_case = TRUE))) ~ "Prior antibiotic exposure, inappropriate choice",
      (str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|chemotherapy|therapy|treatment|use|exposure", ignore_case = TRUE))&str_detect(`Proportion Variable_description`, regex("tigecycline|polymixin|cephalosporin|meropenem|carbapenem|vancomycin|fluoroquinolone|therapy|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide|vancomycin|polymyxin", ignore_case = TRUE))) ~ "Prior antibiotic exposure, specific choice",
      str_detect(`Proportion Variable_description`, regex("antibiotic|antimicrobial|chemotherapy", ignore_case = TRUE)) ~ "Prior antibiotic exposure, non specific",
      # str_detect(`Proportion Variable_description`, regex("cephalosporin|carbapenem|vancomycin|fluoroquinolone|therapy|Piperacillin|Linezolid|Ciprofloxacin|Quinolone|Carbapenem|Ceph|Beta-lactam|Glycopeptide", ignore_case = TRUE)) ~ "Prior antibiotic exposure",
      str_detect(`Proportion Variable_description`, regex("corticoid|steroid", ignore_case = TRUE)) ~ "Prior corticosteroid use",
      (str_detect(`Proportion Variable_description`, regex("therapy|treatment|administration", ignore_case = TRUE))&str_detect(`Proportion Variable_description`, regex("IV|intravenous", ignore_case = TRUE))) ~ "Prior IV therapy",
      str_detect(`Proportion Variable_description`, regex("male|women", ignore_case = TRUE)) ~ "Sex",
      `Proportion Variable_description` %in% c("men", "Men", "Sex - men") ~ "Sex",
      str_detect(`Proportion Variable_description`, regex("mortality|death|fatality|30-day outcome|Outcome - Dead", ignore_case = TRUE)) ~ "Patient outcomes, mortality",
      str_detect(`Proportion Variable_description`, regex("Recovered|Response|Overall survival|clinical cure", ignore_case = TRUE)) ~ "Patient outcomes, cure",
      str_detect(`Proportion Variable_description`, regex("Failure|7-day clinical treatment failure", ignore_case = TRUE)) ~ "Patient outcomes, treatment failure",
      str_detect(`Proportion Variable_description`, regex("complication", ignore_case = TRUE)) ~ "Patient outcomes, complications",
      str_detect(`Proportion Variable_description`, regex("race|ethnicity", ignore_case = TRUE)) ~ "Ethnicity",
      (str_detect(`Proportion Variable_description`, regex("recurrent", ignore_case = TRUE)) & str_detect(`Proportion Variable_description`, regex("BSI", ignore_case = TRUE))) ~ "Prior colonization or infection",
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
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Outcomes - Sepsis attributable mortality"] <- "Patient outcomes, mortality"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Stay in hemato-oncology wards"] <- "Prior hospitalisation"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Community infection - Previous antibiotic use within 90 days"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Community infection - Indwelling biliary drainage"] <- "Invasive procedures"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Outcome parameters - Appropriate therapy administered in <48 h"] <- "Duration to appropriate therapy (binary)"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Effective antimicrobials within 48 hr"] <- "Duration to appropriate therapy (binary)"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Comorbid conditions - Positive A. baumannii blood culture after 72 h of admission"] <- "Hospital-acquired"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Underlying condition - Prior admissions > 2"] <- "Prior hospitalisation"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Underlying condition - Prior antibiotic therapy"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Underlying condition - Crude mortality rate 30 days"] <- "Patient outcomes, mortality"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Recent ventilator-associated pneumonia due to CRAB"] <- "Primary/specific infection site" # not sure. CHECK
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Comorbidity - Malnutrition"] <- "Comorbidities" 
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Baseline comorbidities - hematological"] <- "Comorbidities" 
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Outcome parameters - Discharged to long-term care facilities"] <- "Other" 
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Clinical outcomes - Disseminated intravascular coagulation"] <- "Other" # CHECK
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Clinical outcomes - â‰¥ 10 days of hospital stay from culture to discharge"] <- "Patient outcomes, duration of hospitalisation or treatment"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Healthcare-Associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Infection type - Healthcare-associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Epidemiologic classification - Healthcare-associated"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Index culture >48 h"] <- "Hospital-acquired"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Unreasonable empirical treatment"] <- "Prior antibiotic exposure, inappropriate choice"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Unnecessary use of carbapenems"] <- "Prior antibiotic exposure, inappropriate choice"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Carbapenem administration history"] <- "Prior antibiotic exposure, specific choice"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="Received antimicorbials during previous month"] <- "Prior antibiotic exposure, non specific"
categorical_df$indicatorcategory[categorical_df$`Proportion Variable_description`=="LOS to the first positive culture - â‰¥20 days"] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"

# all specific "healthcare-associated" checked, relabel remaining as non specified
categorical_df$indicatorcategory[grepl("ealthcare-associated", categorical_df$`Proportion Variable_description`)==T|grepl("ealthcare associated", categorical_df$`Proportion Variable_description`)==T] <- "Healthcare associated, other than hospital-acquired or non specified if hospital"


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
categorical_df <- categorical_df %>% filter(grepl("Elixhauser",`Proportion Variable_description`)==F|indicatorcategory!="Other") # high scores have been labelled, the low/reference ones out
categorical_df <- categorical_df %>% filter(grepl("LOS",`Proportion Variable_description`)==F|indicatorcategory!="Other") # long LOS has been labelled, the low/reference ones out


table(categorical_df$indicatorcategory)
categorical_df %>% filter(`Proportion Variable_description`=="Prior carbapenem use") %>% select(`Proportion Variable_description`, indicatorcategory, Study_ID)
check <- categorical_df %>% filter(grepl("Outcome",`Proportion Variable_description`)==T) %>% select(`Proportion Variable_description`, indicatorcategory, Study_ID)
# to check: Outcome|Survival|

categorical_df %>% filter(Study_ID=="#1663") %>% select(`Proportion Variable_description`, indicatorcategory, Study_ID)


# create two levels of indicators (general categories, then more specific)
categorical_df$indicatorcategory_level1 <- dplyr::case_when(
  categorical_df$indicatorcategory %in% c(
    "Healthcare associated, other than hospital-acquired or non specified if hospital",
    "Hospital-acquired", "Long-term care facility", "Prior hospitalisation", "Prior ICU stay", 
    "Community-acquired"
  ) ~ "Healthcare exposure",
  
  categorical_df$indicatorcategory %in% c(
    "Prior colonization or infection"
  ) ~ "Prior colonization or infection",
  
  categorical_df$indicatorcategory %in% c(
    "Invasive procedures", "Prior IV therapy",
    "Transplant", "Blood transfusion"
  ) ~ "Invasive procedures",
  
  categorical_df$indicatorcategory %in% c(
    "Prior antibiotic exposure, specific choice",
    "Prior antibiotic exposure, inappropriate choice",
    "Prior antibiotic exposure, duration",
    "Prior antibiotic exposure, non specific",
    "Duration to appropriate therapy (binary)",
    "Prior corticosteroid use"
  ) ~ "Antibiotic exposure",
  
  categorical_df$indicatorcategory %in% c(
    "Comorbidities", "Comorbidity score, high (Charlson >2;Elixhauser>13)", "NCDs"
  ) ~ "Comorbidities",
  
  categorical_df$indicatorcategory %in% c(
    "Clinical presentation, other than infection site", "Clinical severity",
    "Primary/specific infection site", "Preterm birth/low birth weight"
  ) ~ "Clinical presentation",
  
  categorical_df$indicatorcategory %in% c(
    "Older age (cutoffs >/= 60 years)", "Young age (cutoffs </= 5 years)",
    "Ethnicity", "Sex", "Geography"
  ) ~ "Demographics",
  
  categorical_df$indicatorcategory %in% c(
    "Low blood values", "Biomarker positive"
  ) ~ "Biomedical results",
  
  categorical_df$indicatorcategory %in% c(
    "Pathogen", "Resistance profile"
  ) ~ "Pathogen/resistance",
  
  categorical_df$indicatorcategory %in% c(
    "Patient outcomes, cure", "Patient outcomes, mortality",
    "Patient outcomes, duration of hospitalisation or treatment",
    "Patient outcomes, treatment failure", "Patient outcomes, complications"
  ) ~ "Patient outcomes",
  
  TRUE ~ "Other"
)

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

# make sure the same reference is used when comparing associations
# to make sure "male" is compared against "female" as the reference category (most frequent comparison):
# if sex is "female", replace the number of female resistant by the number of male resistance and the n female susceptible by male
categorical_df$`Number Resistant_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- categorical_df$Resistant_group_tot_nb[categorical_df$ref=="female"&!is.na(categorical_df$ref)] - categorical_df$`Number Resistant_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)] <- categorical_df$Susceptible_group_tot_nb[categorical_df$ref=="female"&!is.na(categorical_df$ref)] - categorical_df$`Number Susceptible_comparator_group_value`[categorical_df$ref=="female"&!is.na(categorical_df$ref)]
categorical_df$indicatorcategory[categorical_df$indicatorcategory=="Sex"] <- "Male sex"

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

# combine lower an upper CI limits in a single variable
categorical_df$ci_label <- sprintf("%.1f-%.1f", categorical_df$ci_low, categorical_df$ci_high) # combining confidence intervals

# display all categorical indicators
catindicators <- categorical_df %>%
  filter(!is.na(`Proportion Variable_description`)) %>%
  select(`Proportion Variable_description`, indicatorcategory, ref, or, ci_low, ci_high, studypop, highrisk, amr, pathogen_antibiotic_combination)  %>%
  distinct()
catindicators
write_xlsx(catindicators, "catindicators.xlsx")
# display all "other"
othercatindicators <- categorical_df %>%
  filter(!is.na(`Proportion Variable_description`)&indicatorcategory=="Other") %>%
  select(`Proportion Variable_description`, indicatorcategory, ref, or, ci_low, ci_high, studypop, highrisk, amr, pathogen_antibiotic_combination)  %>%
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

# 1.5 patient population
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
           str_detect(Population_admitting_ward, "Surgical") ~ "Surgical",
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
length(unique(categorical_df$`Number Variable_description`)) # 2388 cat indicators reported, `Number Variable_description` and `Proportion Variable_description` are the same
length(unique(categorical_df$`Proportion Variable_description`))
# summarize the reported indicators, overall and by AMR profile
categorical_summary <- categorical_df %>%
  group_by(indicatorcategory_level1, indicatorcategory) %>%
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
#reorder so the most frequent are on top
categorical_summary <- categorical_summary %>%
  group_by(indicatorcategory_level1) %>%
  arrange(desc(n_studies), .by_group = TRUE) %>%
  ungroup()
categorical_summary
write_xlsx(categorical_summary, "categorical_summary.xlsx")

categorical_by_amr_summary <- categorical_df %>%
  group_by(indicatorcategory_level1, indicatorcategory, amr) %>%
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
categorical_by_amr_summary

# keep just once the label of the level 1 categories
categorical_summary <- categorical_summary %>% mutate(indicatorcategory_level1 = ifelse(
  duplicated(indicatorcategory_level1), "", indicatorcategory_level1)) %>%
  ungroup()
categorical_summary

# present in a heatmap-style table
formattable(categorical_summary, list(
  n_studies = color_tile("white", "steelblue"),
  median_or = formatter("span",
                        style = x ~ style(
                          display = "block",
                          `border-radius` = "4px",
                          `padding-right` = "4px",
                          `background-color` = ifelse(
                            x < 1,
                            colorRampPalette(c("darkgreen", "white"))(100)[as.numeric(cut(x, breaks = 100))],
                            colorRampPalette(c("white", "darkred"))(100)[as.numeric(cut(pmin(x, 11), breaks = 100))]
                          )
                        )
  )
))
# alternatively
categorical_summary %>%
  gt() %>%
  data_color(
    columns = n_studies,
    colors = scales::col_numeric(
      palette = c("white", "steelblue"),
      domain = c(1, 123)
    )
  ) %>%
  data_color(
    columns = median_or,
    colors = function(x) {
      sapply(x, function(v) {
        if (v <= 1) {
          scales::col_numeric(c("darkgreen", "white"), domain = c(0, 1))(v)
        } else {
          scales::col_numeric(c("white", "darkred"), domain = c(1, 11))(v)
        }
      })
    }
  )

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
