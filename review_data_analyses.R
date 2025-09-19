###########################################
# SCOPING REVIEW FOR AMR PROXY INDICATORS #
###########################################

# install/load packages
pacman::p_load(readxl, lubridate, haven, dplyr, tidyr, digest, ggplot2, survey, srvyr, gtsummary, lme4, broom.mixed, stringr)

#### 1. IMPORT DATA #### 
df <- read_excel("20250830 WHO_Extracted-data_Consensed_review_457022_20250830231950.xlsx")
# see that numeric variables could not be imported as such, since they  sometimes contains characters, like "n=..."
# display variable names
print(colnames(df), max = 1900)
table(df$AMR_mechanism_1)
table(df$AMR_mechanism_2)
