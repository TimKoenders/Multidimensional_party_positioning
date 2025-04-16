#### Clean-up and packages ----------------------------------------------------------------
rm(list = ls())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr",
              "plyr",                          
              "readr",
              "readxl",
              "tidyverse",
              "ggplot2",
              "stringr",
              "car",
              "lmtest",
              "lme4",
              "lmerTest",
              "MuMIn",
              "apaTables",
              "report",
              "lmerTest",
              "lme4",
              "lmtest",
              "apaTables",
              "MuMIn",
              "readxl",
              "influence.ME",
              "rstatix",
              "haven",
              "ggpubr",
              "GenderInfer",
              "foreign",
              "extrafont",
              "tidyr",
              "reshape2",
              "knitr",
              "Gmisch",
              "grid",
              "survey",
              "psych",
              "dplyr",
              "scales",
              "ggrepel",
              "rgl",
              "htmlwidgets",
              "gridExtra")
ipak(packages)


list.files()

#### Importing data ----------------------------------------------------------
df_ches_trend <- read_dta("1999-2019_CHES_dataset_means.dta")
df_ches_2024 <- read_dta("CHES_2024_final_v2.dta")
#### Merging files -------------------------------------------------------
## Check overlap of variable names
setdiff(names(df_ches_trend), names(df_ches_2024))
setdiff(names(df_ches_2024), names(df_ches_trend))
## Remove labels
df_ches_trend <- zap_labels(df_ches_trend)
df_ches_2024 <- zap_labels(df_ches_2024)
## Appending
df_ches <- bind_rows(df_ches_trend, df_ches_2024)




#### Test data availability --------------------------------------------------
## Relevant positional items
# Define variables of interest
vars <- c("lrecon", "eu_position", "immigrate_policy", "multiculturalism", "redistribution", 
          "climate_change", "spendvtax", "deregulation", "environment")

# --- PART 1: Count unique country-years per variable ---

# Step 1: Reshape to long and keep non-missing observations
df_temp <- df_ches %>%
  select(country, electionyear, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  distinct(variable, country, electionyear)

# Step 2: Count number of unique country-years per variable
coverage_counts <- aggregate(electionyear ~ variable, data = df_temp, FUN = length)
coverage_counts <- coverage_counts[order(-coverage_counts$electionyear), ]

# Output: number of country-years each dimension is observed
print(coverage_counts)

# --- PART 2: Count how many dimensions are filled per country-year ---

# Step 1: Create binary indicator of non-missingness per variable
df_binary <- df_ches %>%
  select(country, electionyear, all_of(vars)) %>%
  mutate(across(all_of(vars), ~ !is.na(.)))

# Step 2: Count how many non-missing dimensions per country-year
df_binary$n_dimensions <- rowSums(df_binary[vars])

# Step 3: Count how many country-years have a given number of dimensions
dimension_coverage <- aggregate(electionyear ~ n_dimensions, data = df_binary, FUN = length)

# Step 4: Add cumulative count
dimension_coverage <- dimension_coverage[order(dimension_coverage$n_dimensions, decreasing = TRUE), ]
dimension_coverage$cumulative <- cumsum(dimension_coverage$electionyear)

# Output: coverage by dimensional completeness
print(dimension_coverage)


#### Subset data -------------------------------------------------------------
## Subset
core_vars <- c("deregulation", "immigrate_policy", "multiculturalism", "redistribution", "spendvtax")

df_core <- df_ches %>%
  filter(if_all(all_of(core_vars), ~ !is.na(.))) %>%
  select(country, electionyear, party_id, party, family, all_of(core_vars))

## Information on sample
nrow(df_core)               # Total number of party observations
length(unique(df_core$party_id))  # Number of unique parties
length(unique(paste(df_core$country, df_core$electionyear)))  # Number of unique country-years

# Count time points per party
party_timepoints <- aggregate(electionyear ~ party_id, data = df_core, FUN = function(x) length(unique(x)))
timepoint_distribution <- table(party_timepoints$electionyear)
print(timepoint_distribution)

#### Map country numbers to names ---------------------------------------
country_codes <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 34, 35, 36, 37, 38, 40, 45)

country_names <- c("Belgium", "Denmark", "Germany", "Greece", "Spain", "France", "Ireland", "Italy", "Netherlands", "United Kingdom",
                   "Portugal", "Austria", "Finland", "Sweden", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
                   "Poland", "Romania", "Slovakia", "Slovenia", "Croatia", "Turkey", "Norway", "Switzerland", "Malta", "Luxembourg",
                   "Cyprus", "Iceland")
df_core$country <- country_names[match(df_core$country, country_codes)]


#### Map family numbers to names -----------------------------------------
family_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
family_names <- c("Radical Right", "Conservatives", "Liberal", "Christian-Democratic", "Socialist",
                  "Radical Left", "Green", "Regionalist", "No family", "Confessional", "Agrarian/Centre")
df_core$family <- family_names[match(df_core$family, family_ids)]






#### Save CSV ----------------------------------------------------------------
write.csv(df_core, "df_core.csv", row.names = FALSE)


