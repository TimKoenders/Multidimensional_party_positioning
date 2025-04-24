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
              "gridExtra",
              "data.table")
ipak(packages)


list.files()

#### Importing data ----------------------------------------------------------
df_ches_trend <- read_dta("1999-2019_CHES_dataset_means.dta")
df_ches_2024 <- read_dta("CHES_2024_final_v2.dta")
df_parlgov_1 <- read_excel("parlgov-stable.xlsx")
df_parlgov_2 <- read_csv("view_election.csv")

#### Merging files -------------------------------------------------------
## Check overlap of variable names
setdiff(names(df_ches_trend), names(df_ches_2024))
setdiff(names(df_ches_2024), names(df_ches_trend))
## Remove labels
df_ches_trend <- zap_labels(df_ches_trend)
df_ches_2024 <- zap_labels(df_ches_2024)
## Appending
df_ches <- bind_rows(df_ches_trend, df_ches_2024)
## Merge parlgov into chess
names(df_ches)[names(df_ches) == "party_id"] <- "chess"
df_ches <- merge(df_ches, df_parlgov_1, by = "chess", all.x = TRUE)
## Merge parlgov_1 into parlgov_2
df_parlgov <- left_join(df_parlgov_2, df_parlgov_1, by = "party_id")



#### Map country numbers to names ---------------------------------------
country_codes <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 34, 35, 36, 37, 38, 40, 45)

country_names <- c("Belgium", "Denmark", "Germany", "Greece", "Spain", "France", "Ireland", "Italy", "Netherlands", "United Kingdom",
                   "Portugal", "Austria", "Finland", "Sweden", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
                   "Poland", "Romania", "Slovakia", "Slovenia", "Croatia", "Turkey", "Norway", "Switzerland", "Malta", "Luxembourg",
                   "Cyprus", "Iceland")
df_ches$country <- country_names[match(df_ches$country, country_codes)]


#### Map family numbers to names -----------------------------------------
family_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
family_names <- c("Radical Right", "Conservatives", "Liberal", "Christian-Democratic", "Socialist",
                  "Radical Left", "Green", "Regionalist", "No family", "Confessional", "Agrarian/Centre")
df_ches$family <- family_names[match(df_ches$family, family_ids)]





#### Basic cleaning -----------------------------------------------------
df_parlgov$election_year <- year(df_parlgov$date)
df_parlgov <- df_parlgov[ , !names(df_parlgov) %in% c(
  "created_at", "updated_at", "data_source", 
  "description", "comment", "data_json")]
df_parlgov <- subset(df_parlgov, type == "parliament")
# Rename 'Right-wing' to 'Radical right' in the family_name column of df_voteshares
df_parlgov$family_name <- replace(df_parlgov$family_name, df_parlgov$family_name == "Right-wing", "Radical right")


#### Multiple elections in a single year -------------------------------------
# Ensure election_year is present
df_parlgov$election_year <- year(df_parlgov$date)

# Filter to first election per country-year
df_first_election <- df_parlgov[order(df_parlgov$date), ]
df_first_election <- df_first_election[!duplicated(df_first_election[, c("country_name", "election_year", "party_id")]), ]

# Aggregate vote share per party family
vote_family_year <- aggregate(vote_share ~ country_name + election_year + family_name, 
                              data = df_first_election, sum, na.rm = TRUE)

# Normalize within each country-year
total_votes_year <- aggregate(vote_share ~ country_name + election_year, data = vote_family_year, sum)
vote_family_year <- merge(vote_family_year, total_votes_year, 
                          by = c("country_name", "election_year"), suffixes = c("", "_total"))

vote_family_year$vote_share_rel <- 100 * vote_family_year$vote_share / vote_family_year$vote_share_total
sum_check <- aggregate(vote_share_rel ~ country_name + election_year, 
                       data = vote_family_year, sum)

# Display the result
print(sum_check)



#### Correcting vote shares, Netherlands --------------------------------------------------
## Netherlands
# Check each election year 
df_test <- subset(df_parlgov, country_name == "Netherlands" & election_year >= 1950)
split_by_date <- split(df_test, df_test$date)
for (d in names(split_by_date)) {
  cat("\n--- Date:", d, "---\n")
  print(split_by_date[[d]])
}




#### Save CSV ----------------------------------------------------------------
write.csv(df_ches, "df_ches.csv", row.names = FALSE)
write.csv(df_parlgov, "df_parlgov.csv", row.names = FALSE)
write.csv(vote_family_year, "df_voteshares.csv", row.names = FALSE)


