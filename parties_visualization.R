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
df_parlgov <- read_csv("df_parlgov.csv")
df_ches <- read_csv("df_ches.csv")
df_voteshares <- read_csv("df_voteshares.csv")



#### Formatting plots -----------------------------------------------------
# Define a more representative color palette based on party family associations
custom_colors <- c("Radical right" = "#FF5733",        # Red
                   "Christian democracy" = "#4682B4", # Blue
                   "Communist/Socialist" = "#FF0000", # Red
                   "Social democracy" = "#20B2AA",   # Light teal/blue
                   "Other" = "#D3D3D3",              # Light gray
                   "Conservative" = "#006400",       # Dark green
                   "Liberal" = "#8A2BE2",            # Purple
                   "Green/Ecologist" = "#00FF00",    # Green
                   "Agrarian" = "#FFD700")           # Gold (for Agrarian)
# Define global consistent order for party families
family_order <- c("Christian democracy", "Conservative", "Liberal", 
                  "Social democracy", "Communist/Socialist", "Green/Ecologist", 
                  "Radical right", "Agrarian", "Other")


#### Plotting vote shares  ----------------------------------------------
## Netherlands
# Subset data
nl_data <- subset(df_voteshares, country_name == "Netherlands" & election_year >= 1950)
nl_data <- subset(nl_data, family_name != "no family" & family_name != "Special issue")
nl_data$family_name <- factor(nl_data$family_name, levels = family_order)

# Plot
ggplot(nl_data, aes(x = election_year, y = vote_share_rel, fill = family_name)) +
  geom_area() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Vote Share by Party Family in the Netherlands (Excluding 'Other')",
       x = "Election Year", y = "Relative Vote Share (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 100))


## Denmark
# Subset data
dk_data <- subset(df_voteshares, country_name == "Denmark" & election_year >= 1950)
dk_data <- subset(dk_data, family_name != "no family" & family_name != "Special issue")
dk_data$family_name <- factor(dk_data$family_name, levels = family_order)

# Plot
ggplot(dk_data, aes(x = election_year, y = vote_share_rel, fill = family_name)) +
  geom_area() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Vote Share by Party Family in Denmark (Excluding 'Other')",
       x = "Election Year", y = "Relative Vote Share (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 100))

## Germany
# Subset data
de_data <- subset(df_voteshares, country_name == "Germany" & election_year >= 1950)
de_data <- subset(de_data, family_name != "no family" & family_name != "Special issue")
de_data$family_name <- factor(de_data$family_name, levels = family_order)

# Plot
ggplot(de_data, aes(x = election_year, y = vote_share_rel, fill = family_name)) +
  geom_area() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Vote Share by Party Family in Germany (Excluding 'Other')",
       x = "Election Year", y = "Relative Vote Share (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 100))



