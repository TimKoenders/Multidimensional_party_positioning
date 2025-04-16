#### Clean-up, packages and functions ----------------------------------------------------------------
## Clean-up
rm(list = ls())

## Function for packages
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

## Function for summary statistics
summary_by <- function(data, vars, by = NULL, print_result = TRUE) {
  if (!is.character(vars)) stop("vars must be a character vector of variable names")
  
  summary_fun <- function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    min  = min(x, na.rm = TRUE),
    max  = max(x, na.rm = TRUE),
    n    = sum(!is.na(x))
  )
  
  result_list <- lapply(vars, function(var) {
    if (is.null(by)) {
      stats <- summary_fun(data[[var]])
      out <- data.frame(variable = var, t(stats), row.names = NULL)
    } else {
      temp <- aggregate(data[[var]], by = data[by], FUN = summary_fun)
      stats_df <- as.data.frame(temp$x)
      colnames(stats_df) <- c("mean", "sd", "min", "max", "n")
      out <- cbind(temp[by], variable = var, stats_df)
    }
    return(out)
  })
  
  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  
  options(max.print = 1e6)  # ensure full output always prints
  if (print_result) print(result)
  
  invisible(result)
}


#### Importing data ----------------------------------------------------------
df_core <- read_csv("df_core.csv")

#### Summary statistics -------------------------------------------------
summary_by(df_core, vars = c("deregulation", "redistribution"), by = "family")





