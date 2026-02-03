# This source file specifies the libraries and file paths requried for the project.

# 1. Load packages
packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "readr",
  "dotenv",
  "here",
  "data.table",
  "ggplot2",
  "stringr",
  "purrr",
  "e1071",
  "table1"
)



# Install missing packages if needed
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])

# Load packages
lapply(packages, library, character.only = TRUE)

# 2.  Load environment variables from config.env
dotenv::load_dot_env(here::here("config.env"))

# Assign environment variables to R variables
DATA_RAW     <- Sys.getenv("DATA_RAW")
DATA_DERIVED <- Sys.getenv("DATA_DERIVED")


