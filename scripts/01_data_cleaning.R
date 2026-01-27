# This code describes the cleaning steps used on the raw data files for the PA and Fracture study.

# install packages

install.packages("data.table")
install.packages("dplyr")
install.packages("e1071")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("stringr")

#load libraries

library (data.table)
library (dplyr)
library (e1071)
library (ggplot2)
library (readr)
library(tidyr)
library(stringr)

# load the files for Assessment centre 0, and death

dat_raw <- fread("data_raw/participant_data.csv", data.table = FALSE) 
dat_death <- fread("data_raw/death_data.csv", data.table = FALSE)

# We start by keeping a subset of columns which we are likely to use from the main dat dataset:
  
dat <-
  dat_raw |>
  rename(
  "eid" = "eid",
  "date_assess_A0_raw" = "Date of attending assessment centre | Instance 0" ,
  "ukb_assess_center_raw" = "UK Biobank assessment centre | Instance 0",
  "age_A0_raw" = "Age when attended assessment centre | Instance 0",
  "year_birth_raw" = "yob",
  "month_birth_raw" = "mob",
  "sex_raw" = "sex",
  "ethnicity_raw" = "Ethnic background | Instance 0",
  "qualification_raw" = "Qualifications | Instance 0" ,
  "age_education_raw" = "Age completed full time education | Instance 0",
  "tdi_raw" = "townsend_raw",
  "waist_circ_raw" = "Waist circumference | Instance 0",
  "hip_circ_raw" = "Hip circumference | Instance 0",
  "weight_raw" = "Weight | Instance 0",
  "height_raw" = "p50_i0",
  "BMI_raw" = "Body mass index (BMI) | Instance 0",
  "lost_to_fu_raw" = "lost_to_fu",
  "grip_strength_L_raw" = "Hand grip strength (left) | Instance 0",
  "grip_strength_R_raw" = "Hand grip strength (right) | Instance 0",
  "reaction_time_raw" = "Mean time to correctly identify matches | Instance 0",
  "fall_fracture_raw" = "Fracture resulting from simple fall | Instance 0",
  "self_reported_falls_raw" = "Falls in the last year | Instance 0",
  "self_reported_fracture_A0_raw" = "Fractured/broken bones in last 5 years | Instance 0",
  "fracture_location_A0_raw" = "Fractured bone site(s) | Instance 0",
  "duration_mod_raw" = "Duration of moderate activity | Instance 0",
  "duration_vig_raw" = "Duration of vigorous activity | Instance 0",
  "duration_walk_raw" = "Duration of walks | Instance 0",
  "num_days_mod_raw" = "Number of days/week of moderate physical activity 10+ minutes | Instance 0",
  "num_days_vig_raw" = "Number of days/week of vigorous physical activity 10+ minutes | Instance 0",
  "num_day_walk_raw" = "Number of days/week walked 10+ minutes | Instance 0",
  "usual_walking_pace_raw" = "Usual walking pace | Instance 0",
  "MV_rec_raw" = "At or above moderate/vigorous recommendation | Instance 0",
  "MV_walk_rec_raw" = "At or above moderate/vigorous/walking recommendation | Instance 0",
  "IPAQ_group_raw" = "IPAQ Activity Group | Instance 0",
  "MET_mod_raw" = "MET minutes per week for moderate activity | Instance 0",
  "MET_vig_raw" = "MET minutes per week for vigorous activity | Instance 0",
  "MET_walk_raw" = "MET minutes per week for walking | Instance 0",
  "summed_MET_all_raw" = "Summed MET minutes per week for all activity | Instance 0",
  "summed_days_all_raw" = "Summed days activity | Instance 0",
  "Summed_mins_all_raw" = "Summed minutes activity | Instance 0",
  "light_average_raw" = "Light - Overall average | Instance 0",
  "mod_average_raw" = "Moderate-Vigorous - Overall average | Instance 0",
  "sed_average_raw" = "Sedentary - Overall average | Instance 0",
  "quality_good_cal_raw" = "quality_good_cal",
  "clips_before_cal_raw" = "clips_before_cal",
  "clips_after_cal_raw" = "clips_after_cal",
  "total_reads_raw" = "total_reads",
  "good_quality_wear_time_raw" = "good_quality_wear_time",
  "Overall_activity_acceleration_raw" = "Overall_activity_acceleration",
  "start_date_accel_raw" = "start_date_accel",
  "date_end_accel_raw" = "date_end_accel",
  "wear_duration_raw" = "wear_duration",
  "Arm_LM_left_raw" = "Arm_LM_left_raw",
  "Arm_LM_right_raw" = "Arm_LM_right_raw",
  "Leg_LM_left_raw" = "Leg_LM_left_raw",
  "Leg_LM_right_raw" = "Leg_LM_right_raw",
  "TBMD_T_score_raw" = "TBMD_T_score",
  "head_BMD_raw" = "head_BMD_raw",
  "FN_BMD_left_raw" = "FN_BMD_left_raw",
  "FN_BMD_right_raw" = "FN_BMD_right_raw",
  "FN_T_score_left_raw" = "FN_T_score_left",
  "FN_T_score_right_raw" = "FN_T_score_right"
)


  
# We inspect the data structure to check all columns are the types we expect:

for (data in list(dat, dat_death)){
  str(data, vec.len = 0) # vec.len = 0 avoids accidentally printing data
}

#  "age_education_raw" formatted as character: Needs recoding to allow analysis 
# There are some special values for [age completed full time education](https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=845),
# which need to be removed before it can be coerced to numeric. Let's reformat it appropriately.

# Education 
# Step 1: convert to character and trim
dat <- dat %>%
  mutate(age_education_clean = as.character(age_education_raw),
         age_education_clean = str_trim(age_education_clean))

# Step 2: replace known special cases
dat <- dat %>%
  mutate(age_education_clean = ifelse(
    age_education_clean %in% c("Do not know", "Prefer not to answer", ""), 
    NA, age_education_clean
  ),
  age_education_clean = ifelse(age_education_clean == "Never went to school", 0, age_education_clean)
  )

# Step 3: now safely coerce only remaining numeric strings
dat <- dat %>%
  mutate(age_education_clean = as.numeric(age_education_clean))


# Other variables also need cleaning so NA are accurately coded as one group

#Ethnicity
dat <- dat %>%
  mutate(
    ethnicity_clean = as.character(ethnicity_raw),            # ensure it's character
    ethnicity_clean = str_trim(ethnicity_clean),             # remove spaces
    ethnicity_clean = ifelse(
      ethnicity_clean %in% c("Do not know", "Prefer not to answer", ""), 
      NA, 
      ethnicity_clean
    )
  )

# data check 
table(dat$ethnicity_clean, useNA = "ifany")

# Fracture

# 1 Clean self-reported fracture column
dat <- dat %>%
  mutate(
    self_reported_fracture_A0_clean = as.character(self_reported_fracture_A0_raw),  # ensure character
    self_reported_fracture_A0_clean = str_trim(self_reported_fracture_A0_clean),   # remove spaces
    self_reported_fracture_A0_clean = ifelse(
      self_reported_fracture_A0_clean %in% c("Do not know", "Prefer not to answer", ""),
      NA_character_,
      self_reported_fracture_A0_clean
    )
  ) 

# Clean fracture location column

dat <- dat %>%
  mutate(
    fracture_location_A0_clean = as.character(fracture_location_A0_raw),
    fracture_location_A0_clean = str_trim(fracture_location_A0_clean),
    fracture_location_A0_clean = ifelse(
      fracture_location_A0_clean %in% c("Do not know", "Prefer not to answer", ""),
      NA_character_,
      fracture_location_A0_clean
    )
  )


# The MET catagories are derived so dont themselves have any missing data but in order to calculate how much
# of the population had complete data that contributed to the MET calculations the intensity and quantity of PA is used and needs cleaned

vars <- c(
  "duration_mod_raw",
  "num_days_mod_raw",
  "duration_vig_raw",
  "num_days_vig_raw",
  "duration_walk_raw",
  "num_day_walk_raw"
)

library(stringr)

clean_numeric <- function(x) {
  # 1️⃣ Convert factors to character
  x <- as.character(x)
  
  # 2️⃣ Trim whitespace
  x <- str_trim(x)
  
  # 3️⃣ Replace non-responses with NA
  x[x %in% c("Do not know", "Prefer not to answer", "")] <- NA
  
  # 4️⃣ Keep strings that are numeric (allow 0, decimals)
  numeric_mask <- str_detect(x, "^(\\d+\\.?\\d*|\\d*\\.\\d+)$")
  x[!numeric_mask] <- NA
  
  # 5️⃣ Convert to numeric
  as.numeric(x)
}


# Add all cleaned data in as _clean in table

for (v in vars) {
  new_name <- sub("_raw$", "_clean", v)
  dat[[new_name]] <- clean_numeric(dat[[v]])
}

# Check data to see how much missing from each PA catagory - looks like MET mins has some imputation when num days complete but duration not

completeness <- sapply(
  dat[c("duration_mod_clean", "num_days_mod_clean", "MET_mod_raw")],
  function(x) {
    c(
      "NA" = sum(is.na(x)),
      "non_NA" = sum(!is.na(x))
    )
  }
)

both_non_NA <- sum(
  !is.na(dat$duration_mod_clean) &
    !is.na(dat$num_days_mod_clean)
)

completeness
both_non_NA

# check 0 values preserved
unique(dat$num_days_mod_clean)

# IPAQ If no numeric response add NA

# Create a cleaned copy of the original column
dat$IPAQ_clean <- as.character(dat$IPAQ_group_raw)       # make sure it’s character
dat$IPAQ_clean[trimws(dat$IPAQ_clean) == ""] <- NA       # replace empty strings with NA

# Check the cleaned data
table(dat$IPAQ_clean, useNA = "ifany")

# MET Mod clean
# Create a cleaned copy of the original column
dat$MET_mod_clean <- as.character(dat$MET_mod_raw)       # make sure it’s character
dat$MET_mod_clean[trimws(dat$MET_mod_clean) == ""] <- NA       # replace empty strings with NA

table(dat$MET_mod_clean, useNA = "ifany")

# MET Vig clean
dat$MET_vig_clean <- as.character(dat$MET_vig_raw)       # make sure it’s character
dat$MET_vig_clean[trimws(dat$MET_vig_clean) == ""] <- NA       # replace empty strings with NA


table(dat$MET_vig_clean, useNA = "ifany")

# MET Walk clean
dat$MET_walk_clean <- as.character(dat$MET_walk_raw)       # make sure it’s character
dat$MET_walk_clean[trimws(dat$MET_walk_clean) == ""] <- NA       # replace empty strings with NA


table(dat$MET_walk_clean, useNA = "ifany")


# Anthropometrics
vars <- c(
  "height_raw",
  "weight_raw",
  "waist_circ_raw",
  "BMI_raw"
)

clean_numeric <- function(x) {
  # 1️⃣ Convert factors to character
  x <- as.character(x)
  
  # 2️⃣ Trim whitespace
  x <- str_trim(x)
  
  # 3️⃣ Replace non-responses with NA
  x[x %in% c("Do not know", "Prefer not to answer", "", NA)] <- NA
  
  # 4️⃣ Only keep strings that are purely numeric
  numeric_mask <- str_detect(x, "^\\d*\\.?\\d+$")  # integers or decimals
  x[!numeric_mask] <- NA
  
  # 5️⃣ Convert to numeric
  as.numeric(x)
}

# Add all cleaned data in as _clean in table

for (v in vars) {
  new_name <- sub("_raw$", "_clean", v)
  dat[[new_name]] <- clean_numeric(dat[[v]])
}

# Check data to see how much missing from each PA catagory - looks like MET mins is just when complete data 
# in both catagories

completeness <- sapply(
  dat[c("height_clean", "weight_clean", "waist_circ_clean", "BMI_clean")],
  function(x) {
    c(
      "NA" = sum(is.na(x)),
      "non_NA" = sum(!is.na(x))
    )
  }
)

completeness


#Preparation for time to event analysis

# # We also do some simple formatting of date columns:
#   
# # Tabular participant data
# dat$date_assess_A0_raw <-as.Date(dat$date_assess_A0_raw, format = "%Y-%m-%d")
# dat$lost_to_fu_raw <- as.Date(dat$lost_to_fu_raw, format = "%Y-%m-%d")
# dat$date_end_accel <- as.Date(dat$date_end_accel, format = "%Y-%m-%d")
#  

# Create a new dataset ready for derivations and save cleaned outputs

# All column names
all_cols <- names(dat)

# Columns that have _clean
clean_cols <- all_cols[grepl("_clean$", all_cols)]

# Columns that have _raw
raw_cols <- all_cols[grepl("_raw$", all_cols)]

# Keep only raw columns that don't have a corresponding _clean
raw_cols_to_use <- raw_cols[!sub("_raw$", "_clean", raw_cols) %in% clean_cols]

# Combine clean + remaining raw
cols_for_clean <- c("eid", clean_cols, raw_cols_to_use)

# Pull into a new dataset for derivation
baseline_clean <- dat %>%
  select(all_of(cols_for_clean))

# Save the clean dataset
saveRDS(baseline_clean, "data_derived/baseline_clean.Rds")


########################################################################################################################

# Death data
dat_death$date_death <-
  as.Date(dat_death$date_of_death, format = "%Y-%m-%d")
# A very small number of participants have duplicate records in death data (e.g. perhaps from a second death certificate after post-mortem)
# In this dataset we keep just one record per participant: they should have the same date, and we will use the death_cause dataset for any 
# other records related to death. It also only affects a very small number of participants.

dat_death <-
  dat_death[dat_death$ins_index == 0, ]

# Save cleaned outputs
saveRDS(dat_death, "data_derived/death_clean.Rds")

################################################################################################################################

# HES data
# hes is a combination of admissions and diagnosis details merged prior to download. These contain all of the ICD-10 and ICD-9 codes for participants in the UKBiobank.
# Load data
hes <- fread("data_raw/hes_data.csv", data.table=F, na.strings="")

# To make data handling easier I have made a new data frame with just the eid, episode start date (epistart), the admission date (admidate), 
# and the newly processed ICD-10 code. This is saved to data_clean file as dat_hes_clean.csv to use in further processing/assessment.

# Extract columns eid, epistart, admidate and ICD10_CODE into new df called hes_diag

hes_diag <- hes %>%
  select(eid, epistart, admidate,date_injury, diag_icd10, diag_icd9)

# Event data
# I have created a 'date_injury' at which the fracture event occured. For this I have used the admissions date (admidate).
# Where this was not available I have used the episode start date (epistart).

# Select out only the relevant orthopaedic codes based on the pre-specified analysis plan.

## First filter to keep only the codes beginning with S
# Filter rows where ICD10_Code starts with 'S'
hes_filtered <- hes_diag %>%
  filter(
    grepl("^S", diag_icd10) | grepl("^S", diag_icd9)
  )

#UKB removes the decimal place, for filtering out of non fracture ortho codes we need to put it back in.

# Helper function: insert decimal after 3 characters if longer than 3
insert_icd10_decimal <- function(x) {
  x <- as.character(x)          # ensure character
  is_na <- is.na(x)             # keep track of NAs
  n <- nchar(x)
  
  # Only modify non-NA values longer than 3
  long_idx <- which(!is_na & n > 3)
  x[long_idx] <- paste0(
    substr(x[long_idx], 1, 3),
    ".",
    substr(x[long_idx], 4, n[long_idx])
  )
  
  x
}

# Apply to your filtered HES dataframe
hes_filtered <- hes_filtered %>%
  mutate(
    diag_icd10_full = insert_icd10_decimal(diag_icd10),
    diag_icd9_full  = insert_icd10_decimal(diag_icd9)
  )


HES_trimmed <- hes_filtered %>%
  mutate(
    diag_icd10_trimmed = sub("\\.([0-9])[0-9]+$", ".\\1", diag_icd10_full),
    diag_icd9_trimmed  = sub("\\.([0-9])[0-9]+$", ".\\1", diag_icd9_full)
  )


# Define the Ortho_codes regex patterns
Ortho_codes <- c(
  "S02\\.[0-9]",  # frac_head not tooth
  "S12\\.[0-2]|S12\\.[7-9]",  # frac_Cx
  "S22\\.[0-5]|S22\\.[8-9]",  # frac_Tx
  "S32\\.[0-5]|S32\\.[7-8]",  # frac_Lx_pelvis
  "S42\\.[0-4]|S42\\.[7-9]",  # frac_Sh_arm
  "S52\\.[0-9]",  # frac_Elb_farm
  "S62\\.[0-8]",  # frac_Hand_carpal
  "S72\\.[0-4]|S72\\.[7-9]",  # frac_Hip_thigh
  "S82\\.[0-9]",  # frac_Knee_LL
  "S92\\.[0-5]|S72\\.[7-9]"  # frac_Ankle_ft
)

# Filter rows based on matching ICD10_Code with regex patterns in Ortho_codes
HES_filtered <- HES_trimmed %>%
  filter(
    sapply(1:nrow(.), function(i) {
      code10 <- diag_icd10_trimmed[i]
      code9  <- diag_icd9_trimmed[i]
      any(sapply(Ortho_codes, function(pattern) grepl(pattern, code10))) |
        any(sapply(Ortho_codes, function(pattern) grepl(pattern, code9)))
    })
  )

num_unique_eid <- length(unique(HES_filtered$eid))
print(num_unique_eid)


### find duplicates in diag_icd10 ###

HES_check_duplicates <- HES_filtered %>%
  group_by(eid, date_injury, diag_icd10_trimmed) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# View the result
View(HES_check_duplicates)

num_unique_eid <-length(unique(HES_check_duplicates$eid))
print(num_unique_eid)

#### remove duplicated data i.e exact matches that have been entered more than once for the same date

# Remove duplicates, keeping only the first occurrence
HES_filtered_no_duplicates <- HES_filtered %>%
  group_by(eid, date_injury) %>%
  filter(!duplicated(diag_icd10_trimmed)) %>%
  ungroup()

num_unique_eid <-length(unique(HES_filtered_no_duplicates$eid))
print(num_unique_eid)

# Repeat for ICD9

HES_check_duplicates <- HES_filtered %>%
  group_by(eid, date_injury, diag_icd9_trimmed) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# View the result
View(HES_check_duplicates)

num_unique_eid <-length(unique(HES_check_duplicates$eid))
print(num_unique_eid)

#### remove duplicated data i.e exact matches that have been entered more than once for the same date

# Remove duplicates, keeping only the first occurrence
HES_filtered_no_duplicates <- HES_filtered %>%
  group_by(eid, date_injury) %>%
  filter(!duplicated(diag_icd9_trimmed)) %>%
  ungroup()

num_unique_eid <-length(unique(HES_filtered_no_duplicates$eid))
print(num_unique_eid)

# Washout 

# As patients are often re-admitted with the same condition, i.e a hip fracture can be re-admitted with a second fall, 
# the ICD-10 code can appear twice in quick succession. This will have an effect on any incidence calculations. 
# To avoid this it is usual practice to apply a washout period where any repeat of the code is removed. 
# This does run the risk of under reporting as a second fracture is common within a year of a first event but the washout approach is a pragmatic solution. 
# I have applied a one year washout period.

# Convert startdate to Date format (if not already)
HES_clean_ICD10 <- HES_filtered_no_duplicates %>%
  mutate(date_injury = as.Date(date_injury)) %>%
  
  # Group by EID and Ortho_code
  group_by(eid, diag_icd10_trimmed) %>%
  
  # Sort entries by startdate within each group
  arrange(date_injury, .by_group = TRUE) %>%
  
  # Apply washout period logic
  mutate(
    keep_entry = case_when(
      is.na(lag(date_injury)) ~ TRUE,  # Keep the first occurrence
      (date_injury - lag(date_injury)) > 365 ~ TRUE,  # Keep if more than 1 year apart
      TRUE ~ FALSE  # Exclude if within 1 year of the previous entry
    )
  ) %>%
  
  # Keep only the entries marked as TRUE
  filter(keep_entry) %>%
  
  # Drop temporary column
  select(-keep_entry) %>%
  
  # Ungroup the data
  ungroup()

# and ICD9
HES_clean <- HES_clean_ICD10 %>%
  mutate(date_injury = as.Date(date_injury)) %>%
  
  # Group by EID and Ortho_code
  group_by(eid, diag_icd9_trimmed) %>%
  
  # Sort entries by startdate within each group
  arrange(date_injury, .by_group = TRUE) %>%
  
  # Apply washout period logic
  mutate(
    keep_entry = case_when(
      is.na(lag(date_injury)) ~ TRUE,  # Keep the first occurrence
      (date_injury - lag(date_injury)) > 365 ~ TRUE,  # Keep if more than 1 year apart
      TRUE ~ FALSE  # Exclude if within 1 year of the previous entry
    )
  ) %>%
  
  # Keep only the entries marked as TRUE
  filter(keep_entry) %>%
  
  # Drop temporary column
  select(-keep_entry) %>%
  
  # Ungroup the data
  ungroup()


num_unique_eid <-length(unique(HES_clean$eid))
print(num_unique_eid)

saveRDS(HES_clean, "data_derived/hes_clean.Rds")

# data check
# To make sure this is the most recent HES set we will now group by injury date from most to least recent
HES_clean <- HES_clean %>%
  arrange(desc(date_injury))

          View(HES_clean)

###########################################################################################

# No cleaning applied to accelerometry data - this will be processed in the next step

