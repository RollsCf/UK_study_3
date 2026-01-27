# This code describes the derivation of variables for use in the final analysis - it uses variables created in 01_data_cleaning.R
# Variables that do not need derivation are recoded for ease in analysis ie BMI_raw to BMI

# install packages

install.packages("data.table")
install.packages("dplyr")
install.packages("e1071")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("purrr")
install.packages("stringr")

#load libraries

library (data.table)
library (dplyr)
library (e1071)
library (ggplot2)
library (readr)
library(tidyr)
library(purrr)
library(stringr)

# load the files for Assessment centre 0, HES and death, 

# Load cleaned datasets
dat <- readRDS("data_derived/baseline_clean.Rds")
death    <- readRDS("data_derived/death_clean.Rds")
hes      <- readRDS("data_derived/hes_clean.Rds")


# Derivations in baseline data

# Print column names
names(dat)

# If no derivation needed then drop the word _raw for the analysis to keep coding tight

dat$ethnicity_derived <- case_when(
  dat$ethnicity_clean %in% c(
    "White", "British", "Irish", "Any other white background"
  ) ~ "White",
  
  dat$ethnicity_clean %in% c(
    "Mixed",
    "White and Black Caribbean",
    "White and Black African",
    "White and Asian",
    "Any other mixed background"
  ) ~ "Mixed",
  
  dat$ethnicity_clean %in% c(
    "Asian or Asian British",
    "Indian",
    "Pakistani",
    "Bangladeshi",
    "Any other Asian background"
  ) ~ "Asian",
  
  dat$ethnicity_clean %in% c(
    "Black or Black British",
    "Caribbean",
    "African",
    "Any other Black background",
    "Chinese",
    "Other ethnic group"
  ) ~ "Black or other",
  
  TRUE ~ NA_character_
)

## Check
table(dat$ethnicity_derived, useNA = "ifany")

# UKB assessment centre
# Field 54	# Assessment Centre location (A0)
# To allow me to look at geographic variation I re-coded Assessment centre by Country it was located in.
# Codes were as follows

dat$assessment_country <- case_when(
  dat$ukb_assess_center_raw %in% c(
    "Barts", "Birmingham", "Bristol", "Bury", "Cheadle",
    "Croydon", "Hounslow", "Leeds", "Liverpool", "Middlesbrough",
    "Newcastle", "Nottingham", "Oxford", "Reading", "Sheffield",
    "Stockport", "Stoke", "Sunderland", "Warrington",
    "Wilmslow", "Wolverhampton",
    "Cheadle (Greater Manchester)",
    "Stockport (Greater Manchester)"
  ) ~ "England",
  
  dat$ukb_assess_center_raw %in% c(
    "Edinburgh", "Glasgow"
  ) ~ "Scotland",
  
  dat$ukb_assess_center_raw %in% c(
    "Cardiff", "Swansea"
  ) ~ "Wales",
  
  TRUE ~ NA_character_
)


# Create middle aged cohort by activity measure

# create agegroup_A0 based on age_A0_raw

middle_aged_A0_dat <- dat %>%
  filter(between(age_A0_raw, 40, 65))

# Create age at A0 groups
dat$agegp_A0 <-
  cut(
    dat$age_A0_raw,
    breaks = c(40, 50, 60, 70, 80),
    right = FALSE,
    labels = c("40-49", "50-59", "60-69", "70-79")
  )

# create age-at-accelerometer-wear variable:
  
# Add date of birth
dat$approx_dob_derived <-
  as.Date(paste(dat$yob, dat$mob, "15", sep = "-"),
          "%Y-%B-%d") # UK Biobank doesn't contain day of birth as it would be unnecessary identifying information, so we roughly impute it as the 15th of the birth month.

# Add age at accel entry in days
dat$age_accel_entry_days_derived <-
  difftime(dat$date_end_accel_raw,
           dat$approx_dob,
           units = "days")

# Convert to age at entry in years
dat$age_accel_entry_years <- as.double(dat$age_accel_entry_days_derived)/365.25

# Create mid-life accel group

middle_aged_accel_dat <- dat %>%
  filter(between(age_accel_entry_years, 40, 65))

# Create age at accel groups
dat$age_gp_accel <-
  cut(
    dat$age_accel_entry_years,
    breaks = c(40, 50, 60, 70, 80),
    right = FALSE,
    labels = c("40-49", "50-59", "60-69", "70-79")
  )

# derive new education levels
# define education level ranking
edu_levels <- c(
  "School education or less" = 1,
  "College or vocational training" = 2,
  "University degree or above" = 3
)

# derive new education levels
derive_education_level <- function(qual_vec) {
  sapply(qual_vec, function(x) {
    
    if (is.na(x)) return(NA_character_)
    
    # Split multiple entries by '|', trim whitespace, lowercase
    quals <- trimws(tolower(unlist(strsplit(x, "\\|"))))
    
    # Map each qualification to a category
    mapped <- sapply(quals, function(q) {
      if (q %in% tolower("College or University degree")) {
        "University degree or above"
      } else if (q %in% tolower(c(
        "NVQ or HND or HNC or equivalent",
        "Other professional qualifications eg: nursing, teaching"
      ))) {
        "College or vocational training"
      } else if (q %in% tolower(c(
        "A levels/AS levels or equivalent",
        "O levels/GCSEs or equivalent",
        "CSEs or equivalent",
        "None of the above"
      ))) {
        "School education or less"
      } else {
        NA_character_
      }
    })
    
    # Pick the highest-ranked category
    if (all(is.na(mapped))) {
      NA_character_
    } else {
      names(edu_levels)[which.max(edu_levels[mapped])]
    }
  })
}

# apply function
dat$education_level <- derive_education_level(dat$qualification_raw)

# check
table(dat$education_level, useNA = "ifany")

# convert to ordered factor
dat$education_level <- factor(
  dat$education_level,
  levels = names(edu_levels),
  ordered = TRUE
)


# Anthropometrics

# # Derived variables based on WHO guidance
# WHO recommends WC as a measure of abdominal (central) obesity, because it strongly correlates with visceral fat and cardiometabolic risk.

# Cut-offs for Europeans (typical WHO guidance):
# Sex	  Normal	  Increased risk	    Substantially increased risk
# Men	    <94 cm	  94–102 cm	          >102 cm
# Women	  <80 cm	  80–88 cm	          >88 cm

# create variable
dat$central_obesity_derived <- case_when(
  dat$sex_raw == "Male" & dat$waist_circ_clean >= 102 ~ "Substantially increased risk",
  dat$sex_raw == "Male" & dat$waist_circ_clean >= 94  ~ "Increased risk",
  dat$sex_raw == "Male" & dat$waist_circ_clean < 94   ~ "Normal",
  dat$sex_raw == "Female" & dat$waist_circ_clean >= 88 ~ "Substantially increased risk",
  dat$sex_raw == "Female" & dat$waist_circ_clean >= 80 ~ "Increased risk",
  dat$sex_raw == "Female" & dat$waist_circ_clean < 80  ~ "Normal",
  TRUE ~ NA_character_   # for missing or unknown sex
)

# Create BMI catagories based on non sex specific WHO cut offs
dat$BMI_category_derived <- case_when(
  dat$BMI_clean < 25               ~ "Normal",
  dat$BMI_clean >= 25 & dat$BMI_clean < 30 ~ "Overweight",
  dat$BMI_clean >= 30              ~ "Obese",
  TRUE ~ NA_character_           # for missing or invalid values
)

#### Physical activity measures ####

# In UKB MET mins is derived from num days of activity and an imputed value when duration is missing.
# For the complete case analysis we will create our own variables of mins/wk and METmins/wk with just complete data

# Complete case mins/wk mod activity (cc_mins_wk_mod)

dat <- dat %>%
  mutate(
    cc_mins_wk_mod = ifelse(
      !is.na(duration_mod_clean) & !is.na(num_days_mod_clean),
      duration_mod_clean * num_days_mod_clean,
      NA_real_
    )
  )
# Complete case MET mins/wk mod (cc_MET_mod)

dat <- dat %>%
  mutate(
    cc_MET_mod = ifelse(
      !is.na(cc_mins_wk_mod),
      cc_mins_wk_mod * 4,
      NA_real_
    )
  )

# sense check - compare complete case to MET_mod and see how many eids it is different

# Filter rows where the two values differ
differences <- dat %>%
  filter(!is.na(cc_MET_mod) & !is.na(MET_mod_clean)) %>%  # only compare non-missing
  filter(cc_MET_mod != MET_mod_clean)

# Count total differences
n_different <- nrow(differences)

# Count how many of those are NA or 0 in either column
n_na_or_zero <- differences %>%
  filter(cc_MET_mod == 0 | MET_mod_clean == 0) %>%
  nrow()

n_different
n_na_or_zero

# Complete case mins/wk vig activity (cc_mins_wk_vig)

dat <- dat %>%
  mutate(
    cc_mins_wk_vig = ifelse(
      !is.na(duration_vig_clean) & !is.na(num_days_vig_clean),
      duration_vig_clean * num_days_vig_clean,
      NA_real_
    )
  )
# Complete case MET mins/wk vig (cc_MET_vig)

dat <- dat %>%
  mutate(
    cc_MET_vig = ifelse(
      !is.na(cc_mins_wk_vig),
      cc_mins_wk_vig * 8,
      NA_real_
    )
  )

# Complete case mins/wk mod activity (cc_mins_wk_MVPA)

dat <- dat %>%
  mutate(
    cc_mins_wk_MVPA = ifelse(
      !is.na(cc_mins_wk_mod) & !is.na(cc_mins_wk_vig),
      cc_mins_wk_mod + cc_mins_wk_vig,
      NA_real_
    )
  )

# Complete case MET mins/wk mod (cc_MET_MVPA)

dat <- dat %>%
  mutate(
    cc_MET_MVPA = ifelse(
      !is.na(cc_MET_mod) & !is.na(cc_MET_vig),
      cc_MET_mod + cc_MET_vig,
      NA_real_
    )
  )

# Complete case mins/wk walking activity (cc_mins_wk_walk)

dat <- dat %>%
  mutate(
    cc_mins_wk_walk = ifelse(
      !is.na(duration_walk_clean) & !is.na(num_day_walk_clean),
      duration_walk_clean * num_day_walk_clean,
      NA_real_
    )
  )
# Complete case MET mins/wk walk (cc_MET_walk)

dat <- dat %>%
  mutate(
    cc_MET_walk = ifelse(
      !is.na(cc_mins_wk_walk),
      cc_mins_wk_walk * 3.3,
      NA_real_
    )
  )

# Complete case mins/wk summed activity (cc_mins_wk_summed)
dat <- dat %>%
  mutate(
    cc_mins_wk_summed = ifelse(
      !is.na(cc_mins_wk_walk) & !is.na(cc_mins_wk_mod) & !is.na(cc_mins_wk_vig),
      cc_mins_wk_walk + cc_mins_wk_mod + cc_mins_wk_vig,
      NA_real_
    )
  )

# Complete case MET mins/wk summed (cc_MET_summed)

dat <- dat %>%
  mutate(
    cc_MET_summed = ifelse(
      !is.na(cc_MET_walk) & !is.na(cc_MET_MVPA),
      cc_MET_walk + cc_MET_MVPA,
      NA_real_
    )
  )


# Self-reported physical activity for sensitivity (this is not complete case but uses provided data)
# Combine M and V PA into MVPA for use in later analyses compared to MVPA in accel
# if either mod or vig are NA then add 0, but if both are NA make NA

dat$MET_mod_clean_num <- suppressWarnings(as.numeric(dat$MET_mod_clean))
dat$MET_vig_clean_num <- suppressWarnings(as.numeric(dat$MET_vig_clean))
dat$MET_walk_clean_num <- suppressWarnings(as.numeric(dat$MET_walk_clean))

dat$MVPA_derived <- ifelse(
  is.na(dat$MET_mod_clean_num) & is.na(dat$MET_vig_clean_num),
  NA_real_,
  coalesce(dat$MET_mod_clean_num, 0) +
    coalesce(dat$MET_vig_clean_num, 0)
)

summary(dat$MVPA_derived)
sum(is.na(dat$MVPA_derived))

dat$MMVPA_derived_num <- suppressWarnings(as.numeric(dat$MVPA_derived))

# Create a mins/wk of activity variable for comparison with accel wear


###### Outcome data variables

fracture_sites <- c("Ankle", "Leg", "Hip", "Spine", "Wrist", "Arm", "Other bone")

dat <- dat %>%
  mutate(
    fracture_list = str_split(fracture_location_A0_clean, "\\|"),
    
    Ankle = case_when(
      map_lgl(fracture_list, ~ "Ankle" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    Leg = case_when(
      map_lgl(fracture_list, ~ "Leg" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    Hip = case_when(
      map_lgl(fracture_list, ~ "Hip" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    Spine = case_when(
      map_lgl(fracture_list, ~ "Spine" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    Wrist = case_when(
      map_lgl(fracture_list, ~ "Wrist" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    Arm = case_when(
      map_lgl(fracture_list, ~ "Arm" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    `Other bone` = case_when(
      map_lgl(fracture_list, ~ "Other bone" %in% .x) ~ 1L,
      self_reported_fracture_A0_clean %in% c("Yes", "No") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-fracture_list)



### HES fracture measures ####

# Based on previous work we will use 2 fracture catagories

#  Wrist
#  Hip

# with sensitivity analysis on any
 
hes$All_fracture_hes <- ifelse(
  grepl(
    paste(
      "S02\\.[0-9]",                  # frac_head (not tooth)
      "S12\\.[0-2]|S12\\.[7-9]",       # frac_Cx
      "S22\\.[0-5]|S22\\.[8-9]",       # frac_Tx
      "S32\\.[0-5]|S32\\.[7-8]",       # frac_Lx_pelvis
      "S42\\.[0-4]|S42\\.[7-9]",       # frac_Sh_arm
      "S52\\.[0-9]",                   # frac_Elb_farm
      "S62\\.[0-8]",                   # frac_Hand_carpal
      "S72\\.[0-4]|S72\\.[7-9]",       # frac_Hip_thigh
      "S82\\.[0-9]",                   # frac_Knee_LL
      "S92\\.[0-5]|S92\\.[7-9]",       # frac_Ankle_ft
      sep = "|"
    ),
    hes$diag_icd10_full
  ),
  1, 0
)

# Wrist

hes$Wrist_HES <- ifelse(
  grepl("S52\\.[5-6]", hes$diag_icd10_full) |
    grepl("S62\\.[0-1]", hes$diag_icd10_full),
  1, 0
)

# Hip

hes$Hip_HES <- ifelse(
  grepl("S72\\.[0-2]", hes$diag_icd10_full),
  1, 0
)

# Pull into a new dataset for analysis
analysis_dat <- dat 

# Save the clean dataset
saveRDS(analysis_dat, "data_derived/analysis_dat.rds")

########################### Measures for time-to-event analysis #############################
# Time to event measure
# Time to fracture
# Person time at risk - Will need to work out the censoring dates to do this?





#### Outcome data derivation for time to event analysis

## Add incident disease

# We now add the outcome: time to incident fracture event. Participants can either:

#   - be observed to have a fracture event during their time in the study.
#   - be censored without having had a recorded fracture event.
#[Censoring](https://www.publichealth.columbia.edu/research/population-health-methods/time-event-data-analysis#:~:text=This%20phenomenon%20is%20called%20censoring,participant%20experiences%20a%20different%20event) 
# may occur at death, at the end of records, or at the date at which a particular participant was recorded to be lost-to-follow-up.

# Loss-to-follow-up for particular participants is recorded in [field 191](https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=191). This currently hasn't been updated for several years.

# [The censoring dates (the end of records) can be obtained from UK Biobank](https://biobank.ndph.ox.ac.uk/ukb/exinfo.cgi?src=Data_providers_and_dates). 

# Note that procedures for how censoring dates are provided when data gets updated on the RAP are currently being established. 
# In particular, there is currently a time gap between update of the Showcase data and update of the data provided on RAP. 
# This mean that when the data is newly updated the censoring dates on the UK Biobank webpage may not apply to the RAP. 
# Pending a solution to this (see [this question](https://community.dnanexus.com/s/question/0D5t000003rG3dgCAC/is-there-a-way-to-find-the-record-censoring-dates-for-a-particular-data-release-version) on the community forums), 
# you may wish to manually check the censoring dates. For example, you can implement the rule described on the UK Biobank webpage:

#  *The censoring date is the last day of the month for which the number of records is greater than 90% of the mean of the number of records for the previous three months, 
# except where the data for that month is known to be incomplete in which case the censoring date is the last day of the previous month.*

# Records in different countries of the United Kingdom may have different censoring dates. 
# We match participants to the appropriate censoring date based on the country in which they attended the baseline assessment centre.

# We record the relevant record censoring dates when we wrote this script (i.e. the end of records):


ind_wales <-
  dat$ukb_assess_cent %in% c("Cardiff", "Wrexham", "Swansea")
ind_scotland <- 
  dat$ukb_assess_cent %in% c("Edinburgh", "Glasgow")

# Note that if hospital and death records have different censoring dates, we use the earlier one
dat$date_cens [England]<- "2021-09-30"
dat$date_cens[Wales] <- "2018-02-28"
dat$date_cens[Scotland] <- "2021-07-31"
dat$date_cens <- as.Date(dat$date_cens)


# Participants with a recorded loss-to-follow-up date should be censored at loss-to-follow-up:

# People who were lost to follow-up are censored at earliest of loss-to-follow-up and overall censoring

dat$date_cens <- pmin(dat$date_cens, dat$date_lost_followup, na.rm = TRUE)

# Specific to accelerometer data
# A few people are apparently lost to follow up in linked health records before they wore the accelerometer
# We exclude these people

nb <- nrow(dat)
dat <- dat[!(dat$date_cens < dat$date_end_accel), ]
tab_exc <- rbind(tab_exc, 
                 data.frame("Exclusion" = "Lost to linked health record follow-up before accelerometer study entry", "Number_excluded" = nb - nrow(dat), "Number_remaining" = nrow(dat)))
tab_exc


# Participants who died should be censored at death, provided this occurred before the end of records: 
# People who died are censored at earliest of date of death and overall censoring 

dat$date_cens[dat$ind_died] <-
  pmin(dat$date_cens[dat$ind_died], dat$date_death[dat$ind_died])


# We now add a date for end of follow up, which is either the date at which the participant was censored 
# or the date at which they experienced a fracture event in hospital records as long as this occurred before censoring 
# (occasionally, participants can have records occurring after the record censoring date):


# Add follow up variable
# i.e. same as censor date for participants without a hospital-recorded fracture diagnosis,
# event date for participants with hospital-recorded fracture diagnosis that falls within the study period
dat$date_fu <- dat$date_cens
dat$date_fu[dat$ind_inc_hes_cvd] <-
  pmin(dat$date_hes_first_cvd[dat$ind_inc_hes_cvd], dat$date_fu[dat$ind_inc_hes_cvd])


# We now record the event status at exit. We don't use 'ind_inc_hes_cvd' directly as:

# -   as noted above, there may be instances of people with an event in the data after censoring
# -   more importantly, we will want to add people who have a record for CVD at death without a prior occurrence in hospital data (e.g. someone who died suddenly of a stroke without being admitted to hospital)


dat$ind_inc_cvd <- FALSE

# Mark ind_inc_cvd for people with a hospital record of CVD during the study period
dat$ind_inc_cvd[dat$ind_inc_hes_cvd & (dat$date_hes_first_cvd == dat$date_fu)] <- TRUE

# Mark ind_inc_cvd for participants with a first record of CVD at death
ids_death_cvd <-
  dat_death_cause$eid[grepl("I2[0-5]|I6", dat_death_cause$cause_icd10)]
ind_death_cvd <-  dat$eid %in% ids_death_cvd
dat$ind_inc_cvd[ind_death_cvd &


# We calculate follow up time (i.e. total time on study):

dat$fu_time <-  as.double(difftime(dat$date_fu, dat$date_end_accel, units = "days"))

# Alternatively, we might want to analyse the data using age as the timescale, so we add a variable for age at exit in days:

dat$age_exit_days <- as.double(dat$age_entry_days + dat$fu_time)
dat$age_exit_days2 <-  as.double(difftime(dat$date_fu, dat$approx_dob, units = "days")) # calculation in an alternative way just so we can implement a logic


# Logic check, if you get error message, check code above. If no error message, continue on.

# Logic check
if (!isTRUE(all.equal(dat$age_exit_days, dat$age_exit_days2))){
    stop("Different methods of calculating age at exit give different answers")
}


# We noted that it is well worth inspecting your data to check the code is behaving as expected, especially for some of the logically complex processes in this notebook. 
# This isn't shown here, to minimise the risk of accidentally printing data on the internet, but here are just a few checks we can do to make sure things look sensible:

dat$fu_years <- dat$fu_time/365.25
# Follow up time distribution
hist(dat$fu_years, xlim = c(0,10))

# Follow up time in different groups
aggregate(dat$fu_years, list(dat$ukb_assess_cent), FUN = function(x) {round(median(x), digits = 1)}) 
aggregate(dat$fu_years, list(dat$ind_died), FUN = function(x) {round(median(x), digits = 1)}) 
aggregate(dat$fu_years, list(dat$ind_inc_cvd), FUN = function(x) {round(median(x), digits = 1)}) 

# Max follow up date by assessment centre
aggregate(dat$date_fu, list(dat$ukb_assess_cent), FUN = max)

