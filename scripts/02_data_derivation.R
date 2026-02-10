# This code describes the derivation of variables for use in the final analysis - it uses variables created in 01_data_cleaning.R
# Variables that do not need derivation are renamed for ease in analysis ie BMI_raw to BMI

source(here::here("scripts/00_setup.R"))

# load the files for Assessment centre 0, HES and death, 

dat_clean   <- readRDS(file.path(DATA_DERIVED, "baseline_clean.Rds"))

#### Derivations for cross-sectional analysis ##################


# Print column names
names(dat_clean)

# If no derivation needed then drop the word _raw for the analysis to keep coding tight

dat_clean$ethnicity_derived <- case_when(
  dat_clean$ethnicity_clean %in% c(
    "White", "British", "Irish", "Any other white background"
  ) ~ "White",
  
  dat_clean$ethnicity_clean %in% c(
    "Mixed",
    "White and Black Caribbean",
    "White and Black African",
    "White and Asian",
    "Any other mixed background"
  ) ~ "Mixed",
  
  dat_clean$ethnicity_clean %in% c(
    "Asian or Asian British",
    "Indian",
    "Pakistani",
    "Bangladeshi",
    "Any other Asian background"
  ) ~ "Asian",
  
  dat_clean$ethnicity_clean %in% c(
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
table(dat_clean$ethnicity_derived, useNA = "ifany")


# UKB assessment centre
# Field 54	# Assessment Centre location (A0)
# To allow me to look at geographic variation I re-coded Assessment centre by Country it was located in.
# Codes were as follows

dat_clean$assessment_country <- case_when(
  dat_clean$ukb_assess_center_raw %in% c(
    "Barts", "Birmingham", "Bristol", "Bury", "Cheadle",
    "Croydon", "Hounslow", "Leeds", "Liverpool", "Middlesbrough",
    "Newcastle", "Nottingham", "Oxford", "Reading", "Sheffield",
    "Stockport", "Stoke", "Sunderland", "Warrington",
    "Wilmslow", "Wolverhampton",
    "Cheadle (Greater Manchester)",
    "Stockport (Greater Manchester)"
  ) ~ "England",
  
  dat_clean$ukb_assess_center_raw %in% c(
    "Edinburgh", "Glasgow"
  ) ~ "Scotland",
  
  dat_clean$ukb_assess_center_raw %in% c(
    "Cardiff", "Swansea"
  ) ~ "Wales",
  
  TRUE ~ NA_character_
)


# Create middle aged cohort by activity measure

# create agegroup_A0 based on age_A0_raw

middle_aged_A0_dat <- dat_clean %>%
  filter(between(age_A0_raw, 40, 65))

# Create age at A0 groups
dat_clean$agegp_A0 <-
  cut(
    dat_clean$age_A0_raw,
    breaks = c(40, 50, 60, 70, 80),
    right = FALSE,
    labels = c("40-49", "50-59", "60-69", "70-79")
  )

# create age-at-accelerometer-wear variable:
  
# Add date of birth
dat_clean$approx_dob_derived <-
  as.Date(paste(dat_clean$yob, dat_clean$mob, "15", sep = "-"),
          "%Y-%B-%d") # UK Biobank doesn't contain day of birth as it would be unnecessary identifying information, so we roughly impute it as the 15th of the birth month.



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
dat_clean$education_level <- derive_education_level(dat_clean$qualification_raw)

# check
table(dat_clean$education_level, useNA = "ifany")

# convert to ordered factor
dat_clean$education_level <- factor(
  dat_clean$education_level,
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


dat_clean <- dat_clean %>%
  mutate(
    central_obesity_derived = case_when(
      sex_raw == "Male" & waist_circ_clean >= 102 ~ "Substantially increased risk",
      sex_raw == "Male" & waist_circ_clean >= 94  ~ "Increased risk",
      sex_raw == "Male" & waist_circ_clean < 94   ~ "Normal",
      sex_raw == "Female" & waist_circ_clean >= 88 ~ "Substantially increased risk",
      sex_raw == "Female" & waist_circ_clean >= 80 ~ "Increased risk",
      sex_raw == "Female" & waist_circ_clean < 80  ~ "Normal",
      TRUE ~ NA_character_
    ),
    BMI_category_derived = case_when(
      BMI_clean < 25                     ~ "Normal",
      BMI_clean >= 25 & BMI_clean < 30  ~ "Overweight",
      BMI_clean >= 30                    ~ "Obese",
      TRUE ~ NA_character_
    )
  )


#### Physical activity measures ####

# In UKB MET mins is derived from num days of activity and an imputed value when duration is missing.
# For the complete case analysis we will create our own variables of mins/wk and METmins/wk with just complete data
# When someone responded as 0 to number of days of activity they wont have been prompted on duration - we will impute 0 not NA for these

# Participants reporting zero days of moderate activity were assigned zero minutes per week, recognising that duration was not collected in these cases. 
# Complete case variables were defined accordingly.

# Complete case mins/wk mod activity (cc_mins_wk_mod)

#### Helper functions ####

# Convert to numeric safely (suppress warnings)
as_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Calculate weekly minutes (0 if days==0, NA if missing)
calc_mins_wk <- function(days, duration) {
  case_when(
    days == 0 ~ 0,
    is.na(days) | is.na(duration) ~ NA_real_,
    TRUE ~ duration * days
  )
}

# Calculate MET from minutes with a factor
calc_MET <- function(mins, factor) {
  if_else(!is.na(mins), mins * factor, NA_real_)
}

# Sum multiple columns safely: NA if all NA, otherwise sum treating NA as 0
sum_na0 <- function(...) {
  vals <- cbind(...)
  rowSums(vals, na.rm = TRUE) %>%
    replace(rowSums(!is.na(vals)) == 0, NA_real_)
}

########


dat_clean <- dat_clean %>%
  mutate(
    ## Complete-case weekly minutes
    cc_mins_wk_mod  = calc_mins_wk(num_days_mod_clean, duration_mod_clean),
    cc_mins_wk_vig  = calc_mins_wk(num_days_vig_clean, duration_vig_clean),
    cc_mins_wk_walk = calc_mins_wk(num_day_walk_clean, duration_walk_clean),
    
    ## Complete-case MET calculations
    cc_MET_mod  = calc_MET(cc_mins_wk_mod, 4),
    cc_MET_vig  = calc_MET(cc_mins_wk_vig, 8),
    cc_MET_walk = calc_MET(cc_mins_wk_walk, 3.3),
    
    ## Combined MVPA (minutes & METs)
    cc_mins_wk_MVPA = sum_na0(cc_mins_wk_mod, cc_mins_wk_vig),
    cc_MET_MVPA     = sum_na0(cc_MET_mod, cc_MET_vig),
    
    ## Total summed activity (minutes & METs)
    cc_mins_wk_summed = sum_na0(cc_mins_wk_walk, cc_mins_wk_mod, cc_mins_wk_vig),
    cc_MET_summed     = sum_na0(cc_MET_walk, cc_MET_MVPA),
 
    # Variables below are those presented in UKB but have some imputed values so not same as cc
       
    ## Self-reported MET variables (numeric)
    MET_mod_clean_num  = as_numeric_safe(MET_mod_clean),
    MET_vig_clean_num  = as_numeric_safe(MET_vig_clean),
    MET_walk_clean_num = as_numeric_safe(MET_walk_clean),
    
    ## Self-reported MVPA derived
    MVPA_derived      = sum_na0(MET_mod_clean_num, MET_vig_clean_num),
    MMVPA_derived_num = as_numeric_safe(MVPA_derived)
  )


# Create binary indicator for each of mod, vig and MVPA i.e 0 vs and other value

# Helper function to make binary variable

to_binary <- function(x) as.integer(x > 0)


# apply to variables
vars <- c("cc_MET_mod", "cc_MET_vig", "cc_MET_MVPA")

dat_clean <- dat_clean %>%
  mutate(across(all_of(vars),
                to_binary,
                .names = "{.col}_bin"))




###### Outcome data variables

# Create binary fracture indicator for a given bone

# Define fracture sites
fracture_sites <- c("Ankle", "Leg", "Hip", "Spine", "Wrist", "Arm", "Other bone")

# Helper function
fracture_indicator <- function(fracture_list, bone_name, self_reported) {
  case_when(
    map_lgl(fracture_list, ~ bone_name %in% .x) ~ 1L,
    self_reported %in% c("Yes", "No") ~ 0L,
    TRUE ~ NA_integer_
  )
}

dat_clean <- dat_clean %>%
  mutate(fracture_list = str_split(fracture_location_A0_clean, "\\|")) %>%
  # Dynamically create columns for all bones
  { 
    tmp <- .
    for(bone in fracture_sites) {
      tmp[[bone]] <- fracture_indicator(tmp$fracture_list, bone, tmp$self_reported_fracture_A0_clean)
    }
    tmp
  } %>%
  select(-fracture_list) 



# Save dataset with derivations for analysis
analysis_dat <- dat_clean

saveRDS(
  analysis_dat,
  file.path(DATA_DERIVED, "analysis_dat.rds")
)


########################### Measures for time-to-event analysis #############################

death_clean <- readRDS(file.path(DATA_DERIVED, "death_clean.Rds"))
hes_clean   <- readRDS(file.path(DATA_DERIVED, "hes_clean.Rds"))



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


############# Additional values for TTE analysis  ##########################


# Add age at accel entry in days
dat_clean$age_accel_entry_days_derived <-
  difftime(dat_clean$date_end_accel_raw,
           dat_clean$approx_dob,
           units = "days")

# Convert to age at entry in years
dat_clean$age_accel_entry_years <- as.double(dat_clean$age_accel_entry_days_derived)/365.25

# Create mid-life accel group

middle_aged_accel_dat <- dat_clean %>%
  filter(between(age_accel_entry_years, 40, 65))

# Create age at accel groups
dat_clean$age_gp_accel <-
  cut(
    dat_clean$age_accel_entry_years,
    breaks = c(40, 50, 60, 70, 80),
    right = FALSE,
    labels = c("40-49", "50-59", "60-69", "70-79")
  )

### HES fracture measures ####

# Based on previous work we will use 2 fracture catagories

#  Wrist
#  Hip

# with sensitivity analysis on any

any_fracture <- function(codes, diag) {
  ifelse(grepl(paste(codes, collapse="|"), diag), 1, 0)
}

# Example:
fracture_codes <- c(
  "S02\\.[0-9]", "S12\\.[0-2]|S12\\.[7-9]", "S22\\.[0-5]|S22\\.[8-9]",
  "S32\\.[0-5]|S32\\.[7-8]", "S42\\.[0-4]|S42\\.[7-9]", "S52\\.[0-9]",
  "S62\\.[0-8]", "S72\\.[0-4]|S72\\.[7-9]", "S82\\.[0-9]", "S92\\.[0-5]|S92\\.[7-9]"
)

hes_clean$all_fracture_hes <- any_fracture(fracture_codes, hes_clean$diag_icd10_full)


# Wrist

hes_clean$wrist_hes <- ifelse(
  grepl("S52\\.[5-6]", hes_clean$diag_icd10_full) |
    grepl("S62\\.[0-1]", hes_clean$diag_icd10_full),
  1, 0
)

# Hip

hes_clean$hip_hes <- ifelse(
  grepl("S72\\.[0-2]", hes_clean$diag_icd10_full),
  1, 0
)

# pull into new dataset for analysis

analysis_dat <- dat_clean %>%
  left_join(
    hes_clean %>% select(eid, all_fracture_hes, wrist_hes, hip_hes),
    by = "eid"
  )

