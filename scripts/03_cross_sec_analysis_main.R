# This script details the cross-sectional analysis looking at self-reported physical activity and self-reported fracture risk



# The following processing steps will create automated table outputs

# Create folder path for Table and Figure results
Table_folder_path <- "../results/tables"
Figure_folder_path <- "../results/figures"


# Function to make table from summary data
make_table <- function(summary_df, table_number, folder_path, title = NULL) {
  
  ft <- summary_df |>
    flextable() |>
    autofit() |>
    theme_vanilla()
  
  save_flextable(ft, table_number, folder_path, title)
  
  ft  # return the table for printing in markdown
}

# Function to save flextable outputs to word

save_flextable <- function(ft, table_number, folder_path, title = NULL) {
  # ensure folder exists
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  # construct path
  file_name <- paste0("Table_", table_number, ".docx")
  file_path <- file.path(folder_path, file_name)
  
  # start doc
  doc <- read_docx()
  
  # add title (only if provided)
  if (!is.null(title)) {
    full_title <- paste0("Table ", table_number, ": ", title)
    doc <- doc |>
      body_add_par(full_title, style = "Normal") |>
      body_add_par("", style = "Normal")  # blank line
  }
  
  # add the table and save
  doc <- doc |> body_add_flextable(ft)
  print(doc, target = file_path)
  
  message("Saved: ", file_path)
  invisible(file_path)
}


# install rds from 02_data_derivation
dat <- readRDS("data_derived/analysis_dat.Rds")
View(dat)

# Select variables for cross sectional analysis
dat <- dat %>%
  select (
    eid,
    age_education = age_education_clean,
    ethnicity = ethnicity_derived,
    SRF = self_reported_fracture_A0_clean,
    Wrist,
    Hip,
    duration_mod_PA = duration_mod_clean,
    num_days_mod_PA = num_days_mod_clean,
    duration_vig_PA = duration_vig_clean,
    num_days_vig_PA = num_days_vig_clean,
    duration_walk = duration_walk_clean,
    num_day_walk = num_day_walk_clean,
    IPAQ = IPAQ_clean,
    cc_MET_mod, 
    cc_mins_wk_mod,
    cc_MET_vig,
    cc_mins_wk_vig,
    cc_MET_MVPA,
    cc_mins_wk_MVPA,
    cc_MET_walk,
    cc_mins_wk_walk,
    cc_MET_summed,
    cc_mins_wk_summed,
    MET_mod = MET_mod_clean_num,
    MET_vig = MET_vig_clean_num,
    MET_walk = MET_walk_clean,
    MVPA = MVPA_derived,
    summed_MET_all = summed_MET_all_raw,
    date_assess_A0 = date_assess_A0_raw,
    assessment_country,
    age_A0 = age_A0_raw,
    agegp_A0,
    sex = sex_raw,
    qualification = education_level,
    tdi = tdi_raw,
    WC = waist_circ_clean,
    weight = weight_clean,
    height = height_clean,
    BMI = BMI_clean,
    BMI_category = BMI_category_derived,
    lost_to_fu = lost_to_fu_raw,
    approx_dob_derived,
  )
    
    # Create middle aged cohort A0 for all subsequent analysis

middle_aged_dat <- dat %>%
  filter(between(age_A0, 40, 65))

# # create 10 year age groups
# dat_age_45_55 <- dat %>%
#   filter(between(age_A0_raw, 45, 55))
# 
# dat_age_56_65 <-dat %>%
#   filter(between (age_A0_raw, 56, 65))

# Create Demographics Table 1 split by sex

# Ensure sex is a factor
middle_aged_dat$sex <- factor(middle_aged_dat$sex)

middle_aged_dat <- middle_aged_dat %>%
  mutate(
    Wrist = factor(Wrist, levels = c(0, 1), labels = c("No", "Yes")),
    Hip   = factor(Hip,   levels = c(0, 1), labels = c("No", "Yes"))
  )


# Stratified by sex
table1::table1(
  ~ age_A0 + ethnicity + tdi + qualification + weight + height + WC + BMI + SRF + Wrist + Hip + IPAQ |sex, 
  data = middle_aged_dat,
  overall = "Total",
)
    
# For simplicity in following analysis we will rename the middle aged cohort back to dat but can use middle_aged_data in later 
# complete case vs full cohort comparison
dat <- middle_aged_dat

# Prior to exclusions look at degree of missingness in each variable
# duration of PA variables not included - this was a follow on questions for only those who answered num days>0

vars <-c("ethnicity", "SRF", "num_days_mod_PA", "num_days_vig_PA", "num_day_walk",
         "IPAQ", "qualification", "tdi", "WC", "weight", "height", "BMI")

response_rate <- function(df, var) {
  df %>%
    summarise(
      Answered = sum(!is.na(.data[[var]])),
      Missing = sum(is.na(.data[[var]])),
      Total = nrow(df),
      Answered_pct = round(Answered / Total * 100, 2),
      Missing_pct = round(Missing / Total * 100, 2)
    ) %>%
    mutate(Variable = var) %>%
    select(Variable, Answered, Answered_pct, Missing, Missing_pct)
}

# Apply to all variables in dat
Response_rates <- lapply(vars, function(v) response_rate(dat, v)) %>%
  bind_rows()

# Create and save table using function above

Table_Response <- make_table(
  Response_rates, 
  table_number = "Response Rate", 
  folder_path = Table_folder_path,
  title = "Response rate and degree of missing data on activity type and days per week walking"
)

Table_Response


## Create a complete case analysis data set
## Exclusions 

# Before working with the data, we usually exclude some participants.
# Before working with the data we want to exclude those with missing data in covariates and pre-existing fracture
# We will record how many participants are excluded at each of the steps (e.g. for a flow diagram):

tab_exc <- data.frame("Exclusion" = "Starting cohort (age 40-65)",
                      "Number_excluded" = NA, 
                      "Number_remaining" = nrow(dat)
                      )


# First, we exclude participants with missing fracture data

# Store number before exclusion
nb <- nrow(dat)
# Exclude rows with NA in SRF
dat <- dat[!is.na(dat$SRF), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing fracture data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)

#  Exclude participants whose did not answer the self-reported activity question based on missing answer in IPAQ group

# Store number before exclusion
nb <- nrow(dat)

# Store number before exclusion
nb <- nrow(dat)

# Exclude rows with NA in PA data
dat <- dat[!is.na(dat$cc_MET_mod)
           & !is.na(dat$cc_MET_vig)
           &!is.na(dat$cc_MET_walk), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing MET data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)



# Exclude those with missing covariate data
nb <- nrow(dat)

# Exclude rows with NA in Ethnicity
dat <- dat[!is.na(dat$ethnicity), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing Ethnicity data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)

# Exclude those with no deprivation data

nb <- nrow(dat)

# Exclude rows with NA in tdi
dat <- dat[!is.na(dat$tdi), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing deprivation data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)

# Exclude those with missing education data
nb <- nrow(dat)

# Exclude rows with NA in qualification
dat <- dat[!is.na(dat$qualification), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing qualification data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)

# Exclude those with missing anthropometric data
nb <- nrow(dat)

# Exclude rows with NA in weight
dat <- dat[!is.na(dat$weight)
           & !is.na(dat$height)
           & !is.na(dat$WC)
           & !is.na(dat$BMI), ]


# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing anthropometric data",
    "Number_excluded" = nb - nrow(dat),
    "Number_remaining" = nrow(dat)
  )
)

View (tab_exc)



png(
  filename = file.path(Figure_folder_path, "Exclusion_table.png"),
  width = 1200,
  height = 800,
  res = 150
)




complete_case_dat <- dat

# Save the final dataset
saveRDS(complete_case_dat, "data_derived/complete_case_dat.Rds")

# Compare complete case set (complete_case_dat) to full set (middle_aged_data)

middle_aged_dat <- middle_aged_dat %>%
  mutate(
    included_final = if_else(eid %in% complete_case_dat$eid, 1, 0)
  )

table(middle_aged_dat$included_final)


middle_aged_dat <- middle_aged_dat %>%
  mutate(
    included_final = if_else(eid %in% complete_case_dat$eid, 1, 0),
    included_final = factor(
      included_final,
      levels = c(0, 1),
      labels = c("Full cohort", "Complete cases")
    )
  )

# Step 2: Remove any rows where included_final is NA (just in case)
middle_aged_dat <- middle_aged_dat %>%
  filter(!is.na(included_final))

complete_full_case_comparison <- middle_aged_dat %>%
  tbl_summary(
    by = included_final,  # stratify by full vs complete cases
    include = c(ethnicity, SRF, num_days_mod_PA, num_days_vig_PA, num_day_walk, IPAQ, qualification, tdi, WC, weight, height, BMI),
    missing = "no"
  ) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Cohort type**") 

complete_full_case_comparison_gt <- complete_full_case_comparison %>%
  as_gt() %>%   
  tab_header(
    title = md("**Complete vs Full Cohort Comparison**"),
    subtitle = md("Response rate and missing data on activity type and walking days")
  )


gtsave(complete_full_case_comparison_gt, 
       filename = file.path(Table_folder_path, "complete_full_case_comparison.html"))




## Add 'final dataset' variables

# Some variables can only be generated in the final analytic dataset (e.g. those based on quarters of the data).

# We make a function to cut by quantile:
  

qtile_cut <-  function(x, probs = seq(0, 1, 0.25), na.rm = TRUE, labels = NULL) {
  breaks <- quantile(x = x, probs = probs, na.rm = na.rm)
  out <- cut(x = x, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)
  return(out)
}


# We cut overall activity and Townsend Deprivation Index into quarters:
  
dat$overall_activity_quarters <- qtile_cut(dat$overall_activity, labels = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))
# Note - the TDI classification here was quarters of the study population, which was used in the example papers. However, our group now typically uses TDI scaled to quarters of the UK population, 
# as listed [here](https://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/Townsend_Deprivation_Scores/UK%20Townsend%20Deprivation%20Scores%20from%202011%20census%20data.pdf, page 15)

dat$tdi_quarters <- qtile_cut(dat$tdi, labels = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))

## Descriptive statistics for variables of interest to see if we can model them in the way we want.
