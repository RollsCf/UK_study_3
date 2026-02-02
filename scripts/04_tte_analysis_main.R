# This script describes the time-to-event analysis


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

# Exclude rows with NA in PA data
dat <- dat[!is.na(dat$IPAQ) 
           &!is.na(dat$MET_mod) 
           & !is.na(dat$MET_vig) 
           &!is.na(dat$MET_walk) 
           &!is.na(dat$MVPA), ]

# Add to exclusion table
tab_exc <- rbind(
  tab_exc,
  data.frame(
    "Exclusion" = "Missing activity data",
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





nb <- nrow(dat)
dat <- dat[dat$quality_good_calibration == "Yes", ]
tab_exc <- rbind(tab_exc, data.frame("Exclusion" = "Poor calibration", "Number_excluded" = nb - nrow(dat), "Number_remaining" = nrow(dat)))


# Exclude participants for whom \>1% of values were clipped (fell outside the sensor's range) before or after calibration:

nb <- nrow(dat)
dat <- dat[(dat$clips_before_cal < 0.01*dat$total_reads) & (dat$clips_after_cal < 0.01*dat$total_reads) , ]
tab_exc <- rbind(tab_exc, data.frame("Exclusion" = "Too many clips", "Number_excluded" = nb - nrow(dat), "Number_remaining" = nrow(dat)))


# Exclude participants who had \<3 days wear or did not have wear in each hour of the 24 hour day:

nb <- nrow(dat)
dat <- dat[dat$quality_good_wear_time == "Yes", ] # Note that this has already been calculated in UKB, 
# we don't need to manually calculate it: https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=90015
tab_exc <- rbind(tab_exc, data.frame("Exclusion" = "Poor wear time", "Number_excluded" = nb - nrow(dat), "Number_remaining" = nrow(dat)))


# Exclude participants with unrealistically high overall activity values:

nb <- nrow(dat)
dat <- dat[dat$overall_activity < 100, ]
tab_exc <- rbind(tab_exc, data.frame("Exclusion" = "Very high overall activity", "Number_excluded" = nb - nrow(dat), "Number_remaining" = nrow(dat)))

# We visualise exclusions so far:

tab_exc


