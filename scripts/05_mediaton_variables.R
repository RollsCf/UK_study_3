# Mediation variables
# Grip strength - this will need to be derived in the following script into a single measure of grip strength

# Reaction time "reaction_time_raw",
# This variable has already been through data processing UKB
# Times under 50ms were considered anticipation not reaction and cut
# Times over 2000ms were cut as the cards would have disappeared

# # Falls "self_reported_falls_raw"
# 
# dat$self_reported_falls_clean <- plyr::revalue(dat$self_reported_falls_raw,
#                                      c("Prefer not to answer" = NA))
# 
# dat$fall_fracture_clean <- plyr:: revalue(dat$fall_fracture_raw,
#                                           c("Prefer not to answer" = NA,
#                                             "Do not know" = NA))
# 
# # Usual walking pace "usual_walking_pace_raw"
# 
# 
# dat$usual_walking_pace_clean <- plyr::revalue(dat$usual_walking_pace_raw,
#                                                c("None of the above" = NA,
#                                                  "Prefer not to answer" = NA))
# 
# # V02 max - no cleaning needed "V02_raw", 
# 
# 

##### Mediation variables

# Grip strength - Average the value for L and R into single value
dat$grip_derived <- rowMeans(cbind(dat$grip_strength_L_raw, dat$grip_strength_R_raw), na.rm = TRUE)