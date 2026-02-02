table(dat$num_days_mod_PA, useNA = "ifany")

# summarises fracture by PA group?
tapply(moderate_pa, fracture, summary)
# 
# Logistic regression does NOT require predictors to be normally distributed
#   Non-normal PA is not a problem
# You only log-transform if it helps:
#   Interpretability
# Linearity with the log-odds (see below)
# Extreme skew/outliers
# Your instinct to explore log(PA) is good—but it’s optional, not required.
# If you log-transform:

data$log_moderate <- log(moderate_pa + 1)

# Check linearity of the log-odds (this does matter)
# Logistic regression assumes a linear relationship between the predictor and the log-odds of the outcome.
# Ways to assess this (pick one):

library(ggplot2)
ggplot(data, aes(moderate_pa, fracture)) +
  geom_smooth(method = "loess")

# If it curves strongly → consider:
#   Log transform
# Categorizing PA (e.g., tertiles)
# Splines (advanced, optional)

# Outliers and implausible values
# Check for:
#   Negative minutes
# Extremely high values (e.g., 5,000 mins/week)

boxplot(moderate_pa)

# Correlation between moderate and vigorous PA
# If you plan to include both in one model:

cor(moderate_pa, vigorous_pa, use = "complete.obs")

# If correlation > ~0.7:
#   They may compete with each other


# Run basic unadjusted model
model1 <- glm(fracture ~ log_moderate,
              family = binomial,
              data = data)
summary(model1)

# convert co-efficients to odds rations
exp(cbind(OR = coef(model1), confint(model1)))

#Model diagnostics - model fit (simple)
AIC(model1)

# Compare moderate vs vigorous activity
# You can:
#   Compare effect sizes (ORs)
# Compare models using AIC
# Put both in one model and see which remains significant
# Consider separate models or careful interpretation

glm(fracture ~ log_moderate + log_vigorous + age + sex,
    family = binomial, data = data)


#######################################################
# Considering a binary model - i.e meeting or not meeting the WHO activity threshold
# Create the variable
who_active <- ifelse(moderate_pa + vigorous_pa >= 150, 1, 0)

# 1 = meet threshold
# 0 = not meeting the threshold

glm(fracture ~ who_active + age + sex, family = binomial, data = data)

# convert to odds ratio
exp(coef(model))

# OR < 1 → meeting WHO guidelines is protective
# OR > 1 → meeting WHO guidelines is associated with higher risk (unlikely biologically!)
# No need to worry about:
#   Linearity with log-odds (already handled — binary predictor always has a linear effect on log-odds)
# Transforming the exposure (0/1 can’t be logged)
# Extreme values (binary variables are clean)
