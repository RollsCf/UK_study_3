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


#### Removed from 03_cross_sectional_analysus
### Archive analysis with grouping that didnt account for zeros
````{r}

create_quintile_bins <- function(data, vars) {
  binned_data <- data
  
  for (var in vars) {
    bin_var <- paste0(var, "_q")  # _q for quintile
    
    # 1️⃣ Calculate breaks and make unique
    breaks <- unique(quantile(binned_data[[var]], probs = seq(0, 1, 0.2), na.rm = TRUE))
    
    # 2️⃣ Only cut if there is more than one unique break
    if (length(breaks) > 1) {
      binned_data[[bin_var]] <- cut(
        binned_data[[var]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = paste0("Q", seq_len(length(breaks) - 1))
      )
    } else {
      # If all values are the same, put in single bin
      binned_data[[bin_var]] <- factor("Q1", levels = "Q1")
    }
  }
  
  return(binned_data)
}

PA_vars <- c("MET_mod", "MET_vig", "MET_walk", "MET_MVPA", "summed_MET_all")

cc_dat <- create_quintile_bins(cc_dat, PA_vars)

# Check new columns
head(cc_dat[, c(PA_vars, paste0(PA_vars, "_q"))])



PA_quintile_summary <- lapply(PA_vars, function(var) {
  bin_var <- paste0(var, "_q")
  cc_dat %>%
    group_by(.data[[bin_var]]) %>%
    summarise(
      N = n(),
      Fractures = sum(SRF == "Yes", na.rm = TRUE),
      `Fracture %` = round(100 * mean(SRF == "Yes", na.rm = TRUE), 2)
    ) %>%
    mutate(PA_variable = var) %>%
    rename(Bin = .data[[bin_var]])
}) %>%
  bind_rows() %>%
  select(PA_variable, Bin, N, Fractures, `Fracture %`) %>%
  arrange(PA_variable, Bin)


for (var in PA_vars) {
  bin_var <- paste0(var, "_q")
  
  p <- ggplot(cc_dat, aes(x = .data[[bin_var]], y = as.numeric(SRF == "Yes"))) +
    geom_bar(stat = "summary", fun = "mean", fill = "steelblue", alpha = 0.7) +
    ylab("Proportion with fracture") +
    xlab(paste0(var, " (quintiles)")) +
    ggtitle(paste0("Fracture proportion by ", var, " quintiles")) +
    theme_minimal() +
    ylim(0, 1)
  
  print(p)
}


````
