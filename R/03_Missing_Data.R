# ============================================================
# EXAMPLE 2: INCOMPLETE REPEATED MEASURES
# ============================================================

library(lme4)
library(lmerTest)
library(emmeans)
library(ez)
library(performance)

ergo_simple <- read.csv(
  "data/ergo_simple_shoulder_moment_balanced.csv"
)

ergo_simple$Participant <- factor(ergo_simple$Participant)
ergo_simple$Sex <- factor(
  ergo_simple$Sex,
  levels = c("Male", "Female")
)
ergo_simple$Reach_bin <- factor(
  ergo_simple$Reach_bin,
  levels = c("Near", "Mid", "Far")
)

# Copy the data and remove two outcomes.
ergo_missing <- ergo_simple

ergo_missing$Shoulder_Moment_Nm[
  ergo_missing$Participant == "P03" &
    ergo_missing$Reach_bin == "Far"
] <- NA

ergo_missing$Shoulder_Moment_Nm[
  ergo_missing$Participant == "P04" &
    ergo_missing$Reach_bin == "Near"
] <- NA

print(ergo_missing[is.na(ergo_missing$Shoulder_Moment_Nm), ])

# Attempt rANOVA before explicitly removing incomplete participants.
# try() prevents an error from stopping the rest of the script.
anova_bins_missing_attempt <- try(
  ezANOVA(
    data = ergo_missing,
    dv = Shoulder_Moment_Nm,
    wid = Participant,
    within = Reach_bin,
    between = Sex,
    detailed = TRUE,
    type = 3
  ),
  silent = TRUE
)
print(anova_bins_missing_attempt)

# Identify and remove incomplete participants for rANOVA.
participants_with_missing <- unique(
  ergo_missing$Participant[
    is.na(ergo_missing$Shoulder_Moment_Nm)
  ]
)

ergo_anova_missing <- ergo_missing[
  !(ergo_missing$Participant %in% participants_with_missing),
]
ergo_anova_missing <- droplevels(ergo_anova_missing)

# Complete-case rANOVA.
anova_bins_missing <- ezANOVA(
  data = ergo_anova_missing,
  dv = Shoulder_Moment_Nm,
  wid = Participant,
  within = Reach_bin,
  between = Sex,
  detailed = TRUE,
  type = 3
)
print(anova_bins_missing)

# Count the complete-case participants retained within each sex.
participants_retained <- unique(
  ergo_anova_missing[c("Participant", "Sex")]
)
print(table(participants_retained$Sex))

# Factor-based LMM using all available outcomes.
model_bins_missing <- lmer(
  Shoulder_Moment_Nm ~ Reach_bin * Sex +
    (1 | Participant),
  data = ergo_missing,
  REML = TRUE,
  na.action = na.exclude
)

anova_bins_lmm_missing <- anova(
  model_bins_missing,
  type = 3,
  ddf = "Satterthwaite"
)
print(anova_bins_lmm_missing)
print(summary(model_bins_missing))

emm_bins_missing <- emmeans(
  model_bins_missing,
  ~ Reach_bin | Sex,
  lmer.df = "satterthwaite"
)
print(emm_bins_missing)

pairs_bins_missing <- pairs(
  emm_bins_missing,
  adjust = "tukey"
)
print(pairs_bins_missing)

# Compare the data retained by each analysis.
data_retained <- data.frame(
  Analysis = c(
    "Complete-case rANOVA",
    "Factor-based LMM"
  ),
  Participants_Retained = c(
    length(unique(ergo_anova_missing$Participant)),
    length(unique(
      ergo_missing$Participant[
        !is.na(ergo_missing$Shoulder_Moment_Nm)
      ]
    ))
  ),
  Observations_Retained = c(
    nrow(ergo_anova_missing),
    nobs(model_bins_missing)
  )
)
print(data_retained)

# Examine model diagnostics before interpreting or reporting results.
print(check_model(model_bins_missing))
