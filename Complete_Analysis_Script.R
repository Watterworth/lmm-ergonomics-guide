# ============================================================
# COMPLETE ANALYSIS
# Moving Beyond Repeated Measures ANOVA
# ============================================================

# Run this script from the project root.
# In RStudio, open lmm-ergonomics-guide.Rproj first.

# ============================================================
# 1. Load packages
# ============================================================

library(lme4)
library(lmerTest)
library(emmeans)
library(ez)
library(performance)
library(ggplot2)
library(patchwork)
library(sjPlot)

# ============================================================
# 2. Import and prepare the illustrative dataset
# ============================================================

ergo_simple <- read.csv(
  "data/ergo_simple_shoulder_moment_balanced.csv"
)

# Tell R which variables are categorical and set their order.
ergo_simple$Participant <- factor(ergo_simple$Participant)
ergo_simple$Sex <- factor(
  ergo_simple$Sex,
  levels = c("Male", "Female")
)
ergo_simple$Reach_bin <- factor(
  ergo_simple$Reach_bin,
  levels = c("Near", "Mid", "Far")
)

# View the first observations.
print(head(ergo_simple))

# ============================================================
# OPENING FIGURE
# Continuous versus factor-based representations
# ============================================================

figure_theme <- theme_classic(base_size = 16) +
  theme(
    strip.background = element_rect(
      fill = "grey92",
      colour = "black",
      linewidth = 0.7
    ),
    strip.text = element_text(
      face = "bold",
      size = 18,
      margin = margin(t = 8, b = 8)
    ),
    axis.title = element_text(
      face = "bold",
      size = 19
    ),
    axis.text = element_text(
      size = 16,
      colour = "black"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold",
      size = 17
    ),
    legend.text = element_text(size = 16),
    plot.margin = margin(t = 8, r = 12, b = 8, l = 12)
  )

# Panel A preserves the continuous reach measurements.
continuous_data <- ergo_simple
continuous_data$Panel <- "A. Continuous Analysis"

continuous_plot <- ggplot(
  continuous_data,
  aes(
    x = Reach_cm,
    y = Shoulder_Moment_Nm,
    colour = Sex
  )
) +
  geom_point(size = 3.4, alpha = 0.75) +
  facet_wrap(~ Panel) +
  scale_colour_viridis_d(option = "D", end = 0.85) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    x = "Reach distance (cm)",
    y = "Shoulder moment (Nm)",
    colour = "Sex"
  ) +
  figure_theme

# Panel B displays the same observations in reach bins.
factor_data <- ergo_simple
factor_data$Panel <- "B. Factor-Based Analysis"

factor_plot <- ggplot(
  factor_data,
  aes(
    x = Reach_bin,
    y = Shoulder_Moment_Nm
  )
) +
  geom_boxplot(
    aes(fill = Sex, colour = Sex),
    width = 0.65,
    alpha = 0.55,
    linewidth = 1,
    outlier.shape = NA,
    position = position_dodge(width = 0.75)
  ) +
  geom_point(
    aes(colour = Sex),
    size = 2.8,
    alpha = 0.75,
    position = position_jitterdodge(
      jitter.width = 0.10,
      jitter.height = 0,
      dodge.width = 0.75
    )
  ) +
  facet_wrap(~ Panel) +
  scale_colour_viridis_d(option = "D", end = 0.85) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  guides(
    fill = "none",
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    x = "Reach condition",
    y = "Shoulder moment (Nm)",
    colour = "Sex"
  ) +
  figure_theme

analysis_comparison_figure <-
  continuous_plot +
  factor_plot +
  plot_layout(
    guides = "collect",
    widths = c(1, 1)
  ) &
  theme(legend.position = "bottom")

print(analysis_comparison_figure)



# ============================================================
# EXAMPLE 1A
# Factor-based LMM and repeated-measures ANOVA
# ============================================================

# Confirm that every participant has one observation in each bin.
with(
  ergo_simple,
  table(Participant, Reach_bin)
)

# Fit the factor-based LMM.
model_bins <- lmer(
  Shoulder_Moment_Nm ~ Reach_bin * Sex +
    (1 | Participant),
  data = ergo_simple,
  REML = TRUE
)

# Global Type III tests.
anova_bins_lmm <- anova(
  model_bins,
  type = 3,
  ddf = "Satterthwaite"
)

print(anova_bins_lmm)

# The model summary is included for reference.
print(summary(model_bins))

# Fit the equivalent repeated-measures ANOVA.
anova_bins <- ezANOVA(
  data = ergo_simple,
  dv = Shoulder_Moment_Nm,
  wid = Participant,
  within = Reach_bin,
  between = Sex,
  detailed = TRUE,
  type = 3
)

print(anova_bins$ANOVA)

# Estimate the model-based means for each reach bin within each sex.
emm_bins <- emmeans(
  model_bins,
  ~ Reach_bin | Sex,
  lmer.df = "satterthwaite"
)

print(emm_bins)

# Compare the three reach bins within each sex.
pairs_bins <- pairs(
  emm_bins,
  adjust = "tukey"
)

print(pairs_bins)

# Quickly plot the EMMs and their comparisons.
plot(emm_bins, comparisons = TRUE)

# Convert the EMMs and comparisons to ordinary data frames.
emm_bins_df <- as.data.frame(emm_bins)
pairs_bins_df <- as.data.frame(pairs_bins)

# Prepare the significant Tukey-adjusted comparisons for plotting.
reach_order <- c("Near", "Mid", "Far")
emm_bins_df$x_position <- match(
  as.character(emm_bins_df$Reach_bin),
  reach_order
)

significant_pairs <- pairs_bins_df[
  pairs_bins_df$p.value < 0.05,
]

contrast_parts <- strsplit(
  as.character(significant_pairs$contrast),
  " - ",
  fixed = TRUE
)

significant_pairs$x_start <- vapply(
  contrast_parts,
  function(x) match(x[1], reach_order),
  integer(1)
)

significant_pairs$x_end <- vapply(
  contrast_parts,
  function(x) match(x[2], reach_order),
  integer(1)
)

# Use one significance indicator for all comparisons with p < .05.
significant_pairs$significance <- "*"

significant_pairs$bracket_y <- NA_real_

for (current_sex in unique(as.character(significant_pairs$Sex))) {
  row_indices <- which(
    as.character(significant_pairs$Sex) == current_sex
  )

  comparison_span <-
    significant_pairs$x_end[row_indices] -
    significant_pairs$x_start[row_indices]

  # Place the widest bracket lowest and shorter brackets above it.
  row_indices <- row_indices[
    order(comparison_span, decreasing = TRUE)
  ]

  panel_maximum <- max(
    emm_bins_df$upper.CL[
      as.character(emm_bins_df$Sex) == current_sex
    ]
  )

  significant_pairs$bracket_y[row_indices] <-
    panel_maximum + seq_along(row_indices) * 0.8
}

# Create a customized EMM figure with 95% confidence intervals.

emm_bins_figure <- ggplot(
  emm_bins_df,
  aes(
    x = x_position,
    y = emmean,
    group = Sex,
    colour = Sex
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.08,
    linewidth = 0.8
  ) +
  geom_segment(
    data = significant_pairs,
    aes(
      x = x_start,
      xend = x_end,
      y = bracket_y,
      yend = bracket_y
    ),
    inherit.aes = FALSE,
    colour = "black"
  ) +
  geom_segment(
    data = significant_pairs,
    aes(
      x = x_start,
      xend = x_start,
      y = bracket_y,
      yend = bracket_y - 0.15
    ),
    inherit.aes = FALSE,
    colour = "black"
  ) +
  geom_segment(
    data = significant_pairs,
    aes(
      x = x_end,
      xend = x_end,
      y = bracket_y,
      yend = bracket_y - 0.15
    ),
    inherit.aes = FALSE,
    colour = "black"
  ) +
  geom_text(
    data = significant_pairs,
    aes(
      x = (x_start + x_end) / 2,
      y = bracket_y + 0.12,
      label = significance
    ),
    inherit.aes = FALSE,
    colour = "black",
    size = 5
  ) +
  facet_wrap(~ Sex) +
  scale_colour_viridis_d(
    option = "D",
    end = 0.85
  ) +
  labs(
    x = "Reach Condition",
    y = "Estimated Shoulder Moment (Nm)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

print(emm_bins_figure)

# ============================================================
# EXAMPLE 1B
# Continuous reach model
# ============================================================

# Calculate the average observed reach.
reach_center_cm <- mean(
  ergo_simple$Reach_cm,
  na.rm = TRUE
)

print(reach_center_cm)

# Center reach at its average and divide by 10.
# Reach_c10 = 0 is the average reach.
# A one-unit increase in Reach_c10 is a 10-cm increase in reach.
ergo_simple$Reach_c10 <- (
  ergo_simple$Reach_cm - reach_center_cm
) / 10

# Fit the continuous LMM.
model_cont <- lmer(
  Shoulder_Moment_Nm ~ Reach_c10 * Sex +
    (1 | Participant),
  data = ergo_simple,
  REML = TRUE
)

anova_centered <- anova(
  model_cont,
  type = 3,
  ddf = "Satterthwaite"
)

print(anova_centered)

print(summary(model_cont))

# Estimate the reach slope separately for males and females.
# The slopes are expressed per 10-cm increase in reach.
reach_trends <- emtrends(
  model_cont,
  ~ Sex,
  var = "Reach_c10",
  lmer.df = "satterthwaite"
)

print(reach_trends)

# Test both slopes against zero using Holm adjustment.
reach_trends_zero <- test(
  reach_trends,
  null = 0,
  adjust = "holm"
)

print(reach_trends_zero)

# Directly compare the male and female slopes.
# There is only one comparison, so no adjustment is required.
reach_trends_pair <- pairs(
  reach_trends,
  adjust = "none"
)

print(reach_trends_pair)

# Plot the fitted population-level relationships over the raw data.

continuous_model_figure <- sjPlot::plot_model(
  model_cont,
  type = "pred",
  terms = c("Reach_c10 [all]", "Sex"),
  pred.type = "fe",
  show.data = TRUE,
  ci.lvl = 0.95,
  colors = c("#440154", "#7AD151"),
  title = NULL,
  legend.title = "Sex"
) +
  labs(
    x = "Reach Distance (cm)",
    y = "Shoulder Moment (Nm)",
    title = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(continuous_model_figure)

# ============================================================
# EXAMPLE 2
# Incomplete repeated measures
# ============================================================

# Copy the complete dataset before introducing missing outcomes.
ergo_missing <- ergo_simple

# Remove the P03-Far and P04-Near outcomes.
ergo_missing$Shoulder_Moment_Nm[
  ergo_missing$Participant == "P03" &
    ergo_missing$Reach_bin == "Far"
] <- NA

ergo_missing$Shoulder_Moment_Nm[
  ergo_missing$Participant == "P04" &
    ergo_missing$Reach_bin == "Near"
] <- NA

# Display the two incomplete observations.
print(ergo_missing[
  is.na(ergo_missing$Shoulder_Moment_Nm),
])

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

# Identify participants with at least one missing outcome.
participants_with_missing <- unique(
  ergo_missing$Participant[
    is.na(ergo_missing$Shoulder_Moment_Nm)
  ]
)

print(participants_with_missing)

# Remove those participants for the complete-case rANOVA.
ergo_anova_missing <- ergo_missing[
  !(ergo_missing$Participant %in% participants_with_missing),
]
ergo_anova_missing <- droplevels(ergo_anova_missing)

# Fit the complete-case rANOVA.
anova_bins_missing <- ezANOVA(
  data = ergo_anova_missing,
  dv = Shoulder_Moment_Nm,
  wid = Participant,
  within = Reach_bin,
  between = Sex,
  detailed = TRUE,
  type = 3
)
print(anova_bins_missing$ANOVA)

# Count the complete-case participants retained within each sex.
participants_retained <- unique(
  ergo_anova_missing[c("Participant", "Sex")]
)

print(table(participants_retained$Sex))

# Fit the factor-based LMM to all available outcomes.
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

# Compare the number of participants and observations retained.
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

# ============================================================
# Model diagnostics
# ============================================================

# Examine these figures before interpreting or reporting results.
print(check_model(model_bins))
print(check_model(model_cont))
print(check_model(model_bins_missing))
