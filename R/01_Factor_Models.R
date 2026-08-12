# ============================================================
# EXAMPLE 1A: FACTOR-BASED LMM AND rANOVA
# ============================================================

library(lme4)
library(lmerTest)
library(emmeans)
library(ez)
library(performance)
library(ggplot2)

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

# Confirm the complete and balanced design.
with(ergo_simple, table(Participant, Reach_bin))

# Factor-based LMM.
model_bins <- lmer(
  Shoulder_Moment_Nm ~ Reach_bin * Sex +
    (1 | Participant),
  data = ergo_simple,
  REML = TRUE
)

anova_bins_lmm <- anova(
  model_bins,
  type = 3,
  ddf = "Satterthwaite"
)
print(anova_bins_lmm)
print(summary(model_bins))

# Equivalent rANOVA.
anova_bins <- ezANOVA(
  data = ergo_simple,
  dv = Shoulder_Moment_Nm,
  wid = Participant,
  within = Reach_bin,
  between = Sex,
  detailed = TRUE,
  type = 3
)
print(anova_bins)

# Model-based means and pairwise comparisons.
emm_bins <- emmeans(
  model_bins,
  ~ Reach_bin | Sex,
  lmer.df = "satterthwaite"
)
print(emm_bins)

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
  scale_x_continuous(
    breaks = seq_along(reach_order),
    labels = reach_order
  ) +
  scale_colour_viridis_d(option = "D", end = 0.85) +
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

# Examine model diagnostics before interpreting or reporting results.
print(check_model(model_bins))
