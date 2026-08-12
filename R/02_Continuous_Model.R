# ============================================================
# EXAMPLE 1B: CONTINUOUS REACH MODEL
# ============================================================

library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(ggplot2)
library(sjPlot)

ergo_simple <- read.csv(
  "data/ergo_simple_shoulder_moment_balanced.csv"
)

ergo_simple$Participant <- factor(ergo_simple$Participant)
ergo_simple$Sex <- factor(
  ergo_simple$Sex,
  levels = c("Male", "Female")
)

# Center reach at its average and express it in 10-cm units.
reach_center_cm <- mean(ergo_simple$Reach_cm, na.rm = TRUE)
ergo_simple$Reach_c10 <- (
  ergo_simple$Reach_cm - reach_center_cm
) / 10

# Continuous LMM.
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

# Sex-specific slopes per 10-cm increase in reach.
reach_trends <- emtrends(
  model_cont,
  ~ Sex,
  var = "Reach_c10",
  lmer.df = "satterthwaite"
)
print(reach_trends)

reach_trends_zero <- test(
  reach_trends,
  null = 0,
  adjust = "holm"
)
print(reach_trends_zero)

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
  scale_x_continuous(
    labels = function(x) {
      round(x * 10 + reach_center_cm)
    }
  ) +
  labs(
    x = "Reach Distance (cm)",
    y = "Estimated Shoulder Moment (Nm)",
    title = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(continuous_model_figure)

# Examine model diagnostics before interpreting or reporting results.
print(check_model(model_cont))
