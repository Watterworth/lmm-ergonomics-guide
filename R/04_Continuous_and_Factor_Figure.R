# ============================================================
# CONTINUOUS VERSUS FACTOR-BASED ANALYSIS FIGURE
# ============================================================

library(ggplot2)
library(patchwork)

ergo_simple <- read.csv(
  "data/ergo_simple_shoulder_moment_balanced.csv"
)

ergo_simple$Sex <- factor(
  ergo_simple$Sex,
  levels = c("Male", "Female")
)

ergo_simple$Reach_bin <- factor(
  ergo_simple$Reach_bin,
  levels = c("Near", "Mid", "Far")
)

# Shared appearance for both panels.
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
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    expand = expansion(mult = c(0, 0.03))
  ) +
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
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    expand = expansion(mult = c(0, 0.03))
  ) +
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

ggsave(
  filename = "continuous_vs_factor_analysis.png",
  plot = analysis_comparison_figure,
  width = 12,
  height = 5.8,
  units = "in",
  dpi = 600,
  bg = "white"
)

