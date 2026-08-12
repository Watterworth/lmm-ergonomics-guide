# OPTIONAL: RECREATE THE ILLUSTRATIVE DATASET
# This script documents how the supplied CSV was created.
# It is not required for any analysis in the tutorial.

set.seed(1001)

n_participants <- 14
bins <- c("Near", "Mid", "Far")

sex_by_participant <- rep(
  c("Male", "Female"),
  length.out = n_participants
)

Participant <- factor(
  rep(
    paste0("P", sprintf("%02d", 1:n_participants)),
    each = length(bins)
  )
)

Sex <- factor(
  rep(sex_by_participant, each = length(bins)),
  levels = c("Male", "Female")
)

Reach_bin <- factor(
  rep(bins, times = n_participants),
  levels = bins
)

sample_in_bin <- function(bin) {
  switch(
    bin,
    "Near" = sample(20:35, 1),
    "Mid" = sample(36:55, 1),
    "Far" = sample(56:70, 1)
  )
}

Reach_cm <- vapply(
  as.character(Reach_bin),
  sample_in_bin,
  numeric(1)
)
Reach_cm <- as.integer(Reach_cm)

re_participant <- rnorm(
  n_participants,
  mean = 0,
  sd = 1
)

re_lookup <- setNames(
  re_participant,
  levels(Participant)
)

rand_int <- re_lookup[as.character(Participant)]

beta0 <- 9
b_reach_male <- 0.12
b_reach_female <- b_reach_male - 0.05
b_female_int <- -1.0
sigma_eps <- 1

slope_by_sex <- ifelse(
  Sex == "Male",
  b_reach_male,
  b_reach_female
)

int_by_sex <- ifelse(
  Sex == "Male",
  beta0,
  beta0 + b_female_int
)

Shoulder_Moment_Nm <- int_by_sex +
  slope_by_sex * Reach_cm +
  rand_int +
  rnorm(
    length(Reach_cm),
    mean = 0,
    sd = sigma_eps
  )

Shoulder_Moment_Nm <- round(
  pmax(Shoulder_Moment_Nm, 0),
  2
)

ergo_simple <- data.frame(
  Participant,
  Sex,
  Reach_cm,
  Shoulder_Moment_Nm,
  Reach_bin
)

reach_center_cm <- mean(
  ergo_simple$Reach_cm,
  na.rm = TRUE
)

ergo_simple$Reach_c <-
  ergo_simple$Reach_cm - reach_center_cm
ergo_simple$Reach_c10 <- ergo_simple$Reach_c / 10
ergo_simple$Reach_10 <- ergo_simple$Reach_cm / 10

write.csv(
  ergo_simple,
  "data/ergo_simple_shoulder_moment_balanced.csv",
  row.names = FALSE
)

