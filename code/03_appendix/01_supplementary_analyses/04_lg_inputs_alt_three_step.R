# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, haven)

# LOADING THE DATA --------------------------------------------------------

six_posteriors <- read_sav("./data/latent gold data/2_supplementary_data/six_posteriors.sav")

three_re_posteriors <- read_sav("./data/latent gold data/2_supplementary_data/three_re_posteriors.sav")

other_vars <- readRDS("./data/experimental data/non_conjoint_data.rds")

# FOR THREE STEP ANALYSES IN LATENT GOLD ----------------------------------

six_posteriors |>
  select(1, contains("#"), -`Class#`, adj_weight) |>
  distinct() |>
  left_join(other_vars) |>
  relocate(adj_weight, .after = last_col()) |>
  # Distal Outcome
  relocate(pro_immigration, .before = r_device) |>
  rename_with(~ str_replace(., "r_", ""),
              contains("r_")) |>
  write_sav("./data/latent gold data/2_supplementary_data/six_posteriors_df.sav")


three_re_posteriors |>
  select(1, contains("#"), -`Class#`, adj_weight) |>
  distinct() |>
  left_join(other_vars) |>
  relocate(adj_weight, .after = last_col()) |>
  # Distal Outcome
  relocate(pro_immigration, .before = r_device) |>
  rename_with(~ str_replace(., "r_", ""),
              contains("r_")) |>
  write_sav("./data/latent gold data/2_supplementary_data/three_re_posteriors_df.sav")
