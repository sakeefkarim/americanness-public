# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, haven)

# LOADING THE DATA --------------------------------------------------------

main_posteriors <- read_sav("./data/latent gold data/main.sav")

other_vars <- readRDS("./data/experimental data/non_conjoint_data.rds")

# FOR THREE STEP ANALYSIS IN LATENT GOLD ----------------------------------

main_posteriors |>
  select(1, contains("#"), -`Class#`, adj_weight) |>
  distinct() |>
  left_join(other_vars) |>
  relocate(adj_weight, .after = last_col()) |>
  # Distal Outcome
  relocate(pro_immigration, .before = r_device) |>
  rename_with(~ str_replace(., "r_", ""), contains("r_")) |>
  write_sav("./data/latent gold data/main_posteriors.sav")
