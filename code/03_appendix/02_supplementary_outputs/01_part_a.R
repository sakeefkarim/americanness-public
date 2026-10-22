# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, gt, gtsummary)

# MAIN DATA ---------------------------------------------------------------

quota_vars <- readRDS("./data/experimental data/quota.rds")

# SAMPLE COMPOSITION ------------------------------------------------------

sample_composition <- quota_vars |>
  mutate(age = str_remove_all(age, "Age:") |>
           str_trim(),
         education = case_when(education == "BA+" ~ "College Degree or More",
                               education == "Some College" ~ education,
                               TRUE ~ "High School or Less"),
         
         across(2:6,
                ~fct_relevel(., sort)),
         race = fct_relevel(race, "White", "Black",
                            "Latino"),
         education = fct_relevel(education, "High School or Less",
                                 "Some College"),
         party = fct_relevel(party, "Democrat", "Republican"))


sample_composition_table <- sample_composition |>
  select(-1) |>
  rename_with(str_to_title) |>
  tbl_summary(by = Race,
              type = everything() ~ "categorical",
              statistic = all_categorical() ~ "{p}%",
              digits = all_categorical() ~ 0,
              missing = "no") |>
  modify_header(label ~ "") |>
  modify_footnote(everything() ~ NA)


# Need further adjustments in LaTeX to produce final table—i.e., supplementing these
# numbers with quota targets from Bovitz:

sample_composition_table |>
  as_gt() |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_a1.tex")
