# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  tidyverse,
  gt,
  gtsummary,
  readxl,
  survey
)

# LOADING THE DATA --------------------------------------------------------

wt <- readRDS("./data/experimental data/logics.rds") |>
  select(1, adj_weight)

other_vars <- readRDS("./data/experimental data/non_conjoint_data.rds")

# TABLE D1 ----------------------------------------------------------------

descriptives <- other_vars  |>
  left_join(wt) |>
  rename_with(~ str_remove_all(., "^r_")) |>
  select(-contains("lean")) |>
  # Treating ordinal outcome as numeric in line
  # with Latent GOLD output:
  mutate(pro_immigration = pro_immigration |>
           as.numeric())

vars_of_interest <- descriptives |> select(-c(1, "adj_weight")) |> names()

weighted_df <- svydesign(id = ~ responseid,
                         weights = ~ adj_weight,
                         data = descriptives)


descriptive_table <- weighted_df |>
  tbl_svysummary(
    include = vars_of_interest,
    type = list(pro_immigration ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{p}%"),
    digits = everything() ~ 2
  ) |>
  as_tibble() |>
  select(term = 1, value = 2) |>
  filter(!term == "Unknown")  |>
  mutate(variable = ifelse(is.na(value), term, NA_character_)) |>
  fill(variable)


descriptive_table_1 <-  descriptive_table |>
  mutate(
    variable = case_when(
      term == "pro_immigration" ~ "Attitudes Towards Immigration",
      term == "age" ~ "Age in Years",
      TRUE ~ variable
    )
  ) |>
  drop_na() |>
  mutate(
    variable = case_when(
      str_detect(variable, "part") ~ "Partisan Identity",
      variable == "gender" ~ "Sex or Gender",
      str_detect(variable, "relig") ~ "Religious Affiliation",
      str_detect(variable, "gener") ~ "Immigrant Generation",
      str_detect(variable, "univ") ~ "University Status",
      str_detect(variable, "daily_ne") ~ "News Consumption",
      TRUE ~ str_to_title(variable)
    ),
    term = ifelse(
      str_detect(term, "pro_imm"),
      "Pro-Immigrant Sentiment",
      str_to_title(term)
    ),
    value = case_when(
      term == "Pro-Immigrant Sentiment" ~ "3.31 (σ = 1.28)",
      term == "Age" ~ "44.41 (σ = 15.37)",
      TRUE ~ value
    ),
    variable = fct_relevel(
      variable,
      "Attitudes Towards Immigration",
      "Partisan Identity",
      "Race",
      "Sex or Gender",
      "Age In Years",
      "Religious Affiliation",
      "Immigrant Generation",
      "University Status",
      "News Consumption",
      "Device"
    )
  ) |>
  arrange(variable) |>
  group_by(variable) |>
  gt() |>
  cols_label(term = "", value = "Mean or Share") |>
  cols_align(align = "center", columns = everything()) |>
  tab_options(
    row_group.background.color = "#1D677D",
    data_row.padding = px(10),
    table.font.size = "125%",
    column_labels.padding = px(5),
    column_labels.padding.horizontal = px(10)
  ) |>
  opt_table_font(font = list(google_font(name = "Montserrat")))


descriptive_table_1 |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_d1.tex")

# TABLE D2 ----------------------------------------------------------------

ame_distal <- read_excel("./data/latent gold data/1_results_xlsx/02_distal_immigrant.xlsx",
                         skip = 1) |>
  select(term = 3,
         ame = 4,
         z = 5,
         p = 6)

ame_distal_table <- ame_distal |>
  separate(term, into = c("variable", "level"), sep = "\\(") |>
  mutate(
    level = str_remove(level, "\\)$"),
    level = str_to_title(level),
    level = case_when(
      variable == "age" ~ "Age in Years",
      level == "1" ~ "Moderate Hybrid",
      level == "2" ~ "Restrictive Hybrid",
      level == "3" ~ "Inclusive America",
      level == "4" ~ "Exclusive White America",
      level == "5" ~ "Assimilationist",
      TRUE ~ level
    ),
    variable = case_when(
      variable == "Cluster" ~ "Cultural Logic",
      variable == "party" ~ "Partisan Identity",
      str_detect(variable, "generat") ~ "Generation",
      str_detect(variable, "dail") ~ "News Consumption",
      variable == "university" ~ "University Status",
      variable == "gender" ~ "Sex or Gender",
      str_detect(variable, "relig") ~ "Religious Affiliation",
      TRUE ~ str_to_title(variable)
    )
  ) |>
  mutate(
    significant = ifelse(as.numeric(p) <= 0.05, "yes", "no"),
    across(ame:p, as.numeric),
    across(ame:p, ~ ifelse(is.na(.) |
                             . == 0, "—", str_trim(
                               format(round(., 2), nsmall = 2)
                             ))),
    variable = fct_relevel(
      fct_inorder(variable),
      "Cultural Logic",
      "Partisan Identity",
      "Race",
      "Sex or Gender",
      "Age",
      "Religious Affiliation",
      "Generation",
      "University Status",
      "News Consumption"
    ),
    level = fct_relevel(fct_inorder(level), "Other", after = Inf)
  ) |>
  arrange(variable, level) |>
  select(-p) |>
  group_by(variable) |>
  gt() |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = c(ame, z), rows = `significant` == "yes")
  ) |>
  cols_hide(matches("sign")) |>
  cols_label(matches("am") ~ "AME",
             matches("z") ~ md("*z*"),
             matches("level") ~ "") |>
  fmt_markdown() |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  tab_options(
    row_group.background.color = "#1D677D",
    data_row.padding = px(10),
    table.font.size = "125%",
    column_labels.padding = px(5),
    column_labels.padding.horizontal = px(10)
  ) |>
  cols_align(align = "right", columns = everything()[-c(1:2)]) |>
  cols_align(align = "center", columns = c(1:2))


# Further stylized in LaTeX —

ame_distal_table |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_d2.tex")

# TABLE D3 ----------------------------------------------------------------

ame_class <- read_excel(
  "./data/latent gold data/1_results_xlsx/03_membership_main.xlsx",
  sheet = 3,
  skip = 1
) |>
  select(
    logic = 1,
    term = 3,
    ame = 4,
    z = 5,
    p = 6
  )

ame_class_1 <- ame_class |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    )
  ) |>
  separate(term, into = c("variable", "level"), sep = "\\(") |>
  mutate(
    level = str_remove(level, "\\)$"),
    level = str_to_title(level),
    level = ifelse(variable == "age", "Age in Years", level),
    variable = case_when(
      variable == "party" ~ "Partisan Identity",
      str_detect(variable, "generat") ~ "Immigrant Generation",
      str_detect(variable, "dail") ~ "News Consumption",
      variable == "university" ~ "University Status",
      variable == "gender" ~ "Sex or Gender",
      str_detect(variable, "relig") ~ "Religious Affiliation",
      TRUE ~ str_to_title(variable)
    )
  ) |>
  drop_na() |>
  mutate(
    significant = ifelse(as.numeric(p) <= 0.05, "yes", "no"),
    across(ame:p, as.numeric),
    across(ame:p, ~ ifelse(is.na(.) | . == 0, "—", str_trim(
      format(round(., 2), nsmall = 2)
    ))),
    variable = fct_relevel(
      fct_inorder(variable),
      "Partisan Identity",
      "Race",
      "Sex or Gender",
      "Age",
      "Religious Affiliation",
      "Generation",
      "University Status",
      "News Consumption"
    ),
    level = fct_relevel(fct_inorder(level), "Other", after = Inf),
    logic = fct_inorder(logic)
  ) |>
  arrange(variable, level)

# AME

ame_membership <- ame_class_1 |>
  select(1:4) |>
  pivot_wider(
    id_cols = c(variable, level),
    names_from = logic,
    values_from = ame
  )

# Test Statistic

test_membership <- ame_class_1 |>
  select(1:3, z) |>
  pivot_wider(
    id_cols = c(variable, level),
    names_from = logic,
    values_from = z
  ) |>
  rename_with( ~ paste0(., "_z"), c(3:7))

# Significance

sig_membership <- ame_class_1 |>
  select(1:3, significant) |>
  pivot_wider(
    id_cols = c(variable, level),
    names_from = logic,
    values_from = significant
  ) |>
  rename_with( ~ paste0(., "_sig"), c(3:7))

# Putting it all together

ame_membership_table <- ame_membership |>
  left_join(test_membership) |>
  left_join(sig_membership) |>
  select(
    1:2,
    contains("Moder"),
    contains("Rest"),
    contains("Incl"),
    contains("Excl"),
    contains("Assim")
  ) |>
  group_by(variable) |>
  gt() |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = c(`Moderate Hybrid`, `Moderate Hybrid_z`),
      rows = `Moderate Hybrid_sig` == "yes"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = c(`Restrictive Hybrid`, `Restrictive Hybrid_z`),
      rows = `Restrictive Hybrid_sig` == "yes"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = c(`Inclusive America`, `Inclusive America_z`),
      rows = `Inclusive America_sig` == "yes"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = c(`Exclusive White America`, `Exclusive White America_z`),
      rows = `Exclusive White America_sig` == "yes"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#7D1D67", alpha = 0.7),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = c(`Assimilationist`, `Assimilationist_z`),
      rows = `Assimilationist_sig` == "yes"
    )
  ) |>
  cols_hide(matches("_sig")) |>
  cols_label_with(fn = ~ ifelse(grepl("_", .x), .x, paste0(.x, "_dydx"))) |>
  tab_spanner(
    label = "Moderate Hybrid",
    columns = matches("Moder"),
    id = "1",
    replace = TRUE
  )  |>
  tab_spanner(
    label = "Restrictive Hybrid",
    columns = matches("Rest"),
    id = "2",
    replace = TRUE
  ) |>
  tab_spanner(
    label = "Inclusive America",
    columns = matches("Inclus"),
    id = "3",
    replace = TRUE
  ) |>
  tab_spanner(
    label = md("Exclusive<br>(White) America"),
    columns = matches("Exclus"),
    id = "4",
    replace = TRUE
  ) |>
  tab_spanner(
    label = "Assimilationist",
    columns = matches("Assim"),
    id = "5",
    replace = TRUE
  ) |>
  cols_label_with(fn = ~ ifelse(grepl("dydx", .x), "AME", .x)) |>
  cols_label(matches("z") ~ md("*z*"), matches("level") ~ "") |>
  fmt_markdown() |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  tab_options(
    row_group.background.color = "#1D677D",
    data_row.padding = px(10),
    table.font.size = "125%",
    column_labels.padding = px(5),
    column_labels.padding.horizontal = px(10)
  ) |>
  cols_align(align = "right", columns = everything()[-c(1:2)]) |>
  cols_align(align = "center", columns = c(1:2))


# Further stylized in LaTeX —

ame_membership_table |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_d3.tex")


# TABLE D4 ----------------------------------------------------------------

full_margins <- read_excel(
  "./data/latent gold data/1_results_xlsx/03_membership_main.xlsx",
  sheet = 4,
  skip = 1
) |>
  select(logic = 1,
         term = 3,
         prediction = 4) |>
  # Adding age — from a different sheet.
  bind_rows(
    read_excel(
      "./data/latent gold data/1_results_xlsx/03_membership_main.xlsx",
      sheet = 5,
      skip = 1
    ) |>
      select(
        logic = 1,
        term = 3,
        prediction = 4
      ) |>
      filter(str_detect(term, "20|40|60"))
  ) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    )
  )


marginal_predictions_table <-  full_margins |>
  separate(term, into = c("variable", "level"), sep = "\\(") |>
  mutate(
    level = str_remove(level, "\\)$"),
    level = str_to_title(level),
    level = ifelse(variable == "age", paste(level, "Years Old"), level),
    variable = case_when(
      variable == "party" ~ "Partisan Identity",
      str_detect(variable, "generat") ~ "Generation",
      str_detect(variable, "dail") ~ "News Consumption",
      variable == "age" ~ "Age",
      variable == "university" ~ "University Status",
      variable == "gender" ~ "Sex or Gender",
      str_detect(variable, "relig") ~ "Religious Affiliation",
      TRUE ~ str_to_title(variable)
    )
  ) |>
  mutate(
    variable = fct_relevel(
      variable |> fct_inorder(),
      "Partisan Identity",
      "Race",
      "Sex or Gender",
      "Age",
      "Religious Affiliation",
      "Generation",
      "University Status",
      "News Consumption"
    ),
    level = fct_relevel(fct_inorder(level), "Other", after = Inf),
    logic = fct_inorder(logic)
  ) |>
  arrange(variable, level) |>
  pivot_wider(names_from = logic, values_from = prediction) |>
  group_by(variable) |>
  gt() |>
  fmt_number() |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  tab_options(
    row_group.background.color = "#1D677D",
    data_row.padding = px(10),
    table.font.size = "125%",
    column_labels.padding = px(5),
    column_labels.padding.horizontal = px(10)
  ) |>
  cols_align(align = "center", columns = everything()) |>
  cols_label(level = "")

# Stylize further in LaTeX —

marginal_predictions_table |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_d4.tex")
