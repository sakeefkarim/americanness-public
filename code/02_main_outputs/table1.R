# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(readxl, tidyverse, gt)

# CLASS MEMBERSHIP AND PARTISANSHIP ---------------------------------------

class_predictions_party <-  read_excel(
  "./data/latent gold data/1_results_xlsx/03_membership_main.xlsx",
  sheet = 4,
  skip = 1
) |>
  select(logic = 1,
         party = 3,
         prediction = 4) |>
  filter(str_detect(party, "party"), !str_detect(party, "Othe")) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    party = str_remove_all(party, "party\\(|\\)") |>
      fct_relevel("Democrat", "Independent")
  ) |>
  arrange(logic, party)

# TABLE 1 -----------------------------------------------------------------

pal_dem <- c("#EBF3FA", "#BFD7EE", "#7FB3D9", "#4D91C2", "#2A6FA0")

pal_ind <- c("#F0ECF5", "#D4C6E4", "#AB8FC9", "#8B63B0", "#6B4292")

pal_rep <- c("#FAEDEF", "#EDCACF", "#D8949E", "#C46372", "#A93E4F")

table1 <- class_predictions_party |>
  pivot_wider(names_from = party, values_from = prediction) |>
  gt() |>
  fmt_number() |>
  tab_spanner(label = "Partisan Identity", columns = 2:4) |>
  cols_label(logic = "") |>
  cols_align(align = "center") |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  data_color(
    matches("Dem"),
    method = "numeric",
    palette = pal_dem,
    domain = c(0, 0.4)
  ) |>
  data_color(
    matches("Rep"),
    method = "numeric",
    palette = pal_rep,
    domain = c(0, 0.4)
  ) |>
  data_color(
    matches("Indep"),
    method = "numeric",
    palette = pal_ind,
    domain = c(0, 0.4)
  )

# Need further adjustments in LaTeX to produce colour scheme:

table1 |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/table1.tex")

# High Quality png version:

ggsave(
  as_gtable(table1),
  filename = "./figures_tables/table1.png",
  device = grDevices::png,
  dpi = 1000,
  width = 7.5,
  height = 2.3,
  bg = "white"
)
