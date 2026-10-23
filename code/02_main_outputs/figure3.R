# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(readxl, gt, tidyverse, patchwork, colorspace, hrbrthemes, ggtext)

# DISTAL OUTCOME ----------------------------------------------------------

distal_outcome <- read_excel("./data/latent gold data/1_results_xlsx/02_distal_immigrant.xlsx")

distal_outcome_1 <- distal_outcome |>
  select(logic = 3, ame = 4, z = 5) |>
  mutate(across(c(ame, z), as.numeric)) |>
  filter(str_detect(logic, "Cluster")) |>
  drop_na(z) |>
  mutate(
    logic = case_when(
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

# LEFT PANEL --------------------------------------------------------------

figure3_left <- distal_outcome_1 |>
  ggplot(mapping = aes(
    x = ame,
    xmin = conf.low,
    xmax = conf.high,
    y = fct_rev(logic)
  )) +
  geom_vline(xintercept = 0, linetype = "dotdash") +
  geom_linerange(alpha = 0.5,
                 linewidth = 0.85,
                 colour = "#CF4E9CFF") +
  geom_point(
    shape = 21,
    size = 4.5,
    stroke = 1,
    colour = "white",
    fill = "#CF4E9CFF"
  ) +
  labs(x = "AME of Class Membership on Pro-Immigrant Sentiment", y = "") +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    axis_title_size = 12,
    grid_col = "lightgrey"
  ) +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, "**Moderate Hybrid**", x)
    }
  ) +
  theme(
    axis.text.x.bottom = element_markdown(),
    axis.text.y = element_text(hjust = 0.5),
    axis.title.x.bottom = element_markdown(hjust = 0.5, margin = margin(t = 15))
  )

# RIGHT PANEL -------------------------------------------------------------

distal_outcome_pred <- read_excel(
  "./data/latent gold data/1_results_xlsx/02_distal_immigrant.xlsx",
  sheet = 2,
  skip = 1
) |>
  select(logic = 3, prediction = 4) |>
  filter(str_detect(logic, "Cluster")) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder()
  )

custom_palette <- rev(sequential_hcl(4, palette = "SunsetDark"))

figure3_right <- distal_outcome_pred |>
  rename_all(str_to_title) |>
  gt() |>
  fmt_number() |>
  cols_label(Logic = "") |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  tab_spanner(label = "Pro-Immigrant Sentiment", columns = 2) |>
  data_color(method = "numeric",
             palette = custom_palette,
             domain = c(2:5)) |>
  cols_align(align = "center")

# STITCH TOGETHER ---------------------------------------------------------


tags <- c("**A:**&emsp;**Average Marginal Effects (AME)**",
  "**B:**&emsp;**Predicted Values**"
)

figure_3 <- figure3_left + wrap_table(figure3_right, panel = "rows", space = "full") +
  plot_annotation(
    tag_levels = list(tags),
    theme = theme(plot.margin = margin(5.5, 5.5, 0, 5.5))
  ) &
  theme(
    plot.margin = margin(5.5, 5.5, 0, 5.5),
    plot.tag.location = "plot",
    plot.tag.position = c((30 / 72.27) / 12.35, 1.2),
    plot.tag = element_markdown(
      family = "Montserrat", size = 15, hjust = 0, vjust = 1
    )
  )

ggsave(
  figure_3,
  filename = "./figures_tables/figure3.png",
  dpi = 1000,
  width = 12.35,
  height = 3.85
)
