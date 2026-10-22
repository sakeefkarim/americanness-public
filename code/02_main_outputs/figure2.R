# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, hrbrthemes, colorspace, ggtext, readxl,
               patchwork, scales)

# RESULTS -----------------------------------------------------------------

estimated_margins <- read_excel(
  "./data/latent gold data/1_results_xlsx/01_main_lcr.xlsx",
  sheet = 2,
  skip = 1
) |>
  select(
    term = 3,
    logic = 5,
    prediction = 6,
    se = 7
  ) |>
  mutate(
    conf.low  = prediction - qnorm(0.975) * se,
    conf.high = prediction + qnorm(0.975) * se,
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    )
  )

logics <-  c(
  "Moderate Hybrid",
  "Restrictive Hybrid",
  "Inclusive America",
  "Exclusive White America",
  "Assimilationist",
  "Overall"
)

# MODEL ESTIMATED MEANS, PROBS --------------------------------------------

model_means <- read_excel(
  "./data/latent gold data/1_results_xlsx/01_main_lcr.xlsx",
  sheet = 3,
  skip = 3
) |>
  rename_with( ~ logics, c(2:7)) |>
  rename(value = 1) |>
  slice(8)  |>
  pivot_longer(!value, names_to = "logic", values_to = "prediction") |>
  slice(-6) |>
  mutate(logic = logic |> fct_inorder())


scale_estimate <- read_excel(
  "./data/latent gold data/1_results_xlsx/01_main_lcr.xlsx",
  sheet = 3,
  skip = 3
) |>
  rename_with( ~ logics, c(2:7)) |>
  rename(value = 1) |>
  slice(-8)  |>
  pivot_longer(!value, names_to = "logic", values_to = "prediction") |>
  filter(!str_detect(logic, "Over")) |>
  mutate(logic = logic |> fct_inorder())

# TOP PANEL ---------------------------------------------------------------

logic_labs <- c(
  "Moderate Hybrid"               = "<img src='./data/misc/class1_share.png' width='40.5'/><br>Moderate<br>Hybrid",
  "Restrictive Hybrid"              = "<img src='./data/misc/class2_share.png' width='40.5'/><br>Restrictive<br>Hybrid",
  "Inclusive America"          = "<img src='./data/misc/class_3_4_share.png' width='40.5'/><br>Inclusive<br>America",
  "Exclusive White America" = "<img src='./data/misc/class_3_4_share.png' width='40.5'/><br>Exclusive White<br>America",
  "Assimilationist"           = "<img src='./data/misc/class5_share.png' width='40.5'/><br>Assimilationist"
)

note <- "<span style='color: #9c9c9c; font-size: 11pt;'>(Response Probabilities)</span>"

top_panel <-
  scale_estimate |>
  ggplot(mapping = aes(
    x = value,
    y = logic,
    fill = prediction,
    label = formatC(prediction, format = "f", digits = 2)
  )) +
  facet_wrap(
    ~ logic,
    nrow = 1,
    scales = "free_y",
    labeller = labeller(logic = logic_labs)
  ) +
  geom_tile(colour = "white", linewidth = 0.2) +
  geom_text(aes(colour = after_scale(
    if_else(farver::decode_colour(fill, to = "lab")[, "l"] > 60, "grey20", "white")
  ))) +
  scale_fill_continuous_sequential(palette = "SunsetDark") +
  labs(tag = "A", x = paste("**Americanness Scale Level**", note), y = "") +
  theme_ipsum(base_family = "Montserrat",
              grid = "",
              axis_title_size = 15) +
  theme(
    plot.tag = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x.bottom = element_markdown(margin = margin(t = 10)),
    strip.text = element_markdown(
      face = "bold",
      size = 11,
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    legend.position = "none"
  )

# BOTTOM PANEL ------------------------------------------------------------

marginal_means <- estimated_margins |>
  separate(term, into = c("attribute", "level"), sep = "\\(")  |>
  mutate(
    level = str_remove(level, "\\)$"),
    attribute = case_when(
      str_detect(attribute, "famil") ~ "Immigrant Generation",
      str_detect(attribute, "engl") ~ "English",
      str_detect(attribute, "civic") ~ "Civic Engagement",
      str_detect(attribute, "politic") ~ "Political Ideology",
      str_detect(attribute, "favor") ~ "Favorite Holiday",
      TRUE ~ str_to_title(attribute)
    )
  )

label_thin <- function(n = 3,
                       min_gap = 0.4,
                       labeller = scales::label_number(accuracy = 0.1)) {
  function(x) {
    labels <- labeller(x)
    shown  <- which(!is.na(x) & near(x, round(x, 1)))
    keep   <- shown[unique(round(seq(
      1, length(shown), length.out = min(n, length(shown))
    )))]
    if (length(keep) == 3 &&
        min(diff(x[keep])) < min_gap * diff(range(x[keep])))
      keep <- keep[-2]
    replace(labels, setdiff(seq_along(x), keep), "")
  }
}

logic_labs2 <- c(
  "Moderate Hybrid"               = "Moderate<br>Hybrid",
  "Restrictive Hybrid"              = "Restrictive<br>Hybrid",
  "Inclusive America"          = "Inclusive<br>America",
  "Exclusive White America" = "Exclusive White<br>America",
  "Assimilationist"           = "Assimilationist"
)


bottom_panel <-
  marginal_means |>
  mutate(
    logic = fct_inorder(logic),
    attribute = fct_inorder(attribute)
  ) |>
  ggplot(aes(
    x = prediction,
    y = level |> fct_inorder() |> fct_rev(),
    xmin = conf.low,
    xmax = conf.high
  )) +
  facet_grid(
    attribute ~ logic,
    scales = "free",
    labeller = labeller(logic = logic_labs2, attribute = label_wrap(10)),
    space = "free",
    switch = "y"
  ) +
  geom_vline(
    aes(xintercept = prediction),
    linetype = "dotdash",
    color = "grey20",
    linewidth = 0.6,
    data = model_means
  ) +
  geom_linerange(colour = "#CF4E9CFF",
                 alpha = 0.4,
                 linewidth = 0.65) +
  geom_point(
    shape = 21,
    size = 2.75,
    fill = "#CF4E9CFF",
    colour = "white"
  ) +
  scale_x_continuous(labels = label_thin()) +
  labs(tag = "B", x = "Marginal Means", y = "") +
  theme_ipsum(base_family = "Montserrat",
              grid = "XY",
              grid_col = "grey85") +
  theme(
    legend.position = "none",
    plot.tag = element_text(size = 18, face = "bold"),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 0.5,
      size = 10.5,
      face = "bold"
    ),
    strip.text.x = element_markdown(
      face = "bold",
      size = 10.75,
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    panel.border = element_rect(colour = "grey80", fill = NA),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    axis.title.x = element_markdown(
      size = 15,
      face = "bold",
      margin = margin(t = 10, b = -5)
    ),
    axis.text.x.bottom = element_markdown(face = "bold"),
    axis.text.y = element_text(size = 9.85)
  )


# COMBINE! ----------------------------------------------------------------

# canvas width
W <- 19

pad  <- (W - 11.75) / 2

trim <- 25

trim_in <- trim / 72.27

top <- top_panel + theme(plot.margin = margin(30, 30, 30 - trim, 30))

bottom <- bottom_panel + theme(plot.margin = margin(30 - trim, 30, 30, 30))

combined <- wrap_elements(full = top) /
  (plot_spacer() + wrap_elements(full = bottom) + plot_spacer() +
     plot_layout(widths = c(pad, 11.75, pad))) +
  plot_layout(heights = c(2.85 - trim_in, 9.5 - trim_in))

ggsave(
  "./figures_tables/figure2.png",
  combined,
  width = 19.35,
  height = 2.25 + 9.5 - 2 * trim_in,
  dpi = 1000,
  device = grDevices::png
)

