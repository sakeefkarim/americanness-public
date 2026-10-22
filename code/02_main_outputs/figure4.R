# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(readxl, tidyverse, hrbrthemes, ggtext, ggh4x)

# AME OF PARTISAN IDENTITY ------------------------------------------------

class_ame_party <-  read_excel(
  "./data/latent gold data/1_results_xlsx/03_membership_main.xlsx",
  sheet = 3,
  skip = 1
) |>
  select(
    logic = 1,
    party = 3,
    ame = 4,
    z = 5
  ) |>
  filter(str_detect(party, "party"), !str_detect(party, "Other")) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    party = str_remove_all(party, "party\\(|\\)"),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

# FIGURE 4 ----------------------------------------------------------------

# Reference

ref <- "<img src='data/misc/demo.png' width='25.5'  />"

design <- "
  AABBCC
  #DDEE#
"

logic_labs <- c(
  "Moderate Hybrid"               = "Moderate<br>Hybrid",
  "Restrictive Hybrid"              = "Restrictive<br>Hybrid",
  "Inclusive America"          = "Inclusive<br>America",
  "Exclusive White America" = "Exclusive White<br>America",
  "Assimilationist"           = "Assimilationist"
)


figure_4 <-  class_ame_party |>
  mutate(
    party =  ifelse(
      str_detect(party, "Repub"),
      "<img src='data/misc/gop.png' width='25.5'  />",
      party
    ),
    party = fct_rev(party)
  ) |>
  ggplot(mapping =  aes(
    x = ame,
    y = party,
    xmin = conf.low,
    xmax = conf.high,
    colour = party,
    fill = party
  )) +
  facet_manual(~ logic,
               design = design,
               labeller = labeller(logic = logic_labs)) +
  geom_vline(xintercept = 0,
             linetype = "dotdash",
             linewidth = 0.6) +
  geom_linerange(alpha = 0.5, linewidth = 0.85) +
  geom_point(
    shape = 21,
    size = 4.5,
    stroke = 1,
    colour = "white"
  ) +
  labs(x = "Average Marginal Effect of<br>Partisan Identity on Class Membership", y = "") +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    strip_text_size = 12,
    grid_col = "lightgrey"
  ) +
  scale_colour_manual(values = rev(c("#E81B23", "#5d3c85"))) +
  scale_fill_manual(values = rev(c("#E81B23", "#5d3c85"))) +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, ref, x)
    }
  ) +
  theme(
    panel.border = element_rect(
      colour = "grey80",
      fill = NA,
      linewidth = 0.4
    ),
    legend.position = "none",
    axis.text.x.bottom = element_markdown(),
    axis.text.y.left = element_markdown(),
    strip.text = element_markdown(hjust = 0.5),
    panel.spacing.x = unit(0.8, "cm"),
    axis.title.x.bottom = element_markdown(
      size = 14,
      hjust = 0.5,
      margin = margin(t = 15)
    )
  )

ggsave(
  figure_4,
  filename = "./figures_tables/figure4.png",
  device = grDevices::png,
  dpi = 1000,
  height = 6,
  width = 10,
  bg = "white"
)
