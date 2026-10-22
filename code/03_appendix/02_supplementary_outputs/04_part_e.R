# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse,
               ggtext,
               readxl,
               hrbrthemes,
               ggnewscale,
               ggh4x)

# FIGURE E1 ---------------------------------------------------------------

# LOAD THE DATA

load("./data/misc/marginal_means_data.RData")

figure_e1 <-  mm_party |>
  mutate(attribute = fct_recode(attribute, "Immigrant Generation" = "Family Background")) |>
  ggplot(
    aes(
      x = estimate,
      y = level |> fct_rev(),
      xmin = conf.low,
      xmax = conf.high,
      colour = r_party,
      fill = r_party
    )
  ) +
  facet_wrap( ~ attribute, ncol = 3, scales = "free_y") +
  geom_vline(
    xintercept = grand_mean |>
      pull(estimate),
    lty = "dotdash",
    linewidth = 0.8
  ) +
  geom_linerange(aes(alpha = r_party),
                 position = position_dodge(width = 0.85),
                 linewidth = 1.2) +
  scale_alpha_manual(values = c(0.65, 0.35, 0.65), guide = "none") +
  new_scale("alpha") +
  geom_point(
    aes(alpha = r_party),
    position = position_dodge(width = 0.85),
    size = 5.75,
    stroke = 1.3,
    shape = 21,
    colour = "white"
  ) +
  scale_alpha_manual(values = c(1, 0.45, 1)) +
  theme_ipsum(
    base_family = "Montserrat",
    strip_text_size = 21,
    axis_text_size = 19,
    axis_title_size = 23.5,
    grid = "XY",
    grid_col = "lightgrey"
  ) +
  labs(x = "Estimated *Americanness* Rating (Marginal Mean on 1-7 Scale)", y = "") +
  scale_colour_manual(values = c("#00AEF3", "#5d3c85", "#E81B23")) +
  scale_fill_manual(values = c("#00AEF3", "#5d3c85", "#E81B23")) +
  theme(
    panel.border = element_rect(
      colour = "grey80",
      fill = NA,
      linewidth = 0.4
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text.position = "bottom",
    legend.text = element_text(size = 18),
    legend.key.spacing.x = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", margin = margin(b = 15)),
    axis.title.x = element_markdown(margin = margin(t = 20, b = 5))
  ) +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 5, "5", x)
    }
  ) +
  guides(fill = guide_legend(override.aes = list(
    alpha = c(1, 0.6, 1), colour = alpha(c("#00AEF3", "#5d3c85", "#E81B23"), c(0.55, 0.25, 0.55))
  )))

ggsave(
  figure_e1,
  filename = "./figures_tables/appendix/figure_e1.png",
  device = grDevices::png,
  bg = "white",
  dpi = 1000,
  height = 14.5,
  width = 20
)

# FIGURE E2 ---------------------------------------------------------------

figure_e2 <-  gss_preds_1 |>
  ggplot(
    mapping = aes(
      x = estimate,
      y = outcome |> fct_inorder() |> fct_rev(),
      group = interaction(type, outcome),
      fill = party
    )
  ) +
  facet_wrap( ~ type, ncol = 1, scales = "free_y") +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, "0", x)
    }
  ) +
  geom_vline(xintercept = 0, lty = "dotdash") +
  geom_line(linewidth = 2, colour = "lightgrey") +
  geom_point(
    size = 6.5,
    stroke = 1,
    shape = 21,
    colour = "white"
  ) +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    strip_text_size = 16,
    axis_text_size = 14,
    axis_title_size = 15,
    grid_col = "lightgrey"
  ) +
  labs(x = "Based on Standardized Predictions of ***y***", y = "") +
  scale_fill_manual(values = c("#00AEF3", "#E81B23"), name = "") +
  theme(
    legend.position = "bottom",
    strip.text = element_markdown(margin = margin(b = 10), face = "bold"),
    axis.title.x = element_markdown(margin = margin(t = 15)),
    legend.text = element_text(size = 14),
    legend.key.spacing.x  = unit(1, "cm"),
    legend.text.position = "bottom",
    axis.text.y.left = element_markdown(hjust = 0.5),
    axis.text = element_markdown()
  )


ggsave(
  figure_e2,
  filename = "./figures_tables/appendix/figure_e2.png",
  device = grDevices::png,
  bg = "white",
  dpi = 1000,
  height = 5.25,
  width = 11.35
)

# FIGURE E3 ---------------------------------------------------------------

six_class_ame <- read_excel(
  "./data/latent gold data/1_results_xlsx/appendix/01_alt_lcr.xlsx",
  sheet = 2,
  skip = 1
) |>
  select(
    logic = 1,
    party = 3,
    ame = 4,
    z = 5
  ) |>
  filter(!str_detect(party, "Other")) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  mutate(
    logic = str_replace(logic, "Cluster\\((\\d+)\\)", "*k*<sub>\\1</sub>") |>
      fct_inorder(),
    party = str_remove_all(party, "party\\(|\\)"),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se,
    model = "Six Latent Segments"
  )

three_class_ame <- read_excel(
  "./data/latent gold data/1_results_xlsx/appendix/01_alt_lcr.xlsx",
  sheet = 4,
  skip = 1
) |>
  select(
    logic = 1,
    party = 3,
    ame = 4,
    z = 5
  ) |>
  filter(!str_detect(party, "Other")) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  mutate(
    logic = str_replace(logic, "Cluster\\((\\d+)\\)", "*k*<sub>\\1</sub>") |>
      fct_inorder(),
    party = str_remove_all(party, "party\\(|\\)"),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se,
    model = "Three Latent Segments & Random Effects"
  )

combined_robustness_ame <- bind_rows(six_class_ame, three_class_ame)

# Reference

ref <- "<img src='data/misc/demo.png' width='25.5'  />"

figure_e3 <-  combined_robustness_ame |>
  ggplot(
    mapping =  aes(
      x = ame,
      y = logic |> fct_rev(),
      xmin = conf.low,
      xmax = conf.high,
      colour = party,
      fill = party
    )
  ) +
  facet_wrap( ~ model, scales = "free_y") +
  geom_vline(xintercept = 0,
             linetype = "dotdash",
             linewidth = 0.6) +
  geom_linerange(
    alpha = 0.5,
    linewidth = 0.85,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 4.5,
    stroke = 1,
    colour = "white",
    shape = 21,
    position = position_dodge(width = 0.55)
  )   +
  labs(x = "Average Marginal Effect of<br>Partisan Identity on Class Membership", y = "")  +
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
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text.position = "bottom",
    legend.text = element_markdown(size = 12),
    legend.key.spacing.x = unit(1.5, "lines"),
    axis.text.x.bottom = element_markdown(),
    axis.text.y.left = element_markdown(),
    strip.text = element_text(
      hjust = 0.5,
      face = "bold",
      margin = margin(b = 15)
    ),
    panel.spacing.x = unit(0.8, "cm"),
    axis.title.x = element_markdown(
      size = 14,
      hjust = 0.5,
      margin = margin(t = 15)
    )
  )


ggsave(
  figure_e3,
  filename = "./figures_tables/appendix/figure_e3.png",
  device = grDevices::png,
  dpi = 1000,
  height = 6.25,
  width = 10,
  bg = "white"
)

# FIGURE E4 ---------------------------------------------------------------

# Original

class_ame_original <-  read_excel(
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
      str_detect(logic, "4") ~ "Exclusive<br>White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    party = str_remove_all(party, "party\\(|\\)"),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

# Leaners

class_ame_lean <-  read_excel(
  "./data/latent gold data/1_results_xlsx/appendix/02_alt_three_step.xlsx",
  sheet = "AMEs (Leaners)",
  skip = 1
) |>
  select(
    logic = 1,
    party = 3,
    ame = 4,
    z = 5
  ) |>
  filter(!str_detect(party, "Other")) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive<br>White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    party = str_remove_all(party, "party_lean\\(|\\)"),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

combined_class_ame <- bind_rows(
  class_ame_original |> mutate(model = "Original"),
  class_ame_lean |> mutate(model = "Treating Leaners as Partisans")
)


design <- "
  AABBCC
  #DDEE#
"

figure_e4 <-  combined_class_ame |>
  mutate(
    party =  ifelse(
      str_detect(party, "Repub"),
      "<img src='data/misc/gop.png' width='25.5'  />",
      party
    ),
    party = fct_rev(party)
  ) |>
  ggplot(
    mapping =  aes(
      x = ame,
      y = party,
      xmin = conf.low,
      xmax = conf.high,
      shape = model |> fct_rev(),
      colour = party,
      fill = party
    )
  ) +
  facet_manual( ~ logic, design = design) +
  geom_vline(xintercept = 0,
             linetype = "dotdash",
             linewidth = 0.6) +
  geom_linerange(
    alpha = 0.5,
    linewidth = 0.85,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  geom_point(
    size = 4.5,
    stroke = 1,
    colour = "white",
    position = position_dodge(width = 0.75)
  )  +
  scale_shape_manual(values = c(23, 21)) +
  labs(x = "Average Marginal Effect of<br>Partisan Identity on Class Membership", y = "") +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    strip_text_size = 12,
    grid_col = "lightgrey"
  ) +
  scale_colour_manual(values = rev(c("#E81B23", "#5d3c85")), guide = "none") +
  scale_fill_manual(values = rev(c("#E81B23", "#5d3c85")), guide = "none") +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, ref, x)
    }
  ) +
  guides(shape = guide_legend(
    override.aes = list(
      fill = "grey70",
      colour = "white",
      size = 6
    ),
    reverse = TRUE
  )) +
  theme(
    panel.border = element_rect(
      colour = "grey80",
      fill = NA,
      linewidth = 0.4
    ),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_markdown(size = 12),
    legend.key.spacing.x = unit(1.5, "lines"),
    axis.text.x.bottom = element_markdown(),
    axis.text.y.left = element_markdown(),
    strip.text = element_markdown(hjust = 0.5),
    panel.spacing.x = unit(0.8, "cm"),
    axis.title.x = element_markdown(
      size = 14,
      hjust = 0.5,
      margin = margin(t = 15)
    )
  )

ggsave(
  figure_e4,
  filename = "./figures_tables/appendix/figure_e4.png",
  device = grDevices::png,
  dpi = 1000,
  height = 6.25,
  width = 10,
  bg = "white"
)

# FIGURE E5 ---------------------------------------------------------------

distal_outcome_race <- read_excel(
  "./data/latent gold data/1_results_xlsx/appendix/02_alt_three_step.xlsx",
  sheet = 2,
  skip = 1
)

distal_outcome_race_1 <- distal_outcome_race |>
  select(
    logic = 3,
    race = 5,
    ame = 6,
    z = 7
  ) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  mutate(
    logic = case_when(
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    race = str_extract(race, "(?<=\\().*?(?=\\))") |>
      fct_inorder(),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

figure_e5 <-  distal_outcome_race_1 |>
  ggplot(mapping = aes(
    x = ame,
    xmin = conf.low,
    xmax = conf.high,
    y = fct_rev(logic)
  )) +
  facet_wrap( ~ race, scales = "free_x") +
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
  labs(x = "Average Marginal Effect of Class Membership<br>on Pro-Immigrant Sentiment", y = "") +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    axis_title_size = 12,
    strip_text_size = 14,
    axis_text_size = 12,
    grid_col = "lightgrey"
  ) +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, "**Moderate<br>Hybrid**", x)
    }
  ) +
  theme(
    axis.text.x.bottom = element_markdown(),
    axis.text.y = element_text(hjust = 0.5),
    axis.title.x = element_markdown(hjust = 0.5, margin = margin(t = 20))
  )

ggsave(
  figure_e5,
  filename = "./figures_tables/appendix/figure_e5.png",
  device = grDevices::png,
  dpi = 1000,
  height = 6.25,
  width = 10,
  bg = "white"
)


# FIGURE E6 ---------------------------------------------------------------

class_ame_race <- read_excel(
  "./data/latent gold data/1_results_xlsx/appendix/02_alt_three_step.xlsx",
  sheet = 3,
  skip = 1
) |>
  select(
    logic = 1,
    party = 3,
    race = 5,
    ame = 6,
    z = 7
  ) |>
  mutate(across(c(ame, z), as.numeric)) |>
  drop_na(z) |>
  filter(!str_detect(party, "Othe")) |>
  mutate(
    logic = case_when(
      str_detect(logic, "1") ~ "Moderate Hybrid",
      str_detect(logic, "2") ~ "Restrictive Hybrid",
      str_detect(logic, "3") ~ "Inclusive America",
      str_detect(logic, "4") ~ "Exclusive White America",
      TRUE ~ "Assimilationist"
    ) |>
      fct_inorder(),
    across(c(party, race), ~ fct_inorder(str_extract(
      ., "(?<=\\().*?(?=\\))"
    ))),
    # Computing 95% CIs
    se = abs(ame / z),
    conf.low = ame - 1.96 * se,
    conf.high = ame + 1.96 * se
  )

figure_e6 <-  class_ame_race |>
  ggplot(
    mapping =  aes(
      x = ame,
      y = logic |> fct_rev(),
      xmin = conf.low,
      xmax = conf.high,
      colour = party,
      fill = party
    )
  ) +
  facet_grid(logic ~ race, scales = "free", switch = "y") +
  geom_vline(xintercept = 0,
             linetype = "dotdash",
             linewidth = 0.6) +
  geom_linerange(
    alpha = 0.5,
    linewidth = 0.95,
    position = position_dodge(width = 0.75)
  ) +
  geom_point(
    size = 5.5,
    stroke = 1,
    colour = "white",
    shape = 21,
    position = position_dodge(width = 0.75)
  ) +
  labs(x = "Average Marginal Effect of<br>Partisan Identity on Class Membership", y = "")  +
  theme_ipsum(
    base_family = "Montserrat",
    grid = "XY",
    axis_text_size = 12.5,
    strip_text_size = 16,
    grid_col = "lightgrey"
  ) +
  scale_colour_manual(values = c("#E81B23", "#5d3c85")) +
  scale_fill_manual(values = c("#E81B23", "#5d3c85")) +
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
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text.position = "bottom",
    legend.text = element_markdown(size = 12),
    legend.key.spacing.x = unit(1.5, "lines"),
    axis.text.x.bottom = element_markdown(),
    axis.text.y = element_blank(),
    strip.text.x = element_text(hjust = 0.5, margin = margin(b = 15)),
    strip.text.y.left = element_text(angle = 0, hjust = 0.5),
    panel.spacing.x = unit(0.8, "cm"),
    axis.title.x = element_markdown(
      size = 16,
      hjust = 0.5,
      margin = margin(t = 15)
    )
  )

ggsave(
  figure_e6,
  filename = "./figures_tables/appendix/figure_e6.png",
  device = grDevices::png,
  dpi = 1000,
  height = 8.5,
  width = 13.85,
  bg = "white"
)
