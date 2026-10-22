# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, hrbrthemes, readxl, scales, ggtext, gt)

# FIGURE C1 ---------------------------------------------------------------

class_comparison <- read_excel("./data/latent gold data/1_results_xlsx/appendix/01_alt_lcr.xlsx",
                               sheet = 1)

class_comparison_1 <- class_comparison |>
  pivot_longer(!k, names_to = "measure", values_to = "value") |>
  mutate(measure = ifelse(measure == "bic", "Bayesian Information Criterion (BIC)",
                          "R<sup>2</sup>"),
         selected = ifelse(k == 5, "Analytic Model", "no"),
         .after = k)

class_comparison_plot <-  class_comparison_1 |>
  ggplot(mapping = aes(x = as_factor(k), y = value, group = 1)) +
  facet_wrap(~measure, scales = "free") +
  scale_y_continuous(labels = function(x){ifelse(x > 1, comma(x), x)}) +
  geom_line(alpha = 0.3, lty = "dotdash", linewidth = 0.5) +
  geom_point(mapping = aes(colour = selected), size = 3.75) +
  scale_colour_manual(values = c("Analytic Model" = "#CF4E9CFF",
                                 "no" = "#97A1A7FF"),
                      breaks = "Analytic Model")+
  labs(x = "Number of Latent Segments", y = "", colour = "")  +
  theme_ipsum(base_family = "Montserrat",
              grid_col = "lightgrey",
              grid = "XY",
              strip_text_size = 15,
              axis_title_size = 13,
              axis_text_size = 13) +
  annotate("rect",
           xmin = c(0, 5.5), xmax = c(4.5, 10.5),
           ymin = -Inf, ymax = Inf,
           alpha = 0.07, fill = "black", colour = "lightgrey") +
  theme(legend.position = "bottom",
        legend.text.position = "bottom",
        legend.text = element_text(size = 13),
        strip.text = element_markdown(margin = margin(b = 15)),
        axis.title.x = element_text(margin = margin(t = 15)))


ggsave(class_comparison_plot,
       filename = "./figures_tables/appendix/figure_c1.png",
       bg = "white",
       device = grDevices::png,
       dpi = 1000,
       width = 14, height = 5.25)

# TABLE C1 ----------------------------------------------------------------

estimated_margins <- read_excel("./data/latent gold data/1_results_xlsx/01_main_lcr.xlsx", sheet = 2,
                                skip = 1) |>
  select(term = 3, logic = 5, prediction = 6) |>
  mutate(logic = case_when(str_detect(logic, "1") ~ "Moderate Hybrid",
                           str_detect(logic, "2") ~ "Restrictive Hybrid",
                           str_detect(logic, "3") ~ "Inclusive America",
                           str_detect(logic, "4") ~ "Exclusive White America",
                           TRUE ~ "Assimilationist"))

margins_table <-  estimated_margins |>
  separate(term, into = c("attribute", "level"), sep = "\\(")  |>
  mutate(level = str_remove(level, "\\)$"),
         attribute = case_when(str_detect(attribute, "famil") ~ "Immigrant Generation",
                               str_detect(attribute, "engl") ~ "English",
                               str_detect(attribute, "civic") ~ "Civic Engagement",
                               str_detect(attribute, "politic") ~ "Political Ideology",
                               str_detect(attribute, "favor") ~ "Favorite Holiday",
                               TRUE ~ str_to_title(attribute))) |>
  pivot_wider(names_from = logic, values_from = prediction) |>
  group_by(attribute) |>
  gt() |>
  cols_label(level = "",
             matches("White") ~ md("Exclusive<br> White America")) |>
  fmt_number(decimals = 2) |>
  cols_align(align = "center", columns = everything()) |>
  opt_table_font(font = list(google_font(name = "Montserrat"))) |>
  data_color(method = "numeric",
             palette = "ggsci::red_material")

# Need further adjustments in LaTeX to toggle colour scheme:

margins_table |>
  as_latex() |>
  as.character() |>
  write_lines("./figures_tables/appendix/table_c1.tex")
