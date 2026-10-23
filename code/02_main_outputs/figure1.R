# PREAMBLE ----------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse)

# PALETTE -----------------------------------------------------------------

magenta <- "#CF4E9CFF"
blush   <- "#f0c9e1"
ink     <- "#311a4d"
haze    <- "#ECB8D7"

# NODES -------------------------------------------------------------------

# Coordinates derived via LLM assistance (Claude)

nodes <- tribble(
  ~id,  ~x,   ~y,   ~label,                   ~shape,
  "X",  1.8,  3.2,  "italic(X)",              21,
  "z1", 0,    2,    "italic(z)[1*italic(t)]", 22,
  "z2", 0,    1,    "italic(z)[2*italic(t)]", 22,
  "zk", 0,   -0.5,  "italic(z)[italic(jt)]",  22,
  "y",  3.5,  1,    "italic(y)[italic(t)]",   22
)

covariate <- filter(nodes, id == "X")
outcome   <- filter(nodes, id == "y")

# EDGES -------------------------------------------------------------------

trim_segment <- function(x, y, xend, yend, pad_start = 0.30, pad_end = 0.30) {
  len <- sqrt((xend - x)^2 + (yend - y)^2)
  ux  <- (xend - x) / len
  uy  <- (yend - y) / len
  tibble(
    x    = x + pad_start * ux,
    y    = y + pad_start * uy,
    xend = xend - pad_end * ux,
    yend = yend - pad_end * uy
  )
}

paths <- tribble(
  ~from, ~entry_y, ~pad_end, ~hit_at,
  "z1",  1.15,     0.35,     0.42,
  "z2",  1.00,     0.35,     0.20,
  "zk",  0.85,     0.35,     0.50,
  "X",   1.15,     0.25,     NA
) |>
  left_join(select(nodes, from = id, x, y), by = "from")

direct_arrows <- paths |>
  with(trim_segment(x, y, outcome$x, entry_y, pad_end = pad_end))

modifier_arrows <- paths |>
  filter(from != "X") |>
  mutate(
    hit_x = x + hit_at * (outcome$x - x),
    hit_y = y + hit_at * (entry_y - y)
  ) |>
  with(trim_segment(covariate$x, covariate$y, hit_x, hit_y, pad_end = 0))


modifier_heads <- modifier_arrows |>
  mutate(
    x = xend - 0.001 * (xend - x),
    y = yend - 0.001 * (yend - y)
  )

ellipsis_y <- seq(0.55, -0.05, length.out = 3)

# PLOT --------------------------------------------------------------------

head <- function(len) arrow(length = unit(len, "pt"), type = "closed", angle = 22)

lcr_diagram <-
  ggplot(mapping = aes(x = x, y = y)) +
  geom_segment(
    data = modifier_arrows,
    aes(xend = xend, yend = yend),
    colour = haze,
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_segment(
    data = modifier_heads,
    aes(xend = xend, yend = yend),
    arrow = head(5),
    colour = haze,
    linewidth = 0.35
  ) +
  geom_segment(
    data = direct_arrows,
    aes(xend = xend, yend = yend),
    arrow = head(6),
    colour = magenta,
    linewidth = 0.45,
    lineend = "round"
  ) +
  geom_point(
    data = nodes,
    aes(shape = shape),
    size = 18,
    fill = blush,
    colour = magenta,
    stroke = 1
  ) +
  geom_text(
    data = nodes,
    aes(label = label),
    parse = TRUE,
    size = 5,
    colour = ink,
    family = "EB Garamond"
  ) +
  annotate("point", x = 0, y = ellipsis_y, size = 0.8, colour = magenta) +
  scale_shape_identity() +
  coord_fixed(xlim = c(-0.8, 4.3), ylim = c(-1.2, 3.8), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(lcr_diagram,
       dpi = 1000,
       device = grDevices::png,
       filename = "figures_tables/figure1.png",
       width = 5.25, height = 4.45)
