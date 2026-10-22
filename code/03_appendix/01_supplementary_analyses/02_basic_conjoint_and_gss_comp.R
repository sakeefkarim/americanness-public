# PREAMBLE ----------------------------------------------------------------

pacman::p_load(tidyverse, haven, marginaleffects, gssr)

# LOADING THE DATA --------------------------------------------------------

base_conjoint <- readRDS("./data/experimental data/logics.rds") |>
  left_join(readRDS("./data/experimental data/non_conjoint_data.rds")) |>
  select(-pro_immigration)

# FULLY INTERACTED MODEL --------------------------------------------------

attributez <- c(
  "race",
  "familybackground",
  "religion",
  "englishfluency",
  "politicalideology",
  "civicengagement",
  "favoriteholiday",
  "education",
  "residence"
)

full_interaction <- reformulate(paste(attributez, "r_party", sep = " * "), response = "dva")

mod <- lm(full_interaction, data = base_conjoint, weights = adj_weight)

# MARGINAL MEANS ----------------------------------------------------------

marginal_means_party <- attributez |>
  map(\(x) {
    avg_predictions(
      mod,
      newdata = "balanced",
      by = c(x, "r_party"),
      wts = TRUE,
      vcov = ~ responseid
    ) |>
      as_tibble() |>
      mutate(attribute = x, level = as.character(.data[[x]])) |>
      select(attribute, level, r_party, estimate, conf.low, conf.high)
  }) |>
  list_rbind()

grand_mean <- avg_predictions(mod,
                              newdata = "balanced",
                              wts = TRUE,
                              vcov = ~ responseid)

mm_party <- marginal_means_party |>
  filter(!r_party == "Other") |>
  mutate(
    r_party = fct_relevel(r_party, "Independent", after = 1),
    attribute = str_to_title(attribute),
    attribute = case_when(
      str_detect(attribute, "Famil") ~ "Family Background",
      str_detect(attribute, "Englis") ~ "English",
      str_detect(attribute, "Politic") ~ "Political Ideology",
      str_detect(attribute, "Civic") ~ "Civic Engagement",
      str_detect(attribute, "hol") ~ "Favorite Holiday",
      TRUE ~ attribute
    ) |>
      fct_inorder(),
    level = fct_inorder(ifelse(
      str_detect(level, "Saint"), "Saint Patrick's Day", level
    ))
  )


# STANDARDIZED ------------------------------------------------------------

base_conjoint_stnd <-  base_conjoint |> mutate(dva_stnd = scale(dva) |>
                                                 as.numeric(), .after = dva)

full_interaction_stnd <- reformulate(paste(attributez, "r_party", sep = " * "), response = "dva_stnd")

mod_stnd <- lm(full_interaction_stnd, data = base_conjoint_stnd, weights = adj_weight)

marginal_means_party_stnd <- attributez |>
  keep(\(x) x %in% c("religion", "familybackground")) |>
  map(\(x) {
    avg_predictions(
      mod_stnd,
      newdata = "balanced",
      by = c(x, "r_party"),
      wts = TRUE,
      vcov = ~ responseid
    ) |>
      as_tibble() |>
      mutate(attribute = x, level = as.character(.data[[x]])) |>
      select(attribute, level, r_party, estimate, conf.low, conf.high)
  }) |>
  list_rbind()

select_comparisons <- avg_comparisons(
  mod_stnd,
  variables = c("religion", "familybackground"),
  by = "r_party",
  wts = TRUE,
  vcov = ~ responseid,
  newdata = datagrid(
    r_party = c("Democrat", "Republican"),
    grid_type = "balanced"
  )
)

# COMPARISON TO GSS -------------------------------------------------------

gss_2024 <- gss_get_yr(year = 2024) |>
  select(1:2, partyid, wt = wtssnrps, ambornin:amfeel)

gss_2024_1 <- gss_2024 |> mutate(
  partyid = as_factor(partyid),
  party3 = case_when(
    str_detect(partyid, "independ") ~ "Independent",
    str_detect(partyid, "democr") ~ "Democrat",
    str_detect(partyid, "repub") ~ "Republican",
    str_detect(partyid, "other p") ~ "Other",
    TRUE ~ NA_character_
  ),
  .before = partyid
) |>
  select(-partyid)  |>
  drop_na(party3) |>
  mutate(across(c(5:9), ~ as.numeric(fct_rev(
    fct_drop(as_factor(zap_missing(.)))
  ))),
  party3 = fct_relevel(party3, "Democrat", "Independent", "Republican"))

gss_2024_2 <- gss_2024_1 |>
  pivot_longer(!c(1:4), names_to = "indicator", values_to = "score") |>
  mutate(score = scale(score) |> as.numeric(), id = as_factor(id))

gss_mod <- lm(score ~ indicator * party3, data = gss_2024_2, weights = wt)

gss_stacked <- avg_predictions(
  gss_mod,
  by = c("indicator", "party3"),
  wts = TRUE,
  vcov = ~ id,
  newdata = datagrid(
    party3 = c("Democrat", "Republican"),
    indicator = unique,
    grid_type = "balanced"
  )
)
gss_preds <- gss_stacked |>
  filter(indicator %in% c("ambornin", "amchrstn")) |>
  as_tibble() |>
  select(outcome = indicator, party = party3, estimate) |>
  mutate(
    type = "GSS (2024) — Importance for *Being American*",
    outcome = ifelse(outcome == "amchrstn", "Being Christian", "Birth in America")
  )

gss_preds_1 <- gss_preds |>
  bind_rows(
    select_comparisons |>
      as_tibble() |>
      # Isolating largest contrasts
      filter(str_detect(contrast, "3rd|Athe")) |>
      mutate(
        # Flipping order so it's Christian vs ref:
        estimate = ifelse(term == "religion", -estimate, estimate),
        term = ifelse(
          str_detect(term, "family"),
          "3rd+ Gen ***vs*** 1st Gen",
          "Christian ***vs*** Atheist"
        ),
        type = "Conjoint Experiment (2022) — *Americanness* of Profiles",
        .before = 1
      ) |>
      select(
        1,
        outcome = 2,
        party = r_party,
        estimate,
        context = contrast
      )
  )

# SAVE --------------------------------------------------------------------

# This line only needs to be run once —

save(
  mm_party,
  grand_mean,
  select_comparisons,
  marginal_means_party_stnd,
  gss_preds_1,
  file = "./data/misc/marginal_means_data.RData"
)