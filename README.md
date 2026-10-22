# What Is Americanness?

Replication materials for *What Is Americanness? Polarization and Consensus in Cultural Logics of the American Creed.*

## Requirements

| | |
|---|---|
| [Latent GOLD 6.1](https://www.statisticalinnovations.com/shop/lg-6-1-academic/) | Academic Version; all `.lgs` files |
| R (≥ 4.2) | All `.R` files; package versions locked with [`renv`](https://rstudio.github.io/renv/) |
| Fonts | [Montserrat](https://fonts.google.com/specimen/Montserrat) and [EB Garamond](https://fonts.google.com/specimen/EB+Garamond) (Figure 1) |

## Getting Started

Fire up `americanness.Rproj` on your machine. Then, restore the project library:

```r
renv::restore()
```

This installs the *specific* package versions encoded in `renv.lock`, including [`gssr`](https://kjhealy.github.io/gssr/) from r-universe. 

## Data

Conjoint experiment fielded in 2022: 3,161 U.S. respondents, eight profiles each (25,288 ratings).

| File (`data/experimental data/`) | Contents |
|---|---|
| `logics` | Ratings in long format: Americanness rating (`dva`), nine profile attributes, weight (`adj_weight`) |
| `non_conjoint_data` | Respondent covariates and immigration attitudes |
| `quota` | Quota variables (Table A1) |

Each file is provided as `.rds` and `.csv`; `logics` also as `.sav`. `adj_weight` reweights the sample so that it aligns with the racial composition of the contemporary United States.

## Pipeline

All R paths are relative to the project root.

Latent GOLD requires absolute paths: replace `C:\path\to` in each `.lgs` file. Model output was pasted into Excel spreadsheets in `data/latent gold data/1_results_xlsx/`, which were then fed into R. This isn't necessary per se, but it is the path of least resistance. Re-estimated models need to be pasted into the same sheet layout.

All intermediate files are included. Ergo, each step runs on its own.

### Main Analysis (`code/01_main_analysis/`)

| Step | File | Output |
|---|---|---|
| 1 | `01_baseline_lcr.lgs` | Main LCR: `main.sav`, `01_main_lcr.xlsx` |
| 2 | `02_lg_input_three_step.R` | `main_posteriors.sav` |
| 3 | `03_three_step_distal.lgs` | `02_distal_immigrant.xlsx` |
| 4 | `04_three_step_membership.lgs` | `03_membership_main.xlsx` |

### Main Outputs (`code/02_main_outputs/`)

| Script | Output |
|---|---|
| `figure1.R` | Figure 1: LCR diagram |
| `figure2.R` | Figure 2: class profiles and marginal means |
| `figure3.R` | Figure 3: class membership and pro-immigrant sentiment |
| `figure4.R` | Figure 4: partisan identity and class membership |
| `table1.R` | Table 1: predicted class membership by party |

### Appendix (`code/03_appendix/`)

| Step | File | Output |
|---|---|---|
| 1 | `01_base_lcr_varying_k_1_to_10.lgs` | Fit statistics, 1–10 latent segments |
| 2 | `02_basic_conjoint_and_gss_comp.R` | `marginal_means_data.RData`; pulls in the [2024 GSS](https://gss.norc.org), too |
| 3 | `03_alternative_lcrs.lgs` | Posteriors: six-class + three-class random-effect models  |
| 4 | `04_lg_inputs_alt_three_step.R` | Three-step inputs for the alternative models |
| 5 | `05_three_step_six_class.lgs` | Step 3, six-class solution |
| 6 | `06_three_step_re.lgs` | Step 3, three-class random-effects solution |
| 7 | `07_three_step_leaners_race.lgs` | Step 3, leaners as partisans; moderation by race (class membership, distal immigration attitudes) |

| Script | Output |
|---|---|
| `01_part_a.R` | Table A1 |
| `02_part_c.R` | Figure C1, Table C1 |
| `03_part_d.R` | Tables D1–D4 |
| `04_part_e.R` | Figures E1–E6 |

Outputs are written to `figures_tables/`. Most tables were refined in LaTeX (colour schemes; quota targets for Table A1, etc.).

## Licensing

| Component | Licence |
|---|---|
| Code (`code/`) | [MIT](https://opensource.org/license/mit) (`LICENSE`) |
| Data (`data/`) | [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/) (`data/LICENSE`) |