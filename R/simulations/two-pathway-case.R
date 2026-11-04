# ==============================================================================
# Simulation of PM, MMS, and the degree of suppression for the two-pathway case
# ==============================================================================
#
# Data-generating process (true model)
#   X ~ N(0, 1)
#   M = gamma * X + e1                    (e1 ~ N(0, SD_M^2))
#   Y = delta * X + beta * M + e2         (e2 ~ N(0, SD_Y^2))
#
# True effects
#   DE = delta
#   IE = gamma * beta
#   TE = DE + IE
#   L  = |DE| + |IE|
#
# Measures
#   PM    = IE / TE        (proportion mediated; undefined when TE = 0)
#   MMS   = |IE| / L       (mediated magnitude share; undefined when L = 0)
#   sigma = 1 - |TE| / L   (Degree of Suppression; undefined when L = 0)
#
# Scenarios (DE and gamma are the same in all three; only beta, and so IE,
# differs)
#   Consistent mediation:     beta > 0, so IE has the same sign as DE
#   Inconsistent mediation:   beta < 0, so IE opposes DE; |IE| < |DE|, so TE is
#                             clearly away from zero
#   Near-perfect suppression: beta < 0 with IE almost equal to -DE, so TE is
#                             close to zero but not exactly zero (TE = 0 would
#                             leave the true PM undefined)
#
# Simulation
#   For each scenario, N_REPS datasets of size N_OBS are generated from the true
#   model. In each dataset, M ~ X and Y ~ X + M are fitted by OLS, and the
#   estimated effects (DE_hat = delta_hat, IE_hat = gamma_hat * beta_hat) are
#   used to compute the three measures.
# ==============================================================================

library(dplyr)
library(tibble)
library(tidyr)
library(kableExtra)

source(here::here("R/utils.R"))


# Configuration -----------------------------------------------------------
SEED <- 2026
set.seed(SEED)

N_REPS <- 10000 # simulated datasets per scenario
N_OBS <- 500 # observations per dataset
REPORT_EVERY <- 1000 # progress message interval (datasets)

# True parameters
GAMMA_TRUE <- 0.50
BETA_TRUE <- 0.40
DELTA_TRUE <- 0.40
SD_M <- 1
SD_Y <- 1

# Beta that leaves a small positive TE: IE = 0.5 * -0.76 = -0.38, TE = 0.02
BETA_NEAR_PERFECT <- -0.76

# Effect of M on Y in each scenario; its sign sets the sign of IE
BETA_SCENARIOS <- c(
  `Consistent mediation` = BETA_TRUE,
  `Inconsistent mediation` = -BETA_TRUE,
  `Near-perfect suppression` = BETA_NEAR_PERFECT
)

IE_TRUE <- GAMMA_TRUE * BETA_SCENARIOS

ZERO_TOL <- 1e-10

# Table configs
SCENARIO_ORDER <- names(BETA_SCENARIOS)
MEASURE_ROW_LABELS <- c(
  PM = "$\\widehat{\\mathrm{PM}}$",
  MMS = "$\\widehat{\\mathrm{MMS}}$",
  sigma = "$\\hat{\\sigma}$"
)
TABLE_DIGITS <- 2
TABLE_FONT_SIZE <- 9


# Simulation Functions ----------------------------------------------------
compute_measures <- function(de, ie, tol = ZERO_TOL) {
  te <- de + ie
  l <- abs(de) + abs(ie)

  tibble(
    PM = if_else(abs(te) > tol, ie / te, NA_real_),
    MMS = if_else(l > tol, abs(ie) / l, NA_real_),
    sigma = if_else(l > tol, 1 - abs(te) / l, NA_real_)
  )
}

simulate_dataset <- function(
  beta,
  n_obs = N_OBS,
  gamma = GAMMA_TRUE,
  delta = DELTA_TRUE,
  sd_m = SD_M,
  sd_y = SD_Y
) {
  tibble(
    X = rnorm(n_obs),
    M = gamma * X + rnorm(n_obs, sd = sd_m),
    Y = delta * X + beta * M + rnorm(n_obs, sd = sd_y)
  )
}

estimate_measures <- function(data) {
  gamma_hat <- coef(lm(M ~ X, data = data))[["X"]]
  y_coefs <- coef(lm(Y ~ X + M, data = data))

  compute_measures(
    de = y_coefs[["X"]],
    ie = gamma_hat * y_coefs[["M"]]
  )
}

run_scenario <- function(
  beta,
  scenario,
  n_reps = N_REPS,
  report_every = REPORT_EVERY
) {
  lapply(
    seq_len(n_reps),
    \(i) {
      result <- estimate_measures(simulate_dataset(beta))
      if (i %% report_every == 0) {
        message(
          "Simulation completed for dataset ",
          i,
          " of ",
          n_reps,
          " [",
          scenario,
          "]"
        )
      }
      result
    }
  ) |>
    bind_rows()
}


# Running Simulations -----------------------------------------------------
sim_data <- sapply(
  names(BETA_SCENARIOS),
  \(s) run_scenario(BETA_SCENARIOS[[s]], scenario = s),
  simplify = FALSE
) |>
  bind_rows(.id = "scenario")

true_effects <- tibble(
  scenario = names(IE_TRUE),
  DE = DELTA_TRUE,
  IE = unname(IE_TRUE),
  TE = DE + IE
)

true_measures <- compute_measures(de = true_effects$DE, ie = true_effects$IE) |>
  mutate(scenario = true_effects$scenario, .before = 1)

true_effects
true_measures


# Summary statistics ------------------------------------------------------
summary_table <- sim_data |>
  pivot_longer(
    cols = c(PM, MMS, sigma),
    names_to = "measure",
    values_to = "value"
  ) |>
  group_by(scenario, measure) |>
  summarise(
    min = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    scenario = factor(scenario, levels = SCENARIO_ORDER),
    measure = factor(measure, levels = names(MEASURE_ROW_LABELS))
  ) |>
  arrange(scenario, measure) |>
  mutate(measure = MEASURE_ROW_LABELS[as.character(measure)])

saveRDS(
  object = summary_table,
  file = here::here("tables/two-pathway-summary-table.rds")
)


# Preparing LaTeX Table ---------------------------------------------------
panel_labels <- true_effects |>
  mutate(
    scenario = factor(scenario, levels = SCENARIO_ORDER),
    label = sprintf(
      "\\textbf{\\textit{%s}}:\\; $\\mathrm{IE} = %.2f,\\; \\mathrm{TE} = %.2f$",
      scenario,
      IE,
      TE
    )
  ) |>
  arrange(scenario) |>
  pull(label)

panel_index <- setNames(
  as.integer(table(summary_table$scenario)[SCENARIO_ORDER]),
  panel_labels
)

summary_table_fmt <- summary_table |>
  mutate(across(c(min, q1, median, q3, max), fmt_num))

tbl_caption <- paste0(
  "Simulated sampling distributions of the estimated proportion mediated ",
  "($\\widehat{\\mathrm{PM}}$), mediated magnitude share ",
  "($\\widehat{\\mathrm{MMS}}$), and degree of suppression ($\\hat{\\sigma}$) ",
  "for a single mediator and a continuous outcome under consistent mediation, ",
  "inconsistent mediation, and near-perfect suppression. ",
  "The true direct effect ($\\mathrm{DE} = ",
  sprintf("%.2f", DELTA_TRUE),
  "$) is the same in all scenarios. The scenarios differ in the true ",
  "indirect effect ($\\mathrm{IE}$) and, consequently, in the true total ",
  "effect ($\\mathrm{TE}$); these are given in the panel headers. ",
  "Each row summarizes ",
  format(N_REPS, big.mark = ","),
  " simulated ",
  "datasets of size ",
  format(N_OBS, big.mark = ","),
  " by the minimum, ",
  "first quartile (Q1), median, third quartile (Q3), and maximum of the ",
  "estimates."
)
last_rows <- head(cumsum(panel_index), -1)

tbl_latex <- summary_table_fmt |>
  select(-scenario) |>
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = "",
    align = "lrrrrr",
    col.names = c("", "Min", "Q1", "Median", "Q3", "Max"),
    caption = tbl_caption,
    label = "tbl-two-pathway"
  ) |>
  row_spec(last_rows, extra_latex_after = "\\midrule") |>
  pack_rows(
    index = panel_index,
    bold = FALSE,
    italic = FALSE,
    escape = FALSE,
    latex_align = 'l',
    extra_latex_after = "\\addlinespace[3pt]"
  ) |>
  kable_styling(
    latex_options = "hold_position",
    font_size = TABLE_FONT_SIZE
  ) |>
  polish_latex_table(midrule_gap = "0.6em", tabcolsep = "8pt")

saveRDS(
  tbl_latex,
  file = here::here("tables/two-pathway-latex-table.rds")
)
