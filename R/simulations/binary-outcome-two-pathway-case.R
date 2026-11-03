# ==============================================================================
# Simulation of PM, MMS, and the Degree of Suppression for
# the binary-outcome case
# ==============================================================================
#
# Data-generating process (true model; effects on the log-odds scale)
#   X ~ N(0, 1)
#   e1 ~ N(0, SD_M^2)
#   M = gamma * X + e1
#   Y ~ Bernoulli(p),  logit(p) = i + delta * X + beta * M
#   The intercept i is calibrated in each scenario so that P(Y = 1) = PREVALENCE.
#
# True effects (log-odds scale, per one-unit increase in X)
#   ln DE = delta
#   ln IE = gamma * beta
#   ln TE = ln DE + ln IE
#   L     = |ln DE| + |ln IE|
#
# Measures (all on the log-odds scale, where the effects are additive)
#   PM    = ln IE / ln TE      (proportion mediated; undefined when ln TE = 0)
#   MMS   = |ln IE| / L        (mediated magnitude share; undefined when L = 0)
#   sigma = 1 - |ln TE| / L    (Degree of Suppression; undefined when L = 0)
#
# Scenarios (DE and gamma are the same in all three; only beta, and so IE,
# differs)
#   Consistent mediation:     beta > 0, so IE has the same sign as DE
#   Inconsistent mediation:   beta < 0, so IE opposes DE; |IE| < |DE|, so TE is
#                             clearly away from zero
#   Near-perfect suppression: beta < 0 with IE almost equal to -DE, so TE is
#                             close to zero but not exactly zero
#
# Simulation
#   For each scenario, N_REPS datasets of size N_OBS are generated from the true
#   model. In each dataset, M ~ X is fitted by OLS and Y ~ X + M by logistic
#   regression, and the estimated effects (delta_hat, gamma_hat * beta_hat; the
#   product-of-coefficients method) are used to compute the three measures.
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
N_OBS <- 5000 # observations per dataset
REPORT_EVERY <- 1000 # progress message interval (datasets)

# True parameters (log-odds scale for the outcome model)
GAMMA_TRUE <- 0.50
BETA_TRUE <- 0.40
DELTA_TRUE <- 0.40
SD_M <- 1
PREVALENCE <- 0.10 # marginal P(Y = 1) in each scenario

# Beta that leaves a small positive TE: IE = 0.5 * -0.76 = -0.38, TE = 0.02
BETA_NEAR_PERFECT <- -0.76

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
TABLE_FONT_SIZE <- 10


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

# Intercept of the outcome model that gives a marginal P(Y = 1) equal to
# `prevalence`. The linear predictor is normal with mean `intercept` and
# variance (delta + beta * gamma)^2 + (beta * sd_m)^2, so the marginal
# prevalence is a one-dimensional integral that is solved for the intercept.
calibrate_intercept <- function(
  beta,
  prevalence = PREVALENCE,
  gamma = GAMMA_TRUE,
  delta = DELTA_TRUE,
  sd_m = SD_M
) {
  sd_linear_predictor <- sqrt((delta + beta * gamma)^2 + (beta * sd_m)^2)

  marginal_prevalence <- function(intercept) {
    integrate(
      \(z) plogis(intercept + sd_linear_predictor * z) * dnorm(z),
      lower = -Inf,
      upper = Inf
    )$value
  }

  uniroot(\(a) marginal_prevalence(a) - prevalence, interval = c(-10, 5))$root
}

INTERCEPT_SCENARIOS <- vapply(BETA_SCENARIOS, calibrate_intercept, numeric(1))

simulate_dataset <- function(
  beta,
  intercept,
  n_obs = N_OBS,
  gamma = GAMMA_TRUE,
  delta = DELTA_TRUE,
  sd_m = SD_M
) {
  tibble(
    X = rnorm(n_obs),
    M = gamma * X + rnorm(n_obs, sd = sd_m),
    Y = rbinom(n_obs, size = 1, prob = plogis(intercept + delta * X + beta * M))
  )
}

estimate_measures <- function(data) {
  gamma_hat <- coef(lm(M ~ X, data = data))[["X"]]
  y_coefs <- coef(glm(Y ~ X + M, family = binomial(), data = data))

  compute_measures(
    de = y_coefs[["X"]],
    ie = gamma_hat * y_coefs[["M"]]
  )
}

run_scenario <- function(
  beta,
  intercept,
  scenario,
  n_reps = N_REPS,
  report_every = REPORT_EVERY
) {
  lapply(
    seq_len(n_reps),
    \(i) {
      result <- estimate_measures(simulate_dataset(beta, intercept))
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
  \(s) {
    run_scenario(
      beta = BETA_SCENARIOS[[s]],
      intercept = INTERCEPT_SCENARIOS[[s]],
      scenario = s
    )
  },
  simplify = FALSE
) |>
  bind_rows(.id = "scenario")

# True effects (log-odds scale) and measures for each scenario
true_effects <- tibble(
  scenario = names(IE_TRUE),
  DE = DELTA_TRUE,
  IE = unname(IE_TRUE),
  TE = DE + IE
)

true_measures <- compute_measures(de = true_effects$DE, ie = true_effects$IE) |>
  mutate(scenario = true_effects$scenario, .before = 1)


# Checks ------------------------------------------------------------------
true_effects
true_measures

# Calibrated intercepts and realised prevalence in each scenario
tibble(
  scenario = names(INTERCEPT_SCENARIOS),
  intercept = INTERCEPT_SCENARIOS,
  prevalence = vapply(
    names(BETA_SCENARIOS),
    \(s) {
      mean(
        simulate_dataset(
          beta = BETA_SCENARIOS[[s]],
          intercept = INTERCEPT_SCENARIOS[[s]],
          n_obs = 1e6
        )$Y
      )
    },
    numeric(1)
  )
)

# True effects on the odds-ratio scale
tibble(
  scenario = names(IE_TRUE),
  DE_OR = exp(DELTA_TRUE),
  IE_OR = exp(IE_TRUE),
  TE_OR = exp(DELTA_TRUE + IE_TRUE)
)


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
  file = here::here("tables/binary-outcome-summary-table.rds")
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
  "for a binary outcome under consistent mediation, inconsistent mediation, ",
  "and near-perfect suppression. ",
  "Effects and measures are computed on the log-odds scale, and the outcome ",
  "prevalence is ",
  sprintf("%d\\%%", round(PREVALENCE * 100)),
  ". ",
  "The true direct effect ($\\mathrm{DE} = ",
  sprintf("%.2f", DELTA_TRUE),
  "$) is the same in all scenarios. The scenarios differ in the true ",
  "indirect effect ($\\mathrm{IE}$) and, consequently, in the true total ",
  "effect ($\\mathrm{TE}$); these are given in the panel headers. ",
  "Each row summarises ",
  format(N_REPS, big.mark = ","),
  " simulated ",
  "datasets of size ",
  format(N_OBS, big.mark = ","),
  " by the minimum, ",
  "first quartile (Q1), median (Q2), third quartile (Q3), and maximum of the ",
  "estimates."
)

last_rows <- head(cumsum(panel_index), -1)

tbl_latex <- summary_table_fmt |>
  select(-scenario) |>
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = "\\addlinespace[4pt]",
    align = "lrrrrr",
    col.names = c("", "Min", "Q1", "Q2", "Q3", "Max"),
    caption = tbl_caption,
    label = "tbl-binary-out"
  ) |>
  row_spec(last_rows, extra_latex_after = "\\midrule") |>
  pack_rows(
    index = panel_index,
    bold = FALSE,
    italic = FALSE,
    escape = FALSE,
    latex_align = "l",
    extra_latex_after = "\\addlinespace[4pt]"
  ) |>
  kable_styling(
    latex_options = "hold_position",
    font_size = TABLE_FONT_SIZE
  ) |>
  polish_latex_table(midrule_gap = "0.8em", tabcolsep = "12pt")

saveRDS(
  tbl_latex,
  file = here::here("tables/binary-outcome-latex-table.rds")
)
