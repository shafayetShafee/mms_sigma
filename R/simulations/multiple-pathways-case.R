# ==============================================================================
# Simulation of PM, MMS, and the Degree of Suppression for
# the multiple-mediator case
# ==============================================================================
#
# Data-generating process (true model, K parallel mediators)
#   X   ~ N(0, 1)
#   e_k ~ N(0, SD_M^2)
#   M_k = gamma_k * X + e_k,  k = 1, ..., K
#   e_Y ~ N(0, SD_Y^2)
#   Y   = delta * X + beta_1 * M_1 + ... + beta_K * M_K + e_Y
#
#   Mediators are parallel: no paths between them and independent errors.
#
# True effects
#   DE   = delta
#   IE_k = gamma_k * beta_k
#   TE   = DE + sum_k IE_k
#   L    = |DE| + sum_k |IE_k|
#
# Measures
#   PM_k  = IE_k / TE      (proportion mediated through mediator k;
#                           undefined when TE = 0)
#   MMS_k = |IE_k| / L     (mediated magnitude share of mediator k;
#                           undefined when L = 0)
#   sigma = 1 - |TE| / L   (Degree of Suppression; undefined when L = 0)
#
# Scenarios (DE, gamma_k and |beta_k| are the same in both, so the true MMS_k
# are identical; only the signs of beta_k differ)
#   Consistent mediation:   every IE_k has the same sign as DE
#   Inconsistent mediation: mediators 1 and 3 oppose DE and mediator 2 agrees
#                           with it, so the effects partly cancel (TE is small
#                           relative to L, but not close to zero)
#
# Simulation
#   For each scenario, N_REPS datasets of size N_OBS are generated from the true
#   model. In each dataset, M_k ~ X and Y ~ X + M_1 + ... + M_K are fitted by
#   OLS, and the estimated effects (DE_hat = delta_hat,
#   IE_hat_k = gamma_hat_k * beta_hat_k) are used to compute the measures.
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

# True parameters, one entry per mediator
GAMMA_TRUE <- c(0.50, 0.40, 0.30)
BETA_TRUE <- c(0.40, 0.30, 0.20) # magnitudes; signs are set per scenario
DELTA_TRUE <- 0.30
SD_M <- 1
SD_Y <- 1

N_MEDIATORS <- length(GAMMA_TRUE)
MEDIATOR_VARS <- paste0("M", seq_len(N_MEDIATORS))
MEDIATOR_LABELS <- paste("Mediator", seq_len(N_MEDIATORS))

BETA_SCENARIOS <- list(
  `Consistent mediation` = BETA_TRUE,
  `Inconsistent mediation` = BETA_TRUE * c(-1, 1, -1)
)

IE_TRUE <- lapply(BETA_SCENARIOS, \(beta) GAMMA_TRUE * beta)

ZERO_TOL <- 1e-10

# Table configs
SCENARIO_ORDER <- names(BETA_SCENARIOS)
SIGMA_LABEL <- "$\\hat{\\sigma}$"
SHARED_IE <- 2 # mediator whose IE is the same in every scenario
VARYING_IE <- setdiff(seq_len(N_MEDIATORS), SHARED_IE) # mediators 1 and 3
ROW_LABELS <- c(
  paste0("$\\widehat{\\mathrm{PM}}_", seq_len(N_MEDIATORS), "$"),
  paste0("$\\widehat{\\mathrm{MMS}}_", seq_len(N_MEDIATORS), "$"),
  SIGMA_LABEL
)
TABLE_DIGITS <- 2
TABLE_FONT_SIZE <- 10


# Simulation Functions ----------------------------------------------------
compute_measures <- function(
  de,
  ie,
  med_label = MEDIATOR_LABELS,
  tol = ZERO_TOL
) {
  tibble(
    mediator = factor(med_label, levels = med_label),
    ie = ie
  ) |>
    mutate(
      te = de + sum(ie),
      l = abs(de) + sum(abs(ie)),
      PM = if_else(abs(te) > tol, ie / te, NA_real_),
      MMS = if_else(l > tol, abs(ie) / l, NA_real_),
      sigma = if_else(l > tol, 1 - abs(te) / l, NA_real_)
    ) |>
    select(mediator, PM, MMS, sigma)
}

simulate_dataset <- function(
  beta,
  n_obs = N_OBS,
  gamma = GAMMA_TRUE,
  delta = DELTA_TRUE,
  sd_m = SD_M,
  sd_y = SD_Y,
  med_var = MEDIATOR_VARS
) {
  x <- rnorm(n_obs)
  m <- outer(x, gamma) +
    matrix(rnorm(n_obs * length(gamma), sd = sd_m), nrow = n_obs)
  y <- delta * x + as.vector(m %*% beta) + rnorm(n_obs, sd = sd_y)
  colnames(m) <- med_var

  tibble(X = x, as_tibble(m), Y = y)
}

estimate_measures <- function(data, med_var = MEDIATOR_VARS) {
  gamma_hat <- coef(lm(as.matrix(data[med_var]) ~ X, data = data))["X", ]
  y_coefs <- coef(lm(reformulate(c("X", med_var), "Y"), data = data))

  compute_measures(
    de = y_coefs[["X"]],
    ie = unname(gamma_hat * y_coefs[med_var])
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
    bind_rows(.id = "replication")
}


# Running Simulations -----------------------------------------------------
sim_data <- sapply(
  names(BETA_SCENARIOS),
  \(s) run_scenario(BETA_SCENARIOS[[s]], scenario = s),
  simplify = FALSE
) |>
  bind_rows(.id = "scenario")

# True effects and measures for each scenario
true_effects <- lapply(
  IE_TRUE,
  \(ie) list(DE = DELTA_TRUE, IE = ie, TE = DELTA_TRUE + sum(ie))
)

true_measures <- bind_rows(
  lapply(IE_TRUE, \(ie) compute_measures(de = DELTA_TRUE, ie = ie)),
  .id = "scenario"
)

true_effects
true_measures


# Summary statistics ------------------------------------------------------

# One row per replication x mediator for PM and MMS; sigma describes the model
# as a whole, so it is taken once per replication
table_data <- bind_rows(
  sim_data |>
    select(scenario, mediator, PM, MMS) |>
    pivot_longer(
      cols = c(PM, MMS),
      names_to = "measure",
      values_to = "value"
    ) |>
    mutate(
      row_label = sprintf(
        "$\\widehat{\\mathrm{%s}}_%d$",
        measure,
        as.integer(mediator)
      )
    ),
  sim_data |>
    distinct(scenario, replication, sigma) |>
    transmute(scenario, row_label = SIGMA_LABEL, value = sigma)
) |>
  select(scenario, row_label, value)

summary_table <- table_data |>
  group_by(scenario, row_label) |>
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
    row_label = factor(row_label, levels = ROW_LABELS)
  ) |>
  arrange(scenario, row_label) |>
  mutate(row_label = as.character(row_label))

saveRDS(
  object = summary_table,
  file = here::here("tables/multiple-pathway-summary-table.rds")
)


# Preparing LaTeX Table ---------------------------------------------------
summary_table_fmt <- summary_table |>
  mutate(across(c(min, q1, median, q3, max), fmt_num))

panel_labels <- vapply(
  SCENARIO_ORDER,
  \(s) {
    eff <- true_effects[[s]]
    sprintf(
      "\\textbf{\\textit{%s}}:\\; $%s,\\; \\mathrm{TE} = %.2f$",
      s,
      paste0(
        "\\mathrm{IE}_",
        VARYING_IE,
        " = ",
        sprintf("%.2f", eff$IE[VARYING_IE]),
        collapse = ",\\; "
      ),
      eff$TE
    )
  },
  character(1),
  USE.NAMES = FALSE
)

panel_index <- setNames(
  as.integer(table(summary_table$scenario)[SCENARIO_ORDER]),
  panel_labels
)

tbl_caption <- paste0(
  "Simulated sampling distributions of the estimated proportion mediated ",
  "($\\widehat{\\mathrm{PM}}_k$) and mediated magnitude share ",
  "($\\widehat{\\mathrm{MMS}}_k$) for each of three parallel mediators ",
  "($k = 1, 2, 3$), and of the estimated degree of suppression ",
  "($\\hat{\\sigma}$), under consistent and inconsistent mediation. ",
  "The true direct effect (DE = ", sprintf("%.2f", DELTA_TRUE), ") and the ",
  "true indirect effect through the second mediator (IE$_{", SHARED_IE, "}$ = ",
  sprintf("%.2f", IE_TRUE[[1]][SHARED_IE]), ") are the same in both ",
  "scenarios. The scenarios differ in the true indirect effects through the ",
  "first and third mediators (IE$_1$ and IE$_3$) and, consequently, in the ",
  "true total effect (TE); these are given in the panel headers. ",
  "The degree of suppression describes the model as a whole and is reported ",
  "once per scenario. ",
  "Each row summarises ", format(N_REPS, big.mark = ","), " simulated ",
  "datasets of size ", N_OBS, " by the minimum, first quartile (Q1), ",
  "median (Q2), third quartile (Q3), and maximum of the estimates."
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
    label = "tbl-multi-pathway"
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
  polish_latex_table(midrule_gap = "0.8em", tabcolsep = "10pt")

saveRDS(
  tbl_latex,
  file = here::here("tables/multiple-pathway-latex-table.rds")
)
