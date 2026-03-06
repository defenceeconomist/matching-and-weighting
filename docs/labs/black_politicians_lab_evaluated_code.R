# Black Politicians training aid: evaluated code with processed outputs.
# Run with:
#   Rscript docs/labs/black_politicians_lab_evaluated_code.R

options(width = 120)

# ---- Step 1: Install and load required packages ----
required_packages <- c(
  "causaldata",
  "MatchIt",
  "WeightIt",
  "cobalt",
  "dplyr",
  "ggplot2"
)

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---- Step 2: Build analysis dataset used by all methods ----
dat <- causaldata::black_politicians |>
  dplyr::mutate(
    treat = as.integer(leg_black),
    outcome = as.integer(responded)
  )

design_covariates <- c("medianhhincom", "blackpercent", "leg_democrat")

dat_preview <- dat |>
  dplyr::select(treat, outcome, treat_out, dplyr::all_of(design_covariates)) |>
  head(10)

# ---- Step 3: Raw comparison summary ----
raw_summary <- dat |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    n = dplyr::n(),
    response_rate = mean(outcome, na.rm = TRUE),
    mean_income = mean(medianhhincom, na.rm = TRUE),
    mean_blackpercent = mean(blackpercent, na.rm = TRUE),
    prop_democrat = mean(leg_democrat == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Black legislators", "Non-Black legislators")) |>
  dplyr::select(group, n, response_rate, mean_income, mean_blackpercent, prop_democrat)

# ---- Step 4: Baseline balance benchmark ----
m_raw <- MatchIt::matchit(
  treat ~ medianhhincom + blackpercent + leg_democrat,
  data = dat,
  method = NULL,
  estimand = "ATT"
)

raw_balance <- cobalt::bal.tab(m_raw, un = TRUE, m.threshold = 0.1)

# ---- Step 5: Exact matching ----
m_exact <- MatchIt::matchit(
  treat ~ leg_democrat + leg_senator + south,
  data = dat,
  method = "exact",
  estimand = "ATT"
)

exact_dat <- MatchIt::match.data(m_exact)
exact_counts <- exact_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))

# ---- Step 6: CEM ----
create_even_breaks <- function(x, n) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  min_x + ((0:n) / n) * (max_x - min_x)
}

cem_cutpoints <- list(
  medianhhincom = quantile(dat$medianhhincom, probs = (0:6) / 6, na.rm = TRUE),
  blackpercent = create_even_breaks(dat$blackpercent, 6)
)

m_cem <- MatchIt::matchit(
  treat ~ medianhhincom + blackpercent + leg_democrat + leg_senator + south,
  data = dat,
  method = "cem",
  estimand = "ATT",
  cutpoints = cem_cutpoints
)

cem_dat <- MatchIt::match.data(m_cem)
cem_counts <- cem_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))

cem_balance <- cobalt::bal.tab(m_cem, un = TRUE, m.threshold = 0.1)

# ---- Step 7: Entropy balancing and ESS diagnostics ----
w_ebal <- WeightIt::weightit(
  treat ~ medianhhincom + I(medianhhincom^2) +
    blackpercent + I(blackpercent^2) +
    leg_democrat + leg_senator + south,
  data = dat,
  method = "ebal",
  estimand = "ATT"
)

ebal_balance <- cobalt::bal.tab(w_ebal, un = TRUE, m.threshold = 0.1)

ess <- function(w) {
  (sum(w)^2) / sum(w^2)
}

dat_ebal <- dat |>
  dplyr::mutate(w = w_ebal$weights)

ebal_weight_summary <- dat_ebal |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    raw_n = dplyr::n(),
    ess = ess(w),
    max_weight = max(w),
    mean_weight = mean(w),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control")) |>
  dplyr::select(group, raw_n, ess, max_weight, mean_weight)

# ---- Step 8: Outcome models across designs ----
raw_fit <- stats::lm(outcome ~ treat, data = dat)
exact_fit <- stats::lm(outcome ~ treat, data = exact_dat, weights = weights)
cem_fit <- stats::lm(outcome ~ treat, data = cem_dat, weights = weights)
ebal_fit <- stats::lm(outcome ~ treat, data = dat_ebal, weights = w)

get_treat_row <- function(fit, design_name) {
  out <- coef(summary(fit))
  data.frame(
    design = design_name,
    estimate = unname(out["treat", "Estimate"]),
    std_error = unname(out["treat", "Std. Error"]),
    t_value = unname(out["treat", "t value"]),
    p_value = unname(out["treat", "Pr(>|t|)"]),
    row.names = NULL,
    check.names = FALSE
  )
}

estimate_comparison <- dplyr::bind_rows(
  get_treat_row(raw_fit, "Raw sample"),
  get_treat_row(exact_fit, "Exact matching"),
  get_treat_row(cem_fit, "CEM"),
  get_treat_row(ebal_fit, "Entropy balancing")
)

# ---- Step 8b: Book-style weighted interaction model using CEM weights ----
# This mirrors the chapter's richer specification after the design stage.
final_fit <- stats::lm(
  outcome ~ treat * treat_out +
    nonblacknonwhite +
    black_medianhh +
    white_medianhh +
    statessquireindex +
    totalpop +
    urbanpercent,
  data = cem_dat,
  weights = weights
)

final_fit_coef <- coef(summary(final_fit))
final_fit_key_terms <- data.frame(
  term = rownames(final_fit_coef),
  estimate = unname(final_fit_coef[, "Estimate"]),
  std_error = unname(final_fit_coef[, "Std. Error"]),
  t_value = unname(final_fit_coef[, "t value"]),
  p_value = unname(final_fit_coef[, "Pr(>|t|)"]),
  row.names = NULL,
  check.names = FALSE
)

final_fit_key_terms <- final_fit_key_terms[
  final_fit_key_terms$term %in% c("treat", "treat_out", "treat:treat_out"),
]

# ---- Step 9: Print processed data objects for non-interactive runs ----
message("\n[Processed data preview]")
print(dat_preview)

message("\n[Raw summary]")
print(raw_summary)

message("\n[Exact matched counts]")
print(exact_counts)

message("\n[CEM matched counts]")
print(cem_counts)

message("\n[Entropy balancing weight summary]")
print(ebal_weight_summary)

message("\n[Effect estimate comparison]")
print(estimate_comparison)

message("\n[Book-replication interaction model key terms]")
print(final_fit_key_terms)

message("\nDone: evaluated workflow objects created.")
