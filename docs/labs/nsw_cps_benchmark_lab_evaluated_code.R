# NSW + CPS benchmark lab: evaluated code with processed outputs.
# Run with:
#   Rscript docs/labs/nsw_cps_benchmark_lab_evaluated_code.R

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

# ---- Step 2: Build benchmark and observational datasets ----
nsw <- causaldata::nsw_mixtape |>
  dplyr::mutate(
    treat = as.integer(treat),
    outcome = re78
  )

cps <- causaldata::cps_mixtape |>
  dplyr::mutate(
    treat = as.integer(treat),
    outcome = re78
  )

benchmark_dat <- nsw

obs_dat <- dplyr::bind_rows(
  nsw |>
    dplyr::filter(treat == 1L) |>
    dplyr::mutate(source = "NSW treated"),
  cps |>
    dplyr::filter(treat == 0L) |>
    dplyr::mutate(source = "CPS controls")
)

design_covariates <- c("age", "educ", "black", "hisp", "marr", "nodegree", "re74", "re75")

dat_preview <- obs_dat |>
  dplyr::select(source, treat, outcome, dplyr::all_of(design_covariates)) |>
  head(10)

# ---- Step 3: Experimental benchmark ----
benchmark_fit <- stats::lm(outcome ~ treat, data = benchmark_dat)
benchmark_att <- coef(summary(benchmark_fit))["treat", ]
experimental_estimate <- unname(benchmark_att["Estimate"])

benchmark_summary <- benchmark_dat |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_re78 = mean(outcome, na.rm = TRUE),
    mean_re74 = mean(re74, na.rm = TRUE),
    mean_re75 = mean(re75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "NSW treated", "NSW controls")) |>
  dplyr::select(group, n, mean_re78, mean_re74, mean_re75)

benchmark_effect <- data.frame(
  design = "Experimental benchmark (NSW RCT)",
  estimate = unname(benchmark_att["Estimate"]),
  std_error = unname(benchmark_att["Std. Error"]),
  t_value = unname(benchmark_att["t value"]),
  p_value = unname(benchmark_att["Pr(>|t|)"]),
  row.names = NULL,
  check.names = FALSE
)

# ---- Step 4: Raw observational comparison ----
raw_summary <- obs_dat |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_re78 = mean(outcome, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    mean_educ = mean(educ, na.rm = TRUE),
    prop_black = mean(black == 1, na.rm = TRUE),
    mean_re74 = mean(re74, na.rm = TRUE),
    mean_re75 = mean(re75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "NSW treated", "CPS controls")) |>
  dplyr::select(group, n, mean_re78, mean_age, mean_educ, prop_black, mean_re74, mean_re75)

raw_fit <- stats::lm(outcome ~ treat, data = obs_dat)

# ---- Step 5: Baseline balance and overlap diagnostics ----
m_raw <- MatchIt::matchit(
  treat ~ age + educ + black + hisp + marr + nodegree + re74 + re75,
  data = obs_dat,
  method = NULL,
  estimand = "ATT"
)

raw_balance <- cobalt::bal.tab(m_raw, un = TRUE, binary = "std", m.threshold = 0.1)

ps_fit <- stats::glm(
  treat ~ age + educ + black + hisp + marr + nodegree + re74 + re75,
  data = obs_dat,
  family = stats::binomial()
)

obs_dat <- obs_dat |>
  dplyr::mutate(ps = predict(ps_fit, type = "response"))

overlap_bounds <- obs_dat |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    min_ps = min(ps, na.rm = TRUE),
    max_ps = max(ps, na.rm = TRUE),
    .groups = "drop"
  )

common_support_low <- max(overlap_bounds$min_ps)
common_support_high <- min(overlap_bounds$max_ps)

overlap_summary <- obs_dat |>
  dplyr::group_by(treat) |>
  dplyr::summarise(
    n = dplyr::n(),
    min_ps = min(ps, na.rm = TRUE),
    p10_ps = as.numeric(stats::quantile(ps, 0.10, na.rm = TRUE)),
    p50_ps = as.numeric(stats::quantile(ps, 0.50, na.rm = TRUE)),
    p90_ps = as.numeric(stats::quantile(ps, 0.90, na.rm = TRUE)),
    max_ps = max(ps, na.rm = TRUE),
    in_common_support = mean(ps >= common_support_low & ps <= common_support_high),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "NSW treated", "CPS controls")) |>
  dplyr::select(group, n, min_ps, p10_ps, p50_ps, p90_ps, max_ps, in_common_support)

# ---- Step 6: Exact matching ----
m_exact <- MatchIt::matchit(
  treat ~ black + hisp + marr + nodegree + educ,
  data = obs_dat,
  method = "exact",
  estimand = "ATT"
)

exact_dat <- MatchIt::match.data(m_exact)
exact_counts <- exact_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))

exact_balance <- cobalt::bal.tab(
  m_exact,
  data = obs_dat,
  un = TRUE,
  binary = "std",
  m.threshold = 0.1,
  addl = ~ age + re74 + re75
)

# ---- Step 7: CEM ----
cem_cutpoints <- list(
  age = "q5",
  educ = 4,
  re74 = "q6",
  re75 = "q6"
)

m_cem <- MatchIt::matchit(
  treat ~ age + educ + black + hisp + marr + nodegree + re74 + re75,
  data = obs_dat,
  method = "cem",
  estimand = "ATT",
  cutpoints = cem_cutpoints
)

cem_dat <- MatchIt::match.data(m_cem)
cem_counts <- cem_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))

cem_balance <- cobalt::bal.tab(m_cem, un = TRUE, binary = "std", m.threshold = 0.1)

# ---- Step 8: Entropy balancing ----
w_ebal <- WeightIt::weightit(
  treat ~ age + I(age^2) +
    educ + I(educ^2) +
    black + hisp + marr + nodegree +
    re74 + I(re74^2) +
    re75 + I(re75^2),
  data = obs_dat,
  method = "ebal",
  estimand = "ATT"
)

ebal_balance <- cobalt::bal.tab(w_ebal, un = TRUE, binary = "std", m.threshold = 0.1)

ess <- function(w) {
  (sum(w)^2) / sum(w^2)
}

obs_ebal <- obs_dat |>
  dplyr::mutate(w = w_ebal$weights)

ebal_weight_summary <- obs_ebal |>
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

# ---- Step 9: Effect estimates and benchmark gaps ----
exact_fit <- stats::lm(outcome ~ treat, data = exact_dat, weights = weights)
cem_fit <- stats::lm(outcome ~ treat, data = cem_dat, weights = weights)
ebal_fit <- stats::lm(outcome ~ treat, data = obs_ebal, weights = w)

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
  benchmark_effect,
  get_treat_row(raw_fit, "Raw observational (NSW treated vs CPS)"),
  get_treat_row(exact_fit, "Exact matching"),
  get_treat_row(cem_fit, "CEM"),
  get_treat_row(ebal_fit, "Entropy balancing")
) |>
  dplyr::mutate(
    benchmark_gap = estimate - experimental_estimate,
    abs_benchmark_gap = abs(benchmark_gap)
  )

method_retention <- dplyr::bind_rows(
  data.frame(design = "Raw observational", treated_retained = sum(obs_dat$treat == 1L), control_retained = sum(obs_dat$treat == 0L)),
  data.frame(design = "Exact matching", treated_retained = sum(exact_dat$treat == 1L), control_retained = sum(exact_dat$treat == 0L)),
  data.frame(design = "CEM", treated_retained = sum(cem_dat$treat == 1L), control_retained = sum(cem_dat$treat == 0L)),
  data.frame(design = "Entropy balancing", treated_retained = sum(obs_ebal$treat == 1L), control_retained = sum(obs_ebal$treat == 0L))
)

benchmark_gap_rank <- estimate_comparison |>
  dplyr::filter(design != "Experimental benchmark (NSW RCT)") |>
  dplyr::arrange(abs_benchmark_gap) |>
  dplyr::select(design, estimate, benchmark_gap, abs_benchmark_gap)

# ---- Step 10: Print processed objects for non-interactive runs ----
message("\n[Processed data preview]")
print(dat_preview)

message("\n[Experimental benchmark summary]")
print(benchmark_summary)

message("\n[Raw observational summary]")
print(raw_summary)

message("\n[Overlap summary]")
print(overlap_summary)

message("\n[Exact matched counts]")
print(exact_counts)

message("\n[CEM matched counts]")
print(cem_counts)

message("\n[Entropy balancing weight summary]")
print(ebal_weight_summary)

message("\n[Method retention]")
print(method_retention)

message("\n[Estimate comparison]")
print(estimate_comparison)

message("\n[Benchmark gap ranking]")
print(benchmark_gap_rank)

message("\nDone: evaluated NSW + CPS benchmark workflow objects created.")
