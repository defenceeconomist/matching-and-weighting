# NSW + CPS benchmark lab: executable full analysis script.
# Run with:
#   Rscript docs/labs/nsw_cps_benchmark_lab_full_code.R

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

# ---- Step 2: Load data and define benchmark vs observational samples ----
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

message("\n[Step 2] Observational data schema check")
obs_dat |>
  dplyr::select(source, treat, outcome, dplyr::all_of(design_covariates)) |>
  dplyr::glimpse()

# ---- Step 3: Experimental benchmark from NSW treated vs NSW controls ----
message("\n[Step 3] Experimental benchmark")
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
print(benchmark_summary)

benchmark_fit <- stats::lm(outcome ~ treat, data = benchmark_dat)
benchmark_att <- coef(summary(benchmark_fit))["treat", ]
print(benchmark_att)

# ---- Step 4: Raw observational comparison (NSW treated vs CPS controls) ----
message("\n[Step 4] Raw observational comparison")
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
print(raw_summary)

raw_fit <- stats::lm(outcome ~ treat, data = obs_dat)
raw_att <- coef(summary(raw_fit))["treat", ]
print(raw_att)

# ---- Step 5: Baseline balance and overlap diagnostics ----
message("\n[Step 5] Baseline balance diagnostics")
m_raw <- MatchIt::matchit(
  treat ~ age + educ + black + hisp + marr + nodegree + re74 + re75,
  data = obs_dat,
  method = NULL,
  estimand = "ATT"
)

print(summary(m_raw, un = TRUE))
print(cobalt::bal.tab(m_raw, un = TRUE, binary = "std", m.threshold = 0.1))
print(cobalt::love.plot(
  m_raw,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

message("\n[Step 5] Propensity-score overlap summary")
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
print(overlap_summary)

# ---- Step 6: Exact matching ----
message("\n[Step 6] Exact matching")
m_exact <- MatchIt::matchit(
  treat ~ black + hisp + marr + nodegree + educ,
  data = obs_dat,
  method = "exact",
  estimand = "ATT"
)

print(summary(m_exact, un = TRUE))

exact_dat <- MatchIt::match.data(m_exact)
exact_counts <- exact_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))
print(exact_counts)

print(cobalt::bal.tab(
  m_exact,
  data = obs_dat,
  un = TRUE,
  binary = "std",
  m.threshold = 0.1,
  addl = ~ age + re74 + re75
))

# ---- Step 7: Coarsened exact matching (CEM) ----
message("\n[Step 7] Coarsened exact matching (CEM)")
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

print(summary(m_cem, un = TRUE))
print(cobalt::bal.tab(m_cem, un = TRUE, binary = "std", m.threshold = 0.1))
print(cobalt::love.plot(
  m_cem,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

cem_dat <- MatchIt::match.data(m_cem)
cem_counts <- cem_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))
print(cem_counts)

# ---- Step 8: Entropy balancing and ESS diagnostics ----
message("\n[Step 8] Entropy balancing")
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

print(summary(w_ebal))
print(cobalt::bal.tab(w_ebal, un = TRUE, binary = "std", m.threshold = 0.1))
print(cobalt::love.plot(
  w_ebal,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

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
print(ebal_weight_summary)

# ---- Step 9: Compare all estimates against the experimental benchmark ----
message("\n[Step 9] Estimate comparison against benchmark")
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

experimental_estimate <- unname(benchmark_att["Estimate"])

estimate_comparison <- dplyr::bind_rows(
  get_treat_row(benchmark_fit, "Experimental benchmark (NSW RCT)"),
  get_treat_row(raw_fit, "Raw observational (NSW treated vs CPS)"),
  get_treat_row(exact_fit, "Exact matching"),
  get_treat_row(cem_fit, "CEM"),
  get_treat_row(ebal_fit, "Entropy balancing")
) |>
  dplyr::mutate(
    benchmark_gap = estimate - experimental_estimate,
    abs_benchmark_gap = abs(benchmark_gap)
  )

print(estimate_comparison)

message("\nDone: full NSW + CPS benchmark workflow executed.")
