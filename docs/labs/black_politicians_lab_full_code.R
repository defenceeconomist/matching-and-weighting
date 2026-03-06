# Black Politicians training aid: executable full analysis script.
# Run with:
#   Rscript docs/labs/black_politicians_lab_full_code.R

options(width = 120)

# ---- Step 1: Install and load required packages ----
# Install only packages that are missing so repeated runs stay fast and reproducible.
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

# ---- Step 2: Load data and map variables to design roles ----
# We define a binary treatment and binary outcome used consistently in every method.
dat <- causaldata::black_politicians |>
  dplyr::mutate(
    treat = as.integer(leg_black),
    outcome = as.integer(responded)
  )

design_covariates <- c("medianhhincom", "blackpercent", "leg_democrat")

message("\n[Step 2] Data schema check")
dat |>
  dplyr::select(treat, outcome, treat_out, dplyr::all_of(design_covariates)) |>
  dplyr::glimpse()

# ---- Step 3: Describe the raw comparison before any adjustment ----
message("\n[Step 3] Raw treated vs control comparison")
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

print(raw_summary)

# ---- Step 4: Baseline balance diagnostics (no adjustment) ----
# method = NULL builds a diagnostic object for the unadjusted sample.
message("\n[Step 4] Baseline balance diagnostics")
m_raw <- MatchIt::matchit(
  treat ~ medianhhincom + blackpercent + leg_democrat,
  data = dat,
  method = NULL,
  estimand = "ATT"
)

print(summary(m_raw, un = TRUE))
print(cobalt::bal.tab(m_raw, un = TRUE, m.threshold = 0.1))
print(cobalt::love.plot(
  m_raw,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

# ---- Step 5: Exact matching feasibility check ----
# Exact matching is transparent, but often drops many units.
message("\n[Step 5] Exact matching")
m_exact <- MatchIt::matchit(
  treat ~ leg_democrat + leg_senator + south,
  data = dat,
  method = "exact",
  estimand = "ATT"
)

print(summary(m_exact, un = TRUE))

exact_dat <- MatchIt::match.data(m_exact)
exact_counts <- exact_dat |>
  dplyr::count(treat) |>
  dplyr::mutate(group = dplyr::if_else(treat == 1L, "Treated", "Control"))
print(exact_counts)

# ---- Step 6: Coarsened exact matching (CEM) ----
# We coarsen continuous covariates into bins to relax strict exact matching.
message("\n[Step 6] Coarsened exact matching (CEM)")
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

print(summary(m_cem, un = TRUE))
print(cobalt::bal.tab(m_cem, un = TRUE, m.threshold = 0.1))
print(cobalt::love.plot(
  m_cem,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

# ---- Step 7: Entropy balancing and weight diagnostics ----
# Entropy balancing keeps treated units and reweights controls to satisfy balance constraints.
message("\n[Step 7] Entropy balancing")
w_ebal <- WeightIt::weightit(
  treat ~ medianhhincom + I(medianhhincom^2) +
    blackpercent + I(blackpercent^2) +
    leg_democrat + leg_senator + south,
  data = dat,
  method = "ebal",
  estimand = "ATT"
)

print(summary(w_ebal))
print(cobalt::bal.tab(w_ebal, un = TRUE, m.threshold = 0.1))
print(cobalt::love.plot(
  w_ebal,
  abs = TRUE,
  thresholds = c(m = 0.1),
  stars = "raw"
))

# Effective sample size helps quantify how concentrated the weights become.
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
  )
print(ebal_weight_summary)

# ---- Step 8: Compare treatment effect estimates across designs ----
# Keep the outcome model simple so the design tradeoffs stay easy to interpret.
message("\n[Step 8] Outcome model comparison")
raw_fit <- stats::lm(outcome ~ treat, data = dat)
exact_fit <- stats::lm(outcome ~ treat, data = exact_dat, weights = weights)

cem_dat <- MatchIt::match.data(m_cem)
cem_fit <- stats::lm(outcome ~ treat, data = cem_dat, weights = weights)

ebal_fit <- stats::lm(outcome ~ treat, data = dat_ebal, weights = w)

print(coef(summary(raw_fit)))
print(coef(summary(exact_fit)))
print(coef(summary(cem_fit)))
print(coef(summary(ebal_fit)))

# Optional extension: richer model closer to the original study setup.
message("\n[Step 8 extension] Interaction model with treat_out")
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

print(summary(final_fit))

message("\nDone: full Black Politicians training-aid workflow executed.")
