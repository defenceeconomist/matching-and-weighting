required_packages <- c(
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
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

data("lalonde", package = "MatchIt")

dat <- lalonde |>
  mutate(
    treat = as.integer(treat),
    outcome = re78,
    married = factor(married, levels = c(0, 1), labels = c("not_married", "married")),
    nodegree = factor(nodegree, levels = c(0, 1), labels = c("has_degree", "no_degree"))
  )

design_covariates <- c(
  "age", "educ", "race", "married", "nodegree", "re74", "re75"
)

dat |>
  select(treat, outcome, all_of(design_covariates)) |>
  glimpse()

pre_match_summary <- dat |>
  group_by(treat) |>
  summarise(
    n = n(),
    mean_outcome = mean(outcome, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    mean_educ = mean(educ, na.rm = TRUE),
    prop_married = mean(married == "married", na.rm = TRUE),
    prop_no_degree = mean(nodegree == "no_degree", na.rm = TRUE),
    mean_re74 = mean(re74, na.rm = TRUE),
    mean_re75 = mean(re75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = if_else(treat == 1L, "Treated", "Control")) |>
  select(
    group, n, mean_outcome, mean_age, mean_educ,
    prop_married, prop_no_degree, mean_re74, mean_re75
  ) |>
  mutate(across(-c(group, n), ~ round(.x, 3)))

pre_match_summary

m_out0 <- matchit(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  data = dat,
  method = NULL,
  estimand = "ATT"
)

summary(m_out0, un = TRUE)

unadjusted_balance <- bal.tab(
  m_out0,
  un = TRUE,
  binary = "std",
  disp.v.ratio = TRUE,
  m.threshold = 0.1
)

unadjusted_balance

love.plot(
  m_out0,
  stats = "mean.diffs",
  abs = TRUE,
  binary = "std",
  thresholds = c(m = 0.1),
  var.order = "unadjusted"
)

m_exact <- matchit(
  treat ~ race + married + nodegree + educ,
  data = dat,
  method = "exact",
  estimand = "ATT"
)

matched_exact <- match.data(m_exact)

exact_retention <- tibble(
  metric = c(
    "Treated units in full sample",
    "Treated units retained",
    "Control units in full sample",
    "Control units retained",
    "Matched subclasses"
  ),
  value = c(
    sum(dat$treat == 1),
    sum(matched_exact$treat == 1),
    sum(dat$treat == 0),
    sum(matched_exact$treat == 0),
    dplyr::n_distinct(matched_exact$subclass)
  )
)

exact_retention

summary(m_exact, un = TRUE)

exact_balance <- bal.tab(
  m_exact,
  data = dat,
  un = TRUE,
  binary = "std",
  disp.v.ratio = TRUE,
  m.threshold = 0.1,
  addl = ~ age + re74 + re75
)

exact_balance

fit_exact <- lm(outcome ~ treat, data = matched_exact, weights = weights)

exact_effect <- {
  exact_coef <- coef(summary(fit_exact))["treat", ]
  tibble(
    method = "Exact matching",
    estimate = unname(exact_coef["Estimate"]),
    std_error = unname(exact_coef["Std. Error"]),
    p_value = unname(exact_coef["Pr(>|t|)"])
  )
} |>
  mutate(across(c(estimate, std_error, p_value), ~ round(.x, 3)))

exact_effect

m_cem_default <- matchit(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  data = dat,
  method = "cem",
  estimand = "ATT"
)

matched_cem_default <- match.data(m_cem_default)

cem_cutpoints <- list(
  age = "q5",
  educ = 4,
  re74 = "q4",
  re75 = "q4"
)

m_cem <- matchit(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  data = dat,
  method = "cem",
  estimand = "ATT",
  cutpoints = cem_cutpoints
)

matched_cem <- match.data(m_cem)

cem_default_balance <- bal.tab(
  m_cem_default,
  un = TRUE,
  binary = "std",
  disp.v.ratio = TRUE,
  m.threshold = 0.1
)

cem_balance <- bal.tab(
  m_cem,
  un = TRUE,
  binary = "std",
  disp.v.ratio = TRUE,
  m.threshold = 0.1
)

cem_comparison <- tibble(
  specification = c("Default CEM", "Documented custom cutpoints"),
  treated_retained = c(
    sum(matched_cem_default$treat == 1),
    sum(matched_cem$treat == 1)
  ),
  control_retained = c(
    sum(matched_cem_default$treat == 0),
    sum(matched_cem$treat == 0)
  ),
  matched_subclasses = c(
    dplyr::n_distinct(matched_cem_default$subclass),
    dplyr::n_distinct(matched_cem$subclass)
  ),
  max_abs_adjusted_smd = c(
    max(abs(cem_default_balance$Balance$Diff.Adj), na.rm = TRUE),
    max(abs(cem_balance$Balance$Diff.Adj), na.rm = TRUE)
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cem_comparison

fit_cem <- lm(outcome ~ treat, data = matched_cem, weights = weights)

cem_effect <- {
  cem_coef <- coef(summary(fit_cem))["treat", ]
  tibble(
    method = "Coarsened exact matching",
    estimate = unname(cem_coef["Estimate"]),
    std_error = unname(cem_coef["Std. Error"]),
    p_value = unname(cem_coef["Pr(>|t|)"])
  )
} |>
  mutate(across(c(estimate, std_error, p_value), ~ round(.x, 3)))

cem_effect

w_ebal_default <- weightit(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  data = dat,
  method = "ebal",
  estimand = "ATT"
)

w_ebal_moments <- weightit(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  data = dat,
  method = "ebal",
  estimand = "ATT",
  moments = c(age = 2, re74 = 2, re75 = 2)
)

ebal_default_summary <- summary(w_ebal_default)
ebal_moments_summary <- summary(w_ebal_moments)

ebal_default_tuning_balance <- bal.tab(
  w_ebal_default,
  un = TRUE,
  binary = "std",
  addl = ~ I(age^2) + I(re74^2) + I(re75^2)
)

ebal_moments_tuning_balance <- bal.tab(
  w_ebal_moments,
  un = TRUE,
  binary = "std",
  addl = ~ I(age^2) + I(re74^2) + I(re75^2)
)

second_moment_terms <- c("I(age^2)", "I(re74^2)", "I(re75^2)")

ebal_tuning_comparison <- tibble(
  specification = c(
    "Default means only",
    "Tuned squares for age and prior earnings"
  ),
  control_ess = c(
    ebal_default_summary$effective.sample.size["Weighted", "Control"],
    ebal_moments_summary$effective.sample.size["Weighted", "Control"]
  ),
  max_control_weight = c(
    max(w_ebal_default$weights[dat$treat == 0]),
    max(w_ebal_moments$weights[dat$treat == 0])
  ),
  max_abs_adj_smd_means = c(
    max(abs(ebal_default_tuning_balance$Balance[setdiff(rownames(ebal_default_tuning_balance$Balance), second_moment_terms), "Diff.Adj"]), na.rm = TRUE),
    max(abs(ebal_moments_tuning_balance$Balance[setdiff(rownames(ebal_moments_tuning_balance$Balance), second_moment_terms), "Diff.Adj"]), na.rm = TRUE)
  ),
  max_abs_adj_smd_selected_squares = c(
    max(abs(ebal_default_tuning_balance$Balance[second_moment_terms, "Diff.Adj"]), na.rm = TRUE),
    max(abs(ebal_moments_tuning_balance$Balance[second_moment_terms, "Diff.Adj"]), na.rm = TRUE)
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

ebal_tuning_comparison

w_ebal <- w_ebal_default
ebal_summary <- ebal_default_summary

dat_ebal <- dat |>
  mutate(ebal_weight = w_ebal$weights)

ebal_weight_diagnostics <- tibble(
  metric = c(
    "Treated units",
    "Control units",
    "Treated effective sample size",
    "Control effective sample size",
    "Maximum control weight",
    "Control coefficient of variation"
  ),
  value = c(
    sum(dat$treat == 1),
    sum(dat$treat == 0),
    ebal_summary$effective.sample.size["Weighted", "Treated"],
    ebal_summary$effective.sample.size["Weighted", "Control"],
    max(dat_ebal$ebal_weight[dat_ebal$treat == 0]),
    unname(ebal_summary$coef.of.var["control"])
  )
) |>
  mutate(value = round(value, 3))

ebal_weight_diagnostics

ebal_balance <- bal.tab(
  w_ebal,
  un = TRUE,
  binary = "std",
  disp.v.ratio = TRUE,
  m.threshold = 0.1
)

ebal_balance

fit_ebal <- lm_weightit(
  outcome ~ treat,
  data = dat,
  weightit = w_ebal
)

ebal_effect <- {
  ebal_coef <- coef(summary(fit_ebal))["treat", ]
  tibble(
    method = "Entropy balancing",
    estimate = unname(ebal_coef["Estimate"]),
    std_error = unname(ebal_coef["Std. Error"]),
    p_value = unname(ebal_coef[4])
  )
} |>
  mutate(across(c(estimate, std_error, p_value), ~ round(.x, 3)))

ebal_effect

comparison_balance <- tibble(
  method = c("Raw sample", "Exact matching", "CEM", "Entropy balancing"),
  max_abs_smd = c(
    max(abs(unadjusted_balance$Balance$Diff.Un), na.rm = TRUE),
    max(abs(exact_balance$Balance$Diff.Adj), na.rm = TRUE),
    max(abs(cem_balance$Balance$Diff.Adj), na.rm = TRUE),
    max(abs(ebal_balance$Balance$Diff.Adj), na.rm = TRUE)
  ),
  covariates_above_0_1 = c(
    sum(abs(unadjusted_balance$Balance$Diff.Un) > 0.1, na.rm = TRUE),
    sum(abs(exact_balance$Balance$Diff.Adj) > 0.1, na.rm = TRUE),
    sum(abs(cem_balance$Balance$Diff.Adj) > 0.1, na.rm = TRUE),
    sum(abs(ebal_balance$Balance$Diff.Adj) > 0.1, na.rm = TRUE)
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

comparison_balance

comparison_information <- tibble(
  method = c("Exact matching", "CEM", "Entropy balancing"),
  treated_units_used = c(
    sum(matched_exact$treat == 1),
    sum(matched_cem$treat == 1),
    sum(dat$treat == 1)
  ),
  control_units_used = c(
    sum(matched_exact$treat == 0),
    sum(matched_cem$treat == 0),
    sum(dat$treat == 0)
  ),
  control_information = c(
    sum(matched_exact$treat == 0),
    sum(matched_cem$treat == 0),
    ebal_summary$effective.sample.size["Weighted", "Control"]
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

comparison_information

treatment_effect_summary <- bind_rows(
  exact_effect,
  cem_effect,
  ebal_effect
)

treatment_effect_summary

