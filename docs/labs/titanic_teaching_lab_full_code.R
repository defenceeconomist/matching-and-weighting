# Titanic teaching lab: executable full analysis script.
# Run with:
#   Rscript docs/labs/titanic_teaching_lab_full_code.R

options(width = 120)

# ---- Step 1: Install and load required packages ----
required_packages <- c(
  "causaldata",
  "MatchIt",
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

format_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "-", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

ess <- function(w) {
  (sum(w)^2) / sum(w^2)
}

extract_effect <- function(model, design, treated_retained, control_ess) {
  coef_table <- coef(summary(model))
  estimate <- unname(coef_table["treat", "Estimate"])
  std_error <- unname(coef_table["treat", "Std. Error"])

  data.frame(
    design = design,
    estimate = estimate,
    std_error = std_error,
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    treated_retained = treated_retained,
    control_ess = control_ess,
    row.names = NULL,
    check.names = FALSE
  )
}

# ---- Step 2: Load data and map variables to design roles ----
dat <- causaldata::titanic |>
  dplyr::mutate(
    treat = as.integer(sex == 0),
    outcome = as.integer(survived == 1),
    sex_f = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    age_f = factor(age, levels = c(0, 1), labels = c("Child", "Adult")),
    class_f = factor(class, levels = 1:4, labels = c("First", "Second", "Third", "Crew"))
  )

analysis_formula <- treat ~ class_f + age_f

message("\n[Step 2] Data schema check")
dat |>
  dplyr::select(class_f, age_f, sex_f, outcome) |>
  dplyr::glimpse()

# ---- Step 3: Describe the raw comparison ----
message("\n[Step 3] Raw comparison by sex")
raw_summary <- dat |>
  dplyr::group_by(sex_f) |>
  dplyr::summarise(
    n = dplyr::n(),
    survival_rate = mean(outcome),
    first_class_share = mean(class_f == "First"),
    crew_share = mean(class_f == "Crew"),
    child_share = mean(age_f == "Child"),
    .groups = "drop"
  ) |>
  dplyr::rename(group = sex_f)

raw_summary_display <- raw_summary |>
  dplyr::transmute(
    Group = group,
    N = n,
    `Survival rate` = format_pct(survival_rate),
    `First-class share` = format_pct(first_class_share),
    `Crew share` = format_pct(crew_share),
    `Child share` = format_pct(child_share)
  )

print(raw_summary)

female_survival_rate <- raw_summary$survival_rate[raw_summary$group == "Female"]
male_survival_rate <- raw_summary$survival_rate[raw_summary$group == "Male"]

# ---- Step 4: Inspect exact-match cells before fitting any design ----
message("\n[Step 4] Exact support cells")
all_cells <- expand.grid(
  class_f = levels(dat$class_f),
  age_f = levels(dat$age_f),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    class_f = factor(class_f, levels = levels(dat$class_f)),
    age_f = factor(age_f, levels = levels(dat$age_f))
  )

cell_support <- dat |>
  dplyr::group_by(class_f, age_f) |>
  dplyr::summarise(
    women_n = sum(treat == 1L),
    men_n = sum(treat == 0L),
    women_survival = if (sum(treat == 1L) > 0) mean(outcome[treat == 1L]) else NA_real_,
    men_survival = if (sum(treat == 0L) > 0) mean(outcome[treat == 0L]) else NA_real_,
    .groups = "drop"
  )

cell_support <- all_cells |>
  dplyr::left_join(cell_support, by = c("class_f", "age_f")) |>
  dplyr::mutate(
    women_n = dplyr::coalesce(women_n, 0L),
    men_n = dplyr::coalesce(men_n, 0L),
    women_survival = dplyr::coalesce(women_survival, NA_real_),
    men_survival = dplyr::coalesce(men_survival, NA_real_),
    gap = women_survival - men_survival,
    overlap = dplyr::case_when(
      women_n > 0 & men_n > 0 ~ "Both groups observed",
      women_n + men_n == 0 ~ "Empty cell",
      TRUE ~ "One group only"
    )
  )

cell_support_display <- cell_support |>
  dplyr::transmute(
    `Ticket class` = as.character(class_f),
    Age = as.character(age_f),
    Women = women_n,
    Men = men_n,
    `Women's survival` = format_pct(women_survival),
    `Men's survival` = format_pct(men_survival),
    `Gap (women - men)` = format_pct(gap),
    Overlap = overlap
  )

total_cells_n <- nrow(cell_support)
observed_cells_n <- sum(cell_support$women_n + cell_support$men_n > 0)
supported_cells_n <- sum(cell_support$women_n > 0 & cell_support$men_n > 0)
empty_cells <- cell_support |>
  dplyr::filter(women_n + men_n == 0) |>
  dplyr::transmute(cell = paste(class_f, "x", age_f))
empty_cells_text <- paste(empty_cells$cell, collapse = ", ")

survival_by_cell <- dat |>
  dplyr::group_by(class_f, age_f, sex_f) |>
  dplyr::summarise(
    n = dplyr::n(),
    survival_rate = mean(outcome),
    .groups = "drop"
  )

support_plot <- ggplot2::ggplot(
  survival_by_cell,
  ggplot2::aes(x = class_f, y = survival_rate, fill = sex_f)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.72
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = n),
    position = ggplot2::position_dodge(width = 0.78),
    vjust = -0.35,
    size = 3
  ) +
  ggplot2::facet_wrap(~age_f) +
  ggplot2::coord_cartesian(ylim = c(0, 1.05)) +
  ggplot2::scale_fill_manual(values = c("Female" = "#d95f02", "Male" = "#1b9e77")) +
  ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  ggplot2::labs(
    title = "Survival can be compared within visible age-by-class cells",
    subtitle = "Numbers above bars show the number of passengers in each sex-by-cell group.",
    x = "Ticket class",
    y = "Survival rate",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top")

print(cell_support)
print(support_plot)

# ---- Step 5: Compare exact matching and subclassification ----
message("\n[Step 5] Exact matching and subclassification")
m_raw <- MatchIt::matchit(
  analysis_formula,
  data = dat,
  method = NULL,
  estimand = "ATT"
)

m_exact <- MatchIt::matchit(
  analysis_formula,
  data = dat,
  method = "exact",
  estimand = "ATT"
)

m_subclass <- MatchIt::matchit(
  analysis_formula,
  data = dat,
  method = "subclass",
  estimand = "ATT",
  subclass = 4
)

raw_balance_mat <- summary(m_raw, un = TRUE)$sum.all
exact_balance_mat <- summary(m_exact, un = TRUE)$sum.matched
subclass_balance_mat <- summary(m_subclass, un = TRUE)$sum.across

covariate_labels <- c(
  class_fFirst = "First class",
  class_fSecond = "Second class",
  class_fThird = "Third class",
  class_fCrew = "Crew",
  age_fChild = "Child",
  age_fAdult = "Adult"
)

balance_compare <- data.frame(
  covariate = unname(covariate_labels[rownames(exact_balance_mat)]),
  raw_smd = abs(raw_balance_mat[rownames(exact_balance_mat), "Std. Mean Diff."]),
  exact_smd = abs(exact_balance_mat[, "Std. Mean Diff."]),
  subclass_smd = abs(subclass_balance_mat[rownames(exact_balance_mat), "Std. Mean Diff."]),
  row.names = NULL,
  check.names = FALSE
)

balance_compare_display <- balance_compare |>
  dplyr::transmute(
    Covariate = covariate,
    `Raw |SMD|` = sprintf("%.3f", raw_smd),
    `Exact |SMD|` = sprintf("%.3f", exact_smd),
    `Subclass |SMD|` = sprintf("%.3f", subclass_smd)
  )

print(balance_compare)

exact_data <- MatchIt::match.data(m_exact)
subclass_data <- MatchIt::match.data(m_subclass)

subclass_counts <- with(subclass_data, table(subclass, sex_f))
subclass_counts_display <- data.frame(
  Subclass = rownames(subclass_counts),
  Women = as.integer(subclass_counts[, "Female"]),
  Men = as.integer(subclass_counts[, "Male"]),
  row.names = NULL,
  check.names = FALSE
)

print(subclass_counts_display)

# ---- Step 6: Estimate the raw and adjusted contrasts ----
message("\n[Step 6] Effect comparison")
raw_fit <- stats::lm(outcome ~ treat, data = dat)
exact_fit <- stats::lm(outcome ~ treat, data = exact_data, weights = weights)
subclass_fit <- stats::lm(outcome ~ treat, data = subclass_data, weights = weights)

estimate_table <- dplyr::bind_rows(
  extract_effect(
    raw_fit,
    design = "Raw comparison",
    treated_retained = 1,
    control_ess = sum(dat$treat == 0L)
  ),
  extract_effect(
    exact_fit,
    design = "Exact matching",
    treated_retained = sum(exact_data$treat == 1L) / sum(dat$treat == 1L),
    control_ess = ess(exact_data$weights[exact_data$treat == 0L])
  ),
  extract_effect(
    subclass_fit,
    design = "Subclassification",
    treated_retained = sum(subclass_data$treat == 1L) / sum(dat$treat == 1L),
    control_ess = ess(subclass_data$weights[subclass_data$treat == 0L])
  )
)

estimate_table_display <- estimate_table |>
  dplyr::transmute(
    Design = design,
    `Estimated gap` = format_pct(estimate),
    `95% CI` = paste0(format_pct(ci_low), " to ", format_pct(ci_high)),
    `Treated retained` = format_pct(treated_retained),
    `Control ESS` = sprintf("%.1f", control_ess)
  )

print(estimate_table)

raw_gap_pp <- 100 * estimate_table$estimate[estimate_table$design == "Raw comparison"]
exact_gap_pp <- 100 * estimate_table$estimate[estimate_table$design == "Exact matching"]
subclass_gap_pp <- 100 * estimate_table$estimate[estimate_table$design == "Subclassification"]

message("\nDone: Titanic teaching lab workflow executed.")
