# HISP IE in Practice lab: full R reproduction of the main HISP book replication do-file.
# Run with:
#   Rscript docs/labs/hisp_ie_practice_lab_full_code.R

options(width = 120)

required_packages <- c(
  "haven",
  "readr",
  "dplyr",
  "ggplot2",
  "MatchIt",
  "sandwich",
  "boot",
  "broom",
  "nlme"
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

get_script_path <- function() {
  for (idx in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frame(idx)$ofile)) {
      return(normalizePath(sys.frame(idx)$ofile))
    }
  }

  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)

  if (length(matches) > 0) {
    candidate <- sub(file_arg, "", cmd_args[matches[1]])

    if (grepl("hisp_ie_practice_lab_full_code[.]R$", candidate)) {
      return(normalizePath(candidate))
    }
  }

  normalizePath("docs/labs/hisp_ie_practice_lab_full_code.R")
}

script_path <- get_script_path()
script_dir <- dirname(script_path)
project_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
data_dir <- file.path(project_root, "data", "IE in practice files")

controls <- c(
  "age_hh",
  "age_sp",
  "educ_hh",
  "educ_sp",
  "female_hh",
  "indigenous",
  "hhsize",
  "dirtfloor",
  "bathroom",
  "land",
  "hospital_distance"
)

matching_controls_restricted <- c("age_hh", "educ_hh")
matching_controls_full <- controls

ensure_csv_exports <- function(dir_path) {
  dta_files <- list.files(dir_path, pattern = "[.]dta$", full.names = TRUE)

  dplyr::bind_rows(lapply(dta_files, function(dta_path) {
    csv_path <- sub("[.]dta$", ".csv", dta_path)
    dat <- haven::read_dta(dta_path)
    readr::write_csv(dat, csv_path)

    data.frame(
      dta_file = basename(dta_path),
      csv_file = basename(csv_path),
      rows = nrow(dat),
      columns = ncol(dat),
      stringsAsFactors = FALSE
    )
  }))
}

get_named_value <- function(x, key) {
  key <- as.character(key)

  if (!key %in% names(x)) {
    return(NA_real_)
  }

  unname(as.numeric(x[[key]]))
}

tidy_with_vcov <- function(fit, vcov_mat, df) {
  estimates <- stats::coef(fit)
  std_error <- sqrt(diag(vcov_mat))
  statistic <- estimates / std_error
  conf_crit <- stats::qt(0.975, df = df)

  data.frame(
    term = names(estimates),
    estimate = unname(estimates),
    std.error = unname(std_error),
    statistic = unname(statistic),
    p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
    conf.low = unname(estimates - conf_crit * std_error),
    conf.high = unname(estimates + conf_crit * std_error),
    df = df,
    stringsAsFactors = FALSE
  )
}

append_model_meta <- function(tidy_table, model_name, sample_label, n_obs, n_clusters = NA_integer_) {
  out <- tidy_table
  out$model <- model_name
  out$sample <- sample_label
  out$n_obs <- n_obs
  out$n_clusters <- n_clusters

  out[, c(
    "model",
    "sample",
    "term",
    "estimate",
    "std.error",
    "statistic",
    "p.value",
    "conf.low",
    "conf.high",
    "df",
    "n_obs",
    "n_clusters"
  )]
}

extract_term_row <- function(tidy_table, term, design, sample_label, n_obs, n_clusters = NA_integer_) {
  row <- tidy_table[tidy_table$term == term, , drop = FALSE]

  if (!nrow(row)) {
    return(data.frame(
      design = design,
      sample = sample_label,
      term = term,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      df = NA_real_,
      n_obs = n_obs,
      n_clusters = n_clusters,
      stringsAsFactors = FALSE
    ))
  }

  row$design <- design
  row$sample <- sample_label
  row$n_obs <- n_obs
  row$n_clusters <- n_clusters

  row[, c(
    "design",
    "sample",
    "term",
    "estimate",
    "std.error",
    "statistic",
    "p.value",
    "conf.low",
    "conf.high",
    "df",
    "n_obs",
    "n_clusters"
  )]
}

fit_lm_plain <- function(formula, data, weights_var = NULL) {
  needed <- unique(c(all.vars(formula), weights_var))
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    stats::na.omit()

  if (is.null(weights_var)) {
    fit <- stats::lm(formula, data = dat)
  } else {
    dat$.fit_weights <- dat[[weights_var]]
    fit <- stats::lm(formula, data = dat, weights = .fit_weights)
  }

  tidy <- broom::tidy(fit, conf.int = TRUE)
  tidy$df <- fit$df.residual

  list(
    fit = fit,
    data = dat,
    tidy = tidy,
    n_obs = nrow(dat)
  )
}

fit_lm_cluster <- function(formula, data, cluster_var) {
  needed <- unique(c(all.vars(formula), cluster_var))
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    stats::na.omit()

  fit <- stats::lm(formula, data = dat)
  cluster <- dat[[cluster_var]]
  n_clusters <- dplyr::n_distinct(cluster)
  vcov_mat <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
  tidy <- tidy_with_vcov(fit, vcov_mat, df = n_clusters - 1)

  list(
    fit = fit,
    data = dat,
    vcov = vcov_mat,
    tidy = tidy,
    n_obs = nrow(dat),
    n_clusters = n_clusters
  )
}

fit_iv_2sls_cluster <- function(data, outcome, endog, instrument, controls = character(), cluster_var) {
  needed <- unique(c(outcome, endog, instrument, controls, cluster_var))
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    stats::na.omit()

  x_formula <- stats::reformulate(c(endog, controls))
  z_formula <- stats::reformulate(c(instrument, controls))

  X <- stats::model.matrix(x_formula, data = dat)
  Z <- stats::model.matrix(z_formula, data = dat)
  y <- matrix(dat[[outcome]], ncol = 1)

  W <- solve(crossprod(Z))
  bread <- t(X) %*% Z %*% W %*% t(Z) %*% X
  beta <- solve(bread, t(X) %*% Z %*% W %*% t(Z) %*% y)
  resid <- y - X %*% beta

  cluster <- dat[[cluster_var]]
  cluster_levels <- unique(cluster)
  S <- matrix(0, ncol(Z), ncol(Z))

  for (cluster_id in cluster_levels) {
    idx <- which(cluster == cluster_id)
    zg <- Z[idx, , drop = FALSE]
    ug <- matrix(resid[idx, , drop = FALSE], ncol = 1)
    mg <- t(zg) %*% ug
    S <- S + mg %*% t(mg)
  }

  vcov_mat <- solve(bread) %*% (t(X) %*% Z %*% W %*% S %*% W %*% t(Z) %*% X) %*% solve(bread)

  n_obs <- nrow(dat)
  n_coef <- ncol(X)
  n_clusters <- length(cluster_levels)
  vcov_mat <- vcov_mat * (n_clusters / (n_clusters - 1)) * ((n_obs - 1) / (n_obs - n_coef))

  estimates <- as.numeric(beta)
  std_error <- sqrt(diag(vcov_mat))
  statistic <- estimates / std_error
  df <- n_clusters - 1
  conf_crit <- stats::qt(0.975, df = df)

  tidy <- data.frame(
    term = colnames(X),
    estimate = estimates,
    std.error = std_error,
    statistic = statistic,
    p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
    conf.low = estimates - conf_crit * std_error,
    conf.high = estimates + conf_crit * std_error,
    df = df,
    stringsAsFactors = FALSE
  )

  first_stage <- fit_lm_cluster(
    stats::reformulate(c(instrument, controls), response = endog),
    data = dat,
    cluster_var = cluster_var
  )

  list(
    data = dat,
    tidy = tidy,
    n_obs = n_obs,
    n_clusters = n_clusters,
    first_stage = first_stage
  )
}

ttest_binary <- function(data, outcome, group_var, sample_label) {
  dat <- data |>
    dplyr::select(dplyr::all_of(c(outcome, group_var))) |>
    stats::na.omit()

  dat <- dat[dat[[group_var]] %in% c(0, 1), , drop = FALSE]
  group_factor <- factor(dat[[group_var]], levels = c(0, 1))
  test <- stats::t.test(dat[[outcome]] ~ group_factor, var.equal = TRUE)
  means <- tapply(dat[[outcome]], dat[[group_var]], mean)
  ns <- table(dat[[group_var]])

  data.frame(
    sample = sample_label,
    outcome = outcome,
    group_var = group_var,
    group0_n = get_named_value(ns, "0"),
    group1_n = get_named_value(ns, "1"),
    group0_mean = get_named_value(means, "0"),
    group1_mean = get_named_value(means, "1"),
    diff_group0_minus_group1 = get_named_value(means, "0") - get_named_value(means, "1"),
    statistic = unname(test$statistic),
    p.value = unname(test$p.value),
    conf.low = unname(test$conf.int[1]),
    conf.high = unname(test$conf.int[2]),
    stringsAsFactors = FALSE
  )
}

ttest_many <- function(data, outcomes, group_var, sample_label) {
  dplyr::bind_rows(lapply(outcomes, function(outcome) {
    ttest_binary(data, outcome, group_var, sample_label)
  }))
}

coalesce_pair <- function(left, right) {
  ifelse(is.na(left), right, left)
}

build_matching_wide_data <- function(data) {
  wide <- reshape(
    as.data.frame(data),
    direction = "wide",
    idvar = "household_identifier",
    timevar = "round",
    sep = ""
  )

  keep_vars <- c(
    "locality_identifier",
    "treatment_locality",
    "promotion_locality",
    "eligible",
    "enrolled",
    "enrolled_rp",
    "poverty_index",
    "age_hh",
    "age_sp",
    "educ_hh",
    "educ_sp",
    "female_hh",
    "indigenous",
    "hhsize",
    "dirtfloor",
    "bathroom",
    "land",
    "hospital_distance",
    "hospital"
  )

  for (var_name in keep_vars) {
    wide[[var_name]] <- coalesce_pair(wide[[paste0(var_name, "0")]], wide[[paste0(var_name, "1")]])
  }

  wide[order(wide$household_identifier), ]
}

build_ps_density_plot <- function(data, title) {
  plot_dat <- data.frame(
    pscore = data$pscore,
    group = ifelse(data$enrolled == 1, "Enrolled", "Not enrolled"),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = pscore, color = group, fill = group)) +
    ggplot2::geom_density(alpha = 0.18, linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("Enrolled" = "#1b4d3e", "Not enrolled" = "#b24a2e")) +
    ggplot2::scale_fill_manual(values = c("Enrolled" = "#1b4d3e", "Not enrolled" = "#b24a2e")) +
    ggplot2::labs(
      title = title,
      x = "Estimated propensity score",
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

fit_match_branch <- function(data, covariates, label, seed = 100) {
  needed <- unique(c("enrolled", "health_expenditures0", "health_expenditures1", covariates))
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    stats::na.omit()

  ps_formula <- stats::reformulate(covariates, response = "enrolled")
  ps_fit <- stats::glm(ps_formula, data = dat, family = stats::binomial(link = "probit"))
  dat$pscore <- stats::predict(ps_fit, type = "response")

  set.seed(seed)
  match_obj <- MatchIt::matchit(
    ps_formula,
    data = dat,
    method = "nearest",
    distance = "glm",
    link = "probit",
    ratio = 1,
    replace = FALSE,
    m.order = "random",
    estimand = "ATT",
    normalize = FALSE
  )

  matched_dat <- MatchIt::match.data(match_obj)

  support <- data.frame(
    enrolled = dat$enrolled,
    support = as.integer(match_obj$weights > 0),
    stringsAsFactors = FALSE
  ) |>
    dplyr::count(enrolled, support) |>
    dplyr::mutate(
      group = ifelse(enrolled == 1, "Enrolled", "Not enrolled"),
      support_label = ifelse(support == 1, "Matched support", "Outside matched support")
    ) |>
    dplyr::select(group, support_label, n)

  weighted_fit <- fit_lm_plain(health_expenditures1 ~ enrolled, matched_dat, weights_var = "weights")

  boot_stat <- function(sample_data, indices) {
    boot_dat <- sample_data[indices, , drop = FALSE]
    boot_dat <- stats::na.omit(boot_dat)

    if (!nrow(boot_dat) || length(unique(boot_dat$enrolled)) < 2) {
      return(NA_real_)
    }

    match_boot <- tryCatch(
      MatchIt::matchit(
        ps_formula,
        data = boot_dat,
        method = "nearest",
        distance = "glm",
        link = "probit",
        ratio = 1,
        replace = FALSE,
        m.order = "random",
        estimand = "ATT",
        normalize = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(match_boot)) {
      return(NA_real_)
    }

    matched_boot <- tryCatch(MatchIt::match.data(match_boot), error = function(e) NULL)

    if (is.null(matched_boot) || !nrow(matched_boot) || length(unique(matched_boot$enrolled)) < 2) {
      return(NA_real_)
    }

    fit_boot <- stats::lm(health_expenditures1 ~ enrolled, data = matched_boot, weights = weights)
    unname(stats::coef(fit_boot)[["enrolled"]])
  }

  set.seed(seed)
  boot_fit <- boot::boot(dat, statistic = boot_stat, R = 50)
  boot_vals <- as.numeric(boot_fit$t)

  bootstrap_summary <- data.frame(
    design = label,
    bootstrap_reps = 50,
    valid_reps = sum(!is.na(boot_vals)),
    att_original = unname(stats::coef(weighted_fit$fit)[["enrolled"]]),
    bootstrap_mean = mean(boot_vals, na.rm = TRUE),
    bootstrap_se = stats::sd(boot_vals, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  list(
    data = dat,
    ps_fit = ps_fit,
    match_obj = match_obj,
    matched_data = matched_dat,
    support = support,
    weighted_fit = weighted_fit,
    bootstrap = boot_fit,
    bootstrap_summary = bootstrap_summary,
    ps_plot = build_ps_density_plot(dat, paste0(label, ": propensity-score overlap"))
  )
}

compute_icc <- function(data, outcome, cluster_var) {
  needed <- c(outcome, cluster_var)
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    stats::na.omit()

  fit <- nlme::lme(
    stats::reformulate("1", response = outcome),
    random = stats::as.formula(paste("~ 1 |", cluster_var)),
    data = dat,
    na.action = stats::na.omit,
    control = nlme::lmeControl(returnObject = TRUE)
  )

  varcorr <- nlme::VarCorr(fit)
  between <- as.numeric(varcorr[1, "Variance"])
  within <- as.numeric(varcorr[nrow(varcorr), "Variance"])
  between / (between + within)
}

two_sample_n_per_arm <- function(sd_outcome, minimum_detectable_effect, power, alpha = 0.05) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- stats::qnorm(power)

  ceiling(((z_alpha + z_beta)^2 * (sd_outcome^2 + sd_outcome^2)) / minimum_detectable_effect^2)
}

cluster_adjusted_total_n <- function(total_n_no_cluster, rho, num_clusters) {
  total_n <- total_n_no_cluster

  for (iter in seq_len(100)) {
    avg_cluster_size <- total_n / num_clusters
    updated_total_n <- total_n_no_cluster * (1 + (avg_cluster_size - 1) * rho)

    if (abs(updated_total_n - total_n) < 1e-8) {
      total_n <- updated_total_n
      break
    }

    total_n <- updated_total_n
  }

  ceiling(total_n)
}

build_power_table <- function(mean_outcome, sd_outcome, delta_values, power, direction, outcome_label) {
  target_mean <- switch(
    direction,
    decrease = mean_outcome - delta_values,
    increase = mean_outcome + delta_values
  )

  data.frame(
    outcome = outcome_label,
    power = power,
    minimum_detectable_effect = delta_values,
    target_mean = target_mean,
    n_per_arm = vapply(
      delta_values,
      function(delta) two_sample_n_per_arm(sd_outcome, delta, power),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(total_n = 2 * n_per_arm)
}

augment_clustered_power <- function(power_table, rho, num_clusters) {
  power_table |>
    dplyr::rowwise() |>
    dplyr::mutate(
      rho = rho,
      num_clusters = num_clusters,
      total_n_clustered = cluster_adjusted_total_n(total_n, rho, num_clusters),
      n_per_arm_clustered = ceiling(total_n_clustered / 2),
      avg_cluster_size = ceiling(total_n_clustered / num_clusters),
      design_effect = total_n_clustered / total_n
    ) |>
    dplyr::ungroup()
}

csv_export_manifest <- ensure_csv_exports(data_dir)
evaluation_csv_path <- file.path(data_dir, "evaluation.csv")
evaluation_dat <- readr::read_csv(evaluation_csv_path, show_col_types = FALSE)

data_overview <- data.frame(
  dataset = "evaluation.csv",
  n_rows = nrow(evaluation_dat),
  n_cols = ncol(evaluation_dat),
  n_households = dplyr::n_distinct(evaluation_dat$household_identifier),
  n_localities = dplyr::n_distinct(evaluation_dat$locality_identifier),
  baseline_rows = sum(evaluation_dat$round == 0, na.rm = TRUE),
  followup_rows = sum(evaluation_dat$round == 1, na.rm = TRUE),
  stringsAsFactors = FALSE
)

data_preview <- evaluation_dat |>
  dplyr::select(
    household_identifier,
    locality_identifier,
    treatment_locality,
    promotion_locality,
    eligible,
    enrolled,
    round,
    health_expenditures
  ) |>
  head(10)

panel_completeness <- evaluation_dat |>
  dplyr::count(household_identifier, name = "rounds_observed") |>
  dplyr::count(rounds_observed, name = "households")

# ---- Method 1: before-after within enrolled households in treatment localities ----
method_1_dat <- evaluation_dat |>
  dplyr::filter(treatment_locality == 1, enrolled == 1)

method_1_ttest <- ttest_binary(
  method_1_dat,
  outcome = "health_expenditures",
  group_var = "round",
  sample_label = "Method 1: treatment localities, enrolled households"
)

method_1_reg_a <- fit_lm_cluster(
  health_expenditures ~ round,
  data = method_1_dat,
  cluster_var = "locality_identifier"
)

method_1_reg_b <- fit_lm_cluster(
  stats::reformulate(c("round", controls), response = "health_expenditures"),
  data = method_1_dat,
  cluster_var = "locality_identifier"
)

method_1_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_1_reg_a$tidy,
    "Method 1: before-after without controls",
    "Treatment localities, enrolled households",
    method_1_reg_a$n_obs,
    method_1_reg_a$n_clusters
  ),
  append_model_meta(
    method_1_reg_b$tidy,
    "Method 1: before-after with controls",
    "Treatment localities, enrolled households",
    method_1_reg_b$n_obs,
    method_1_reg_b$n_clusters
  )
)

# ---- Method 2: enrolled vs not enrolled at follow-up in treatment localities ----
method_2_dat <- evaluation_dat |>
  dplyr::filter(treatment_locality == 1, round == 1)

method_2_ttest <- ttest_binary(
  method_2_dat,
  outcome = "health_expenditures",
  group_var = "enrolled",
  sample_label = "Method 2: treatment localities at follow-up"
)

method_2_reg_a <- fit_lm_cluster(
  health_expenditures ~ enrolled,
  data = method_2_dat,
  cluster_var = "locality_identifier"
)

method_2_reg_b <- fit_lm_cluster(
  stats::reformulate(c("enrolled", controls), response = "health_expenditures"),
  data = method_2_dat,
  cluster_var = "locality_identifier"
)

method_2_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_2_reg_a$tidy,
    "Method 2: enrolled vs not enrolled without controls",
    "Treatment localities at follow-up",
    method_2_reg_a$n_obs,
    method_2_reg_a$n_clusters
  ),
  append_model_meta(
    method_2_reg_b$tidy,
    "Method 2: enrolled vs not enrolled with controls",
    "Treatment localities at follow-up",
    method_2_reg_b$n_obs,
    method_2_reg_b$n_clusters
  )
)

# ---- Method 3: randomized assignment among eligible households ----
method_3_dat <- evaluation_dat |>
  dplyr::filter(eligible == 1)

method_3_baseline_outcome_ttest <- ttest_binary(
  method_3_dat |> dplyr::filter(round == 0),
  outcome = "health_expenditures",
  group_var = "treatment_locality",
  sample_label = "Method 3: eligible households at baseline"
)

method_3_baseline_control_ttests <- ttest_many(
  method_3_dat |> dplyr::filter(round == 0),
  outcomes = controls,
  group_var = "treatment_locality",
  sample_label = "Method 3: baseline balance on controls"
)

method_3_outcome_ttests <- dplyr::bind_rows(
  ttest_binary(
    method_3_dat |> dplyr::filter(round == 0),
    outcome = "health_expenditures",
    group_var = "treatment_locality",
    sample_label = "Method 3: baseline outcome"
  ),
  ttest_binary(
    method_3_dat |> dplyr::filter(round == 1),
    outcome = "health_expenditures",
    group_var = "treatment_locality",
    sample_label = "Method 3: follow-up outcome"
  )
)

method_3_reg_a <- fit_lm_cluster(
  health_expenditures ~ treatment_locality,
  data = method_3_dat |> dplyr::filter(round == 1),
  cluster_var = "locality_identifier"
)

method_3_reg_b <- fit_lm_cluster(
  stats::reformulate(c("treatment_locality", controls), response = "health_expenditures"),
  data = method_3_dat |> dplyr::filter(round == 1),
  cluster_var = "locality_identifier"
)

method_3_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_3_reg_a$tidy,
    "Method 3: randomized assignment without controls",
    "Eligible households at follow-up",
    method_3_reg_a$n_obs,
    method_3_reg_a$n_clusters
  ),
  append_model_meta(
    method_3_reg_b$tidy,
    "Method 3: randomized assignment with controls",
    "Eligible households at follow-up",
    method_3_reg_b$n_obs,
    method_3_reg_b$n_clusters
  )
)

# ---- Method 4: instrumental variables under randomized promotion ----
method_4_dat <- evaluation_dat

method_4_ttests <- dplyr::bind_rows(
  ttest_binary(
    method_4_dat |> dplyr::filter(round == 0),
    outcome = "health_expenditures",
    group_var = "promotion_locality",
    sample_label = "Method 4: baseline health expenditures by promotion locality"
  ),
  ttest_binary(
    method_4_dat |> dplyr::filter(round == 1),
    outcome = "health_expenditures",
    group_var = "promotion_locality",
    sample_label = "Method 4: follow-up health expenditures by promotion locality"
  ),
  ttest_binary(
    method_4_dat |> dplyr::filter(round == 1),
    outcome = "enrolled_rp",
    group_var = "promotion_locality",
    sample_label = "Method 4: follow-up enrollment by promotion locality"
  )
)

method_4_iv_a <- fit_iv_2sls_cluster(
  data = method_4_dat |> dplyr::filter(round == 1),
  outcome = "health_expenditures",
  endog = "enrolled_rp",
  instrument = "promotion_locality",
  controls = character(),
  cluster_var = "locality_identifier"
)

method_4_iv_b <- fit_iv_2sls_cluster(
  data = method_4_dat |> dplyr::filter(round == 1),
  outcome = "health_expenditures",
  endog = "enrolled_rp",
  instrument = "promotion_locality",
  controls = controls,
  cluster_var = "locality_identifier"
)

method_4_first_stage <- dplyr::bind_rows(
  append_model_meta(
    method_4_iv_a$first_stage$tidy,
    "Method 4 first stage: no controls",
    "All households at follow-up",
    method_4_iv_a$first_stage$n_obs,
    method_4_iv_a$first_stage$n_clusters
  ),
  append_model_meta(
    method_4_iv_b$first_stage$tidy,
    "Method 4 first stage: with controls",
    "All households at follow-up",
    method_4_iv_b$first_stage$n_obs,
    method_4_iv_b$first_stage$n_clusters
  )
)

method_4_iv_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_4_iv_a$tidy,
    "Method 4 second stage: no controls",
    "All households at follow-up",
    method_4_iv_a$n_obs,
    method_4_iv_a$n_clusters
  ),
  append_model_meta(
    method_4_iv_b$tidy,
    "Method 4 second stage: with controls",
    "All households at follow-up",
    method_4_iv_b$n_obs,
    method_4_iv_b$n_clusters
  )
)

# ---- Method 5: regression discontinuity ----
method_5_dat <- evaluation_dat |>
  dplyr::filter(treatment_locality == 1) |>
  dplyr::mutate(
    poverty_index_left = ifelse(poverty_index <= 58, poverty_index - 58, 0),
    poverty_index_right = ifelse(poverty_index > 58, poverty_index - 58, 0)
  )

method_5_baseline_dat <- method_5_dat |> dplyr::filter(round == 0)
method_5_followup_dat <- method_5_dat |> dplyr::filter(round == 1)

method_5_baseline_fit <- fit_lm_plain(
  health_expenditures ~ poverty_index,
  data = method_5_baseline_dat
)

method_5_baseline_dat$he_pred0 <- stats::predict(method_5_baseline_fit$fit, newdata = method_5_baseline_dat)

method_5_followup_fit_a <- fit_lm_plain(
  health_expenditures ~ poverty_index_left + poverty_index_right + eligible,
  data = method_5_followup_dat
)

method_5_followup_dat$he_pred1 <- stats::predict(method_5_followup_fit_a$fit, newdata = method_5_followup_dat)

method_5_followup_fit_b <- fit_lm_plain(
  stats::reformulate(
    c("eligible", "poverty_index_left", "poverty_index_right", controls),
    response = "health_expenditures"
  ),
  data = method_5_followup_dat
)

method_5_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_5_baseline_fit$tidy,
    "Method 5: baseline health expenditures on poverty index",
    "Treatment localities at baseline",
    method_5_baseline_fit$n_obs
  ),
  append_model_meta(
    method_5_followup_fit_a$tidy,
    "Method 5: RDD without controls",
    "Treatment localities at follow-up",
    method_5_followup_fit_a$n_obs
  ),
  append_model_meta(
    method_5_followup_fit_b$tidy,
    "Method 5: RDD with controls",
    "Treatment localities at follow-up",
    method_5_followup_fit_b$n_obs
  )
)

rdd_density_plot <- ggplot2::ggplot(method_5_baseline_dat, ggplot2::aes(x = poverty_index)) +
  ggplot2::geom_density(fill = "#d9e6f2", color = "#24415d", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = 58, linetype = "dashed", color = "#b24a2e") +
  ggplot2::labs(
    title = "Method 5: baseline poverty-index density",
    x = "Baseline poverty index",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 11)

rdd_participation_dat <- method_5_dat |>
  dplyr::group_by(poverty_index) |>
  dplyr::summarise(enrollment_rate = mean(enrolled, na.rm = TRUE), .groups = "drop")

rdd_participation_plot <- ggplot2::ggplot(
  rdd_participation_dat,
  ggplot2::aes(x = poverty_index, y = enrollment_rate)
) +
  ggplot2::geom_point(color = "#1b4d3e", alpha = 0.55, size = 1.4) +
  ggplot2::geom_vline(xintercept = 58, linetype = "dashed", color = "#b24a2e") +
  ggplot2::labs(
    title = "Method 5: participation by poverty index",
    x = "Baseline poverty index",
    y = "Participation rate in HISP"
  ) +
  ggplot2::theme_minimal(base_size = 11)

rdd_followup_plot <- ggplot2::ggplot(
  method_5_followup_dat |>
    dplyr::filter(health_expenditures < 60),
  ggplot2::aes(x = poverty_index, y = health_expenditures)
) +
  ggplot2::geom_point(alpha = 0.12, size = 1, color = "#24415d") +
  ggplot2::geom_line(
    data = method_5_followup_dat |>
      dplyr::arrange(poverty_index),
    ggplot2::aes(y = he_pred1),
    color = "#b24a2e",
    linewidth = 0.9
  ) +
  ggplot2::geom_vline(xintercept = 58, linetype = "dashed", color = "#3b3b3b") +
  ggplot2::labs(
    title = "Method 5: follow-up health expenditures around the cutoff",
    x = "Baseline poverty index",
    y = "Health expenditures"
  ) +
  ggplot2::theme_minimal(base_size = 11)

# ---- Method 6: difference-in-differences ----
method_6_dat <- evaluation_dat |>
  dplyr::filter(treatment_locality == 1) |>
  dplyr::mutate(enrolled_round = enrolled * round)

method_6_ttests <- dplyr::bind_rows(
  ttest_binary(
    method_6_dat |> dplyr::filter(enrolled == 0),
    outcome = "health_expenditures",
    group_var = "round",
    sample_label = "Method 6: nonenrolled households"
  ),
  ttest_binary(
    method_6_dat |> dplyr::filter(enrolled == 1),
    outcome = "health_expenditures",
    group_var = "round",
    sample_label = "Method 6: enrolled households"
  )
)

method_6_reg_a <- fit_lm_cluster(
  health_expenditures ~ enrolled_round + round + enrolled,
  data = method_6_dat,
  cluster_var = "locality_identifier"
)

method_6_reg_b <- fit_lm_cluster(
  stats::reformulate(c("enrolled_round", "round", "enrolled", controls), response = "health_expenditures"),
  data = method_6_dat,
  cluster_var = "locality_identifier"
)

method_6_regressions <- dplyr::bind_rows(
  append_model_meta(
    method_6_reg_a$tidy,
    "Method 6: difference-in-differences without controls",
    "Treatment localities, both rounds",
    method_6_reg_a$n_obs,
    method_6_reg_a$n_clusters
  ),
  append_model_meta(
    method_6_reg_b$tidy,
    "Method 6: difference-in-differences with controls",
    "Treatment localities, both rounds",
    method_6_reg_b$n_obs,
    method_6_reg_b$n_clusters
  )
)

# ---- Method 7: propensity-score matching ----
matching_wide_dat <- build_matching_wide_data(evaluation_dat)

matching_wide_summary <- data.frame(
  wide_rows = nrow(matching_wide_dat),
  enrolled_households = sum(matching_wide_dat$enrolled == 1, na.rm = TRUE),
  not_enrolled_households = sum(matching_wide_dat$enrolled == 0, na.rm = TRUE),
  stringsAsFactors = FALSE
)

matching_restricted <- fit_match_branch(
  matching_wide_dat,
  covariates = matching_controls_restricted,
  label = "Method 7 restricted matching"
)

matching_restricted_support <- matching_restricted$support
matching_restricted_regression <- append_model_meta(
  matching_restricted$weighted_fit$tidy,
  "Method 7 restricted matching: weighted regression",
  "Wide household file, covariates = age_hh + educ_hh",
  matching_restricted$weighted_fit$n_obs
)
matching_restricted_bootstrap <- matching_restricted$bootstrap_summary
matching_restricted_ps_plot <- matching_restricted$ps_plot

matching_full <- fit_match_branch(
  matching_wide_dat,
  covariates = matching_controls_full,
  label = "Method 7 full matching"
)

matching_full_support <- matching_full$support
matching_full_regression <- append_model_meta(
  matching_full$weighted_fit$tidy,
  "Method 7 full matching: weighted regression",
  "Wide household file, covariates = full control set",
  matching_full$weighted_fit$n_obs
)
matching_full_bootstrap <- matching_full$bootstrap_summary
matching_full_ps_plot <- matching_full$ps_plot

matched_did_pairs <- matching_full$matched_data |>
  dplyr::arrange(subclass, dplyr::desc(enrolled)) |>
  dplyr::group_by(subclass) |>
  dplyr::summarise(
    treated_baseline = health_expenditures0[enrolled == 1][1],
    treated_followup = health_expenditures1[enrolled == 1][1],
    control_baseline = health_expenditures0[enrolled == 0][1],
    control_followup = health_expenditures1[enrolled == 0][1],
    matched_dd = (treated_followup - treated_baseline) - (control_followup - control_baseline),
    .groups = "drop"
  )

matched_did_manual_summary <- matched_did_pairs |>
  dplyr::summarise(
    matched_pairs = dplyr::n(),
    mean_matched_dd = mean(matched_dd, na.rm = TRUE),
    sd_matched_dd = stats::sd(matched_dd, na.rm = TRUE),
    min_matched_dd = min(matched_dd, na.rm = TRUE),
    median_matched_dd = stats::median(matched_dd, na.rm = TRUE),
    max_matched_dd = max(matched_dd, na.rm = TRUE)
  )

matched_did_reg_dat <- matching_full$matched_data |>
  dplyr::mutate(diff = health_expenditures1 - health_expenditures0)

matched_did_reg_fit <- fit_lm_plain(
  diff ~ enrolled,
  data = matched_did_reg_dat,
  weights_var = "weights"
)

matched_did_regression <- append_model_meta(
  matched_did_reg_fit$tidy,
  "Method 7 matched difference-in-differences",
  "Matched full-set sample",
  matched_did_reg_fit$n_obs
)

# ---- Power calculations ----
power_dat <- evaluation_dat |>
  dplyr::filter(eligible == 1)

power_baseline_dat <- power_dat |>
  dplyr::filter(round == 0)

power_followup_treat_dat <- power_dat |>
  dplyr::filter(round == 1, treatment_locality == 1)

health_rho_baseline <- compute_icc(power_baseline_dat, "health_expenditures", "locality_identifier")
hospital_rho_baseline <- compute_icc(power_baseline_dat, "hospital", "locality_identifier")
health_rho_followup <- compute_icc(power_followup_treat_dat, "health_expenditures", "locality_identifier")
hospital_rho_followup <- compute_icc(power_followup_treat_dat, "hospital", "locality_identifier")

power_parameter_summary <- dplyr::bind_rows(
  data.frame(
    outcome = "health_expenditures",
    sample = "Baseline eligible households",
    mean = mean(power_baseline_dat$health_expenditures, na.rm = TRUE),
    sd = stats::sd(power_baseline_dat$health_expenditures, na.rm = TRUE),
    rho = health_rho_baseline,
    stringsAsFactors = FALSE
  ),
  data.frame(
    outcome = "hospital",
    sample = "Baseline eligible households",
    mean = mean(power_baseline_dat$hospital, na.rm = TRUE),
    sd = stats::sd(power_baseline_dat$hospital, na.rm = TRUE),
    rho = hospital_rho_baseline,
    stringsAsFactors = FALSE
  ),
  data.frame(
    outcome = "health_expenditures",
    sample = "Follow-up treated eligible households",
    mean = mean(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
    sd = stats::sd(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
    rho = health_rho_followup,
    stringsAsFactors = FALSE
  ),
  data.frame(
    outcome = "hospital",
    sample = "Follow-up treated eligible households",
    mean = mean(power_followup_treat_dat$hospital, na.rm = TRUE),
    sd = stats::sd(power_followup_treat_dat$hospital, na.rm = TRUE),
    rho = hospital_rho_followup,
    stringsAsFactors = FALSE
  )
)

power_health_no_cluster_90 <- build_power_table(
  mean_outcome = mean(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
  sd_outcome = stats::sd(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
  delta_values = c(1, 2, 3),
  power = 0.9,
  direction = "decrease",
  outcome_label = "Health expenditures"
)

power_health_no_cluster_80 <- build_power_table(
  mean_outcome = mean(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
  sd_outcome = stats::sd(power_followup_treat_dat$health_expenditures, na.rm = TRUE),
  delta_values = c(1, 2, 3),
  power = 0.8,
  direction = "decrease",
  outcome_label = "Health expenditures"
)

power_hospital_no_cluster_80 <- build_power_table(
  mean_outcome = mean(power_followup_treat_dat$hospital, na.rm = TRUE),
  sd_outcome = stats::sd(power_followup_treat_dat$hospital, na.rm = TRUE),
  delta_values = c(0.01, 0.02, 0.03),
  power = 0.8,
  direction = "increase",
  outcome_label = "Hospitalization rate"
)

power_health_clustered_100 <- augment_clustered_power(
  power_health_no_cluster_80,
  rho = health_rho_followup,
  num_clusters = 100
)

power_health_clustered_varying_clusters <- dplyr::bind_rows(lapply(
  c(30, 58, 81, 90, 120),
  function(num_clusters) {
    augment_clustered_power(
      power_health_no_cluster_80 |>
        dplyr::filter(minimum_detectable_effect == 2),
      rho = health_rho_followup,
      num_clusters = num_clusters
    )
  }
))

# ---- Consolidated effect summary ----
effect_pathway_summary <- dplyr::bind_rows(
  extract_term_row(
    method_1_reg_a$tidy,
    "round",
    "Method 1 before-after",
    "Treatment localities, enrolled households",
    method_1_reg_a$n_obs,
    method_1_reg_a$n_clusters
  ),
  extract_term_row(
    method_1_reg_b$tidy,
    "round",
    "Method 1 before-after + controls",
    "Treatment localities, enrolled households",
    method_1_reg_b$n_obs,
    method_1_reg_b$n_clusters
  ),
  extract_term_row(
    method_2_reg_a$tidy,
    "enrolled",
    "Method 2 enrolled vs not enrolled",
    "Treatment localities at follow-up",
    method_2_reg_a$n_obs,
    method_2_reg_a$n_clusters
  ),
  extract_term_row(
    method_2_reg_b$tidy,
    "enrolled",
    "Method 2 enrolled vs not enrolled + controls",
    "Treatment localities at follow-up",
    method_2_reg_b$n_obs,
    method_2_reg_b$n_clusters
  ),
  extract_term_row(
    method_3_reg_a$tidy,
    "treatment_locality",
    "Method 3 randomized assignment",
    "Eligible households at follow-up",
    method_3_reg_a$n_obs,
    method_3_reg_a$n_clusters
  ),
  extract_term_row(
    method_3_reg_b$tidy,
    "treatment_locality",
    "Method 3 randomized assignment + controls",
    "Eligible households at follow-up",
    method_3_reg_b$n_obs,
    method_3_reg_b$n_clusters
  ),
  extract_term_row(
    method_4_iv_a$tidy,
    "enrolled_rp",
    "Method 4 IV",
    "All households at follow-up",
    method_4_iv_a$n_obs,
    method_4_iv_a$n_clusters
  ),
  extract_term_row(
    method_4_iv_b$tidy,
    "enrolled_rp",
    "Method 4 IV + controls",
    "All households at follow-up",
    method_4_iv_b$n_obs,
    method_4_iv_b$n_clusters
  ),
  extract_term_row(
    method_5_followup_fit_a$tidy,
    "eligible",
    "Method 5 RDD",
    "Treatment localities at follow-up",
    method_5_followup_fit_a$n_obs
  ),
  extract_term_row(
    method_5_followup_fit_b$tidy,
    "eligible",
    "Method 5 RDD + controls",
    "Treatment localities at follow-up",
    method_5_followup_fit_b$n_obs
  ),
  extract_term_row(
    method_6_reg_a$tidy,
    "enrolled_round",
    "Method 6 difference-in-differences",
    "Treatment localities, both rounds",
    method_6_reg_a$n_obs,
    method_6_reg_a$n_clusters
  ),
  extract_term_row(
    method_6_reg_b$tidy,
    "enrolled_round",
    "Method 6 difference-in-differences + controls",
    "Treatment localities, both rounds",
    method_6_reg_b$n_obs,
    method_6_reg_b$n_clusters
  ),
  extract_term_row(
    matching_restricted$weighted_fit$tidy,
    "enrolled",
    "Method 7 restricted matching",
    "Wide household file",
    matching_restricted$weighted_fit$n_obs
  ),
  extract_term_row(
    matching_full$weighted_fit$tidy,
    "enrolled",
    "Method 7 full matching",
    "Wide household file",
    matching_full$weighted_fit$n_obs
  ),
  extract_term_row(
    matched_did_reg_fit$tidy,
    "enrolled",
    "Method 7 matched difference-in-differences",
    "Matched full-set sample",
    matched_did_reg_fit$n_obs
  )
)
