# HISP IE in Practice lab: evaluated code with processed outputs.
# Run with:
#   Rscript docs/labs/hisp_ie_practice_lab_evaluated_code.R

options(width = 120)

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

    if (grepl("hisp_ie_practice_lab_evaluated_code[.]R$", candidate)) {
      return(normalizePath(candidate))
    }
  }

  normalizePath("docs/labs/hisp_ie_practice_lab_evaluated_code.R")
}

script_dir <- dirname(get_script_path())
source(file.path(script_dir, "hisp_ie_practice_lab_full_code.R"), local = TRUE)

message("\n[CSV export manifest]")
print(csv_export_manifest)

message("\n[Data overview]")
print(data_overview)

message("\n[Panel completeness]")
print(panel_completeness)

message("\n[Method effect summary]")
print(effect_pathway_summary)

message("\n[Method 4 first-stage terms]")
print(method_4_first_stage[method_4_first_stage$term %in% c("promotion_locality", "age_hh", "educ_hh"), ])

message("\n[Method 5 RDD coefficients]")
print(method_5_regressions[method_5_regressions$term %in% c("poverty_index", "poverty_index_left", "poverty_index_right", "eligible"), ])

message("\n[Restricted matching bootstrap]")
print(matching_restricted_bootstrap)

message("\n[Full matching bootstrap]")
print(matching_full_bootstrap)

message("\n[Matched DiD summary]")
print(matched_did_manual_summary)

message("\n[Power parameter summary]")
print(power_parameter_summary)

message("\n[Clustered power table]")
print(power_health_clustered_varying_clusters)

message("\nDone: evaluated HISP IE in Practice workflow objects created.")
