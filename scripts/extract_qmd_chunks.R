#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

input_qmd <- if (length(args) >= 1) args[[1]] else "docs/matching_methods_report.qmd"
output_r <- if (length(args) >= 2) args[[2]] else "docs/matching_methods_report_full_code.R"
separator_template <- if (length(args) >= 3) args[[3]] else "# ---- chunk %d ----"

if (!file.exists(input_qmd)) {
  stop(sprintf("Input file not found: %s", input_qmd), call. = FALSE)
}

lines <- readLines(input_qmd, warn = FALSE)

in_r_chunk <- FALSE
chunks <- list()
current <- character(0)

for (line in lines) {
  if (!in_r_chunk && grepl("^```\\{r[^}]*\\}\\s*$", line)) {
    in_r_chunk <- TRUE
    current <- character(0)
    next
  }

  if (in_r_chunk && grepl("^```\\s*$", line)) {
    chunks[[length(chunks) + 1]] <- current
    in_r_chunk <- FALSE
    current <- character(0)
    next
  }

  if (in_r_chunk) {
    current <- c(current, line)
  }
}

if (in_r_chunk) {
  stop("Unclosed R chunk detected in input QMD.", call. = FALSE)
}

output_lines <- character(0)

for (i in seq_along(chunks)) {
  output_lines <- c(
    output_lines,
    sprintf(separator_template, i),
    chunks[[i]],
    ""
  )
}

writeLines(output_lines, output_r)

message(sprintf("Wrote %d R chunks from %s to %s", length(chunks), input_qmd, output_r))
