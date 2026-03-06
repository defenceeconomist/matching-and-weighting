# Matching TRG

## Goal

This project will produce a reproducible report showing how to design, diagnose, and estimate observational causal analyses using:

- exact matching
- coarsened exact matching
- entropy balancing for weighting

The report will explain the causal inference background needed to use these methods responsibly and will include worked R examples. The structure follows three core ideas emphasized in the reviewed sources:

- matching and weighting are design-stage tools, not substitutes for causal assumptions
- balance and retained sample size must both be assessed after adjustment
- entropy balancing should be presented as a weighting method, not as a matching method

Matching examples will use the `MatchIt` package, with entropy balancing implemented as a weighting approach using a dedicated weighting package.

This material is intended to fit the training orientation of the UK government Evaluation Academy. The current framing follows the Cabinet Office Evaluation Task Force slide decks for Evaluation Academy 2.0, especially the quasi-experimental designs module:

- GOV.UK resource hub: `https://www.gov.uk/government/publications/etf-evaluation-academy-20-resources`
- Quasi-experimental designs slides: `https://assets.publishing.service.gov.uk/media/67adca8a2535b0468badce35/6-etf-evaluation-academy-20-quasi-experimental-designs-slides.pdf`

Those materials frame the Academy as a way to build a shared understanding of evaluation methods across government and present matching as one quasi-experimental design among several, alongside regression discontinuity, difference-in-differences, synthetic control, and pre-post designs.

## Deliverables

- A Quarto report that explains the methods and presents a design-first workflow.
- R code examples for pre-match diagnostics, exact matching, and coarsened exact matching using `MatchIt`.
- R code examples for entropy balancing, diagnostics, and treatment effect estimation.
- A bibliography covering the reviewed sources and key methodological references.
- A comparison of tradeoffs across the design strategies.
- A structure that is usable in evaluator training or methods-oriented capacity-building settings.

## Report Scope

The report is intended to show:

- how matching, subclassification, and weighting fit within a causal inference workflow
- how matching sits within the wider family of quasi-experimental designs used in government evaluation
- what assumptions are required for causal interpretation
- why the target estimand must be chosen before adjustment
- how to diagnose covariate balance before and after adjustment
- why balance checking and sample retention have to be considered together
- how to estimate treatment effects after design
- when each method is more or less appropriate
- how these methods can be taught as part of good evaluation practice for programme and policy evaluation

## Intended Audience

The primary audience is:

- evaluators
- staff responsible for commissioning or managing evaluations
- analysts who need a practical introduction to design-stage causal adjustment

The framing is aligned with the Evaluation Academy's train-the-trainer model: practical methods instruction that can be taught onward inside departments and applied to real programme and policy evaluations. The slide deck language also suggests the report should help evaluators choose among quasi-experimental options rather than present matching in isolation.

## Roadmap Progress

The report now contains a full end-to-end worked example using `lalonde`, with runnable code for design, diagnostics, estimation, and synthesis across methods. The roadmap items below are complete for the current scope.

Status labels used below:

- `Done`: implemented in the report with substantive prose and runnable analysis code.

| Roadmap item | Status | Current state |
| --- | --- | --- |
| 1. Introduce the causal question, estimand, and observational setting. | `Done` | The report now frames the worked example as a retrospective employment-programme evaluation targeting the `ATT` with the `lalonde` data. |
| 2. Summarize the causal inference background. | `Done` | The report covers potential outcomes, confounding, exchangeability, positivity, common support, consistency, and SUTVA. |
| 3. Introduce subclassification as a motivating conditioning strategy and explain the curse of dimensionality. | `Done` | Both topics are drafted in the causal inference background section. |
| 4. Position matching within the wider quasi-experimental design toolbox and explain when other designs may be preferable. | `Done` | The report includes a dedicated positioning section tied to the Evaluation Academy framing. |
| 5. Define the data structure, treatment, outcome, adjustment covariates, and focal group for the estimand. | `Done` | The report defines the `lalonde` treatment indicator, outcome, design covariates, and treated focal group for the `ATT`. |
| 6. Check initial imbalance before matching using `MatchIt` with `method = NULL`. | `Done` | The pre-match diagnostics section is now runnable and includes descriptive comparisons, `summary(m_out0, un = TRUE)`, `bal.tab()`, and a love plot. |
| 7. Demonstrate exact matching with `MatchIt` using `method = "exact"`. | `Done` | The report includes runnable exact-matching code, diagnostics, and an `ATT` estimate. |
| 8. Demonstrate coarsened exact matching with `MatchIt` using `method = "cem"`. | `Done` | The report includes runnable CEM code (default and tuned variants), diagnostics, and an `ATT` estimate. |
| 9. Demonstrate entropy balancing as a weighting method that targets exact moment balance. | `Done` | The report includes runnable `WeightIt` entropy-balancing code, tuning comparison, diagnostics, and weighted estimation. |
| 10. Assess post-adjustment balance, overlap, retained sample size, and effective sample size. | `Done` | Post-adjustment diagnostics and retention/ESS reporting are implemented for all methods. |
| 11. Estimate treatment effects only after an acceptable design has been achieved. | `Done` | Method-specific treatment-effect estimation is implemented and summarized comparatively. |
| 12. Compare methods on transparency, feasibility, precision, and target population. | `Done` | A cross-method comparison section synthesizes tradeoffs across balance, retention, and interpretability. |
| 13. Translate the methods into evaluator-facing guidance for programme and policy evaluation practice. | `Done` | The guidance section now includes method-selection and reporting recommendations for evaluators. |
| 14. Discuss limitations, sensitivity to design choices, and practical guidance. | `Done` | A dedicated limitations section covers unmeasured confounding, specification sensitivity, and estimand drift. |
| 15. Provide a reproducible appendix with package setup, references, and full code. | `Done` | The report includes package setup, glossary/session information, references, and a full-code companion output. |

## Proposed Package Stack

- `MatchIt`
- `WeightIt`
- `cobalt`
- `dplyr`
- `ggplot2`
- `knitr`

## Render the Website

This project is configured as a Quarto project in `docs/_quarto.yml`.

From the repository root, render the full website with:

```bash
quarto render docs
```

This renders:

- `docs/index.html`
- `docs/matching_methods_report.html` (main article)
- `docs/labs/index.html` and lab scaffold pages
- `docs/slides/index.html` and slide scaffold pages
- `docs/matching_methods_report_full_code.html`
- `docs/matching_methods_report_qa_log.html`

If you only want the main article, run:

```bash
quarto render docs/matching_methods_report.qmd --to html
```

If you only want the slide deck, run:

```bash
quarto render docs/slides/matching_methods_slides.qmd --to revealjs
```

## Extract R Code from QMD

The repository includes a helper script for extracting all R chunks from a Quarto document into a plain `.R` file:

`scripts/extract_qmd_chunks.R`

Default usage (matches this project's report files):

```bash
Rscript scripts/extract_qmd_chunks.R
```

Equivalent explicit usage:

```bash
Rscript scripts/extract_qmd_chunks.R \
  docs/matching_methods_report.qmd \
  docs/matching_methods_report_full_code.R
```

Optional third argument customizes the chunk separator format (default: `# ---- chunk %d ----`).

## Next Steps

- Add hidden-bias sensitivity analysis beyond design diagnostics (e.g., Rosenbaum-style bounds or omitted-variable benchmarking).
- Add a concise executive-summary output tailored for training delivery alongside the full technical report.

## Dataset Expansion Roadmap

The next dataset work should prioritize examples from *Causal Inference: The Mixtape* and *The Effect* that fit the current report's design-first framing: a binary treatment, a clearly defined outcome, enough pre-treatment covariates to make overlap and balance meaningful, and data that are easy to document and reproduce.

Status labels used below:

- `Next`: highest-priority implementation target for the report.
- `Later`: useful extension once the first expansion dataset is complete.

| Roadmap item | Status | Why this dataset fits |
| --- | --- | --- |
| 16. Add `causaldata::black_politicians` as the first non-`lalonde` worked example. | `Next` | This is the strongest immediate candidate from *The Effect* because the matching chapter already uses it for coarsened exact matching and entropy balancing. It shifts the report into a second substantive domain, keeps a binary treatment and outcome, and is well-suited to comparing pruning, balance, and weight concentration. |
| 17. Add a benchmarking appendix using `causaldata::nsw_mixtape` plus `causaldata::cps_mixtape`. | `Next` | This comes directly from the Mixtape matching chapter and is useful even though it is close to `lalonde`. The value is not novelty of topic but benchmarking: it lets the report compare observational matching and weighting estimates against the known NSW experimental result and show more explicitly how poor overlap with observational controls can distort conclusions. |
| 18. Add a short teaching appendix using `causaldata::titanic` for subclassification and simple exact matching. | `Later` | This is not the best policy-style example, so it should not displace the main report narrative. It is still a strong classroom dataset because the treatment, covariates, and curse-of-dimensionality problem are visually transparent and easy to explain in slides or training notes. |
| 19. Document a small acquisition layer for book-native datasets. | `Next` | `MatchIt`, `WeightIt`, and `cobalt` are already installed locally, but `causaldata` is not. The cleanest path is either to add `causaldata` as a documented dependency or to vendor the specific source files needed for the roadmap datasets into a reproducible `data/raw` workflow. |

### Planned sequence

1. Implement `black_politicians` first as the main transportability example for exact matching on a reduced discrete covariate set, CEM on richer bins, and entropy balancing with explicit ESS and weight-dispersion checks.
2. Implement `nsw_mixtape` plus `cps_mixtape` second as a validation appendix that compares adjusted observational estimates with the known experimental benchmark.
3. Add `titanic` only as a compact pedagogic appendix or slide-friendly note, not as the main substantive extension.
4. If dataset expansion becomes a recurring pattern, move the common data-ingest and diagnostics code into reusable helper chunks so each new example only changes the treatment, outcome, and covariate specification.
