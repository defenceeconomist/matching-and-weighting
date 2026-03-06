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

The report scaffold and most of the conceptual narrative are now in place. The main gap is the empirical layer: a concrete example dataset, runnable code chunks, rendered diagnostics, and final comparison outputs.

Status labels used below:

- `Done`: substantive prose is drafted in the report.
- `In progress`: the section exists, but still depends on a concrete dataset, runnable code, or fuller synthesis.

| Roadmap item | Status | Current state |
| --- | --- | --- |
| 1. Introduce the causal question, estimand, and observational setting. | `In progress` | Estimands and the observational framing are introduced, but the concrete study question and dataset are still placeholders. |
| 2. Summarize the causal inference background. | `Done` | The report covers potential outcomes, confounding, exchangeability, positivity, common support, consistency, and SUTVA. |
| 3. Introduce subclassification as a motivating conditioning strategy and explain the curse of dimensionality. | `Done` | Both topics are drafted in the causal inference background section. |
| 4. Position matching within the wider quasi-experimental design toolbox and explain when other designs may be preferable. | `Done` | The report includes a dedicated positioning section tied to the Evaluation Academy framing. |
| 5. Define the data structure, treatment, outcome, adjustment covariates, and focal group for the estimand. | `In progress` | The section structure exists, but the example dataset and variable definitions are not yet filled in. |
| 6. Check initial imbalance before matching using `MatchIt` with `method = NULL`. | `In progress` | The pre-match diagnostics section and example code skeleton are present, but not yet runnable. |
| 7. Demonstrate exact matching with `MatchIt` using `method = "exact"`. | `In progress` | Method overview and commented implementation are drafted. |
| 8. Demonstrate coarsened exact matching with `MatchIt` using `method = "cem"`. | `In progress` | Method overview and commented implementation are drafted. |
| 9. Demonstrate entropy balancing as a weighting method that targets exact moment balance. | `In progress` | Method overview and commented `WeightIt` implementation are drafted. |
| 10. Assess post-adjustment balance, overlap, retained sample size, and effective sample size. | `In progress` | Diagnostic sections are scaffolded, but they still need real outputs from a concrete example. |
| 11. Estimate treatment effects only after an acceptable design has been achieved. | `In progress` | Estimation sections are included, but the code and results are still placeholders. |
| 12. Compare methods on transparency, feasibility, precision, and target population. | `In progress` | Comparison headings are in place, but the synthesis still needs completed analyses. |
| 13. Translate the methods into evaluator-facing guidance for programme and policy evaluation practice. | `In progress` | Guidance sections are outlined and reporting recommendations are partly drafted. |
| 14. Discuss limitations, sensitivity to design choices, and practical guidance. | `In progress` | Limitations are listed, but the full discussion remains to be written. |
| 15. Provide a reproducible appendix with package setup, references, and full code. | `In progress` | Package setup, glossary, and references are present; full runnable code and session information are still pending. |

## Proposed Package Stack

- `MatchIt`
- `WeightIt`
- `cobalt`
- `dplyr`
- `ggplot2`
- `knitr`

## Render the Report

This project is configured as a Quarto project in `docs/_quarto.yml`.

From the repository root, render the report with:

```bash
quarto render docs --to html
```

The rendered file is written to:

`docs/matching_methods_report.html`

If you only want to render this one document, run:

```bash
quarto render docs/matching_methods_report.qmd --to html
```

## Next Steps

- Add a concrete demonstration dataset, treatment definition, outcome, and target estimand.
- Replace commented code scaffolds with runnable chunks and render the resulting diagnostics and estimates.
- Finish the comparison, evaluator guidance, limitations, and appendix using outputs from the completed example.
