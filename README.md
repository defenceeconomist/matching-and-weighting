# Impact Evaluation Quasi-Experimental Design: Matching and Weighting Learning Resources

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
cd docs
quarto render
```

This renders:

- `docs/_site/index.html`
- `docs/_site/articles/matching_methods_report.html` (main article)
- `docs/_site/labs/index.html` and lab pages
- `docs/_site/labs/black_politicians_lab.html`
- `docs/_site/labs/black_politicians_lab_full_code.html`
- `docs/_site/labs/black_politicians_lab_evaluated_code.html`
- `docs/_site/labs/black_politicians_lab_qa_log.html`
- `docs/_site/labs/titanic_teaching_lab.html`
- `docs/_site/labs/titanic_teaching_lab_full_code.html`
- `docs/_site/slides/index.html` and slide pages
- `docs/_site/articles/matching_methods_report_full_code.html`
- `docs/_site/articles/matching_methods_report_qa_log.html`

If you only want the main article, run:

```bash
cd docs
quarto render articles/matching_methods_report.qmd
```

If you only want the slide deck, run:

```bash
cd docs
quarto render slides/matching_workshop.qmd
```

## GitLab CI Deployment

The repository includes a GitLab CI pipeline in `.gitlab-ci.yml` that:

- renders the Quarto site on pushes to `main`
- writes the built site into `docs/_site/`
- force-updates a deploy branch named `gh-pages` with the contents of `docs/_site/`

The deploy branch name can be changed with the `DEPLOY_BRANCH` CI/CD variable.

The deploy job tries to push with `CI_JOB_TOKEN` by default. If your GitLab project does not allow job-token writes to the repository, set these protected CI/CD variables instead:

- `DEPLOY_PUSH_USERNAME`
- `DEPLOY_PUSH_TOKEN`

After that, every push to `main` will rebuild the site and refresh the deploy branch automatically.

## GitHub Pages Deployment

The repository also includes a GitHub Actions workflow at `.github/workflows/gh-pages.yml`.

On every push to `main`, it:

- installs Quarto and R
- installs the site build dependencies
- renders the site from `docs/`
- publishes `docs/_site/` to the `gh-pages` branch

To use it on GitHub, enable GitHub Pages in the repository settings and choose `gh-pages` as the publishing branch.

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
  docs/articles/matching_methods_report.qmd \
  docs/articles/matching_methods_report_full_code.R
```

Optional third argument customizes the chunk separator format (default: `# ---- chunk %d ----`).

## Black Politicians Lab Outputs

The `black_politicians` training aid now includes four linked outputs:

- Training aid page: `docs/labs/black_politicians_lab.qmd` -> `docs/labs/black_politicians_lab.html`
- Full executable script and page:
  - `docs/labs/black_politicians_lab_full_code.R`
  - `docs/labs/black_politicians_lab_full_code.qmd` -> `docs/labs/black_politicians_lab_full_code.html`
- Evaluated script and page (shows processed data objects/results):
  - `docs/labs/black_politicians_lab_evaluated_code.R`
  - `docs/labs/black_politicians_lab_evaluated_code.qmd` -> `docs/labs/black_politicians_lab_evaluated_code.html`
- QA log against *The Effect* chapter workflow:
  - `docs/labs/black_politicians_lab_qa_log.qmd` -> `docs/labs/black_politicians_lab_qa_log.html`

Render these pages individually with:

```bash
quarto render docs/labs/black_politicians_lab.qmd --to html
quarto render docs/labs/black_politicians_lab_full_code.qmd --to html
quarto render docs/labs/black_politicians_lab_evaluated_code.qmd --to html
quarto render docs/labs/black_politicians_lab_qa_log.qmd --to html
```

## Next Steps

- Add hidden-bias sensitivity analysis beyond design diagnostics (e.g., Rosenbaum-style bounds or omitted-variable benchmarking).
- Tighten the slide-to-lab handoff with any remaining lecture refinements that emerge during delivery testing.

## Dataset Expansion Roadmap

The next dataset work should prioritize examples from *Causal Inference: The Mixtape* and *The Effect* that fit the current report's design-first framing: a binary treatment, a clearly defined outcome, enough pre-treatment covariates to make overlap and balance meaningful, and data that are easy to document and reproduce.

Status labels used below:

- `Done`: implemented and rendered in the current repo.
- `Next`: highest-priority implementation target for the report.
- `Later`: useful extension once the first expansion dataset is complete.

| Roadmap item | Status | Why this dataset fits |
| --- | --- | --- |
| 16. Add `causaldata::black_politicians` as the first non-`lalonde` worked example. | `Done` | Implemented as a training aid with linked full-code, evaluated-code, and QA-log outputs. The current implementation keeps the design-first comparison focus (exact/CEM/entropy balancing) and includes a chapter-alignment QA trace against *The Effect*. |
| 17. Add a benchmarking appendix using `causaldata::nsw_mixtape` plus `causaldata::cps_mixtape`. | `Done` | Implemented as a full lab bundle with linked lab, full-code, evaluated-code, and QA-log outputs. The workflow benchmarks observational estimates against the NSW experimental reference and reports benchmark gaps across raw, exact, CEM, and entropy-balancing designs. |
| 18. Add a short teaching appendix using `causaldata::titanic` for subclassification and simple exact matching. | `Done` | Implemented as a compact teaching lab with an executed narrative page and a linked full-code companion. The workflow focuses on visible support cells, exact matching on discrete covariates, and a simple subclassification comparison. |
| 19. Add slide outputs that bridge the main report into the lab bundle. | `Done` | The repo now includes a main report training deck plus a separate pre-lab background lecture with speaker notes, both rendered in `docs/slides/` and linked from the site navigation. |
| 20. Add sensitivity-analysis extensions beyond balance and retention diagnostics. | `Next` | This is the main remaining methods gap: the report currently explains design limitations clearly, but it does not yet include a formal hidden-bias or omitted-variable sensitivity workflow. |


### Planned sequence

1. Use the main report and slide decks as the teaching entry point: report deck first, then the pre-lab background lecture, then the labs.
2. Maintain `black_politicians` as the primary non-`lalonde` training example and keep chapter-alignment QA checks current as lab code evolves.
3. Keep the NSW/CPS benchmark lab aligned with the Mixtape objective as code evolves and rerun benchmark-gap QA after substantive method changes.
4. Keep `titanic` as a compact pedagogic appendix or slide-friendly note, not as the main substantive extension.
5. Prioritize sensitivity-analysis additions next so the methods workflow extends beyond observed-balance diagnostics.

## Fetch Processed EvidenceBase Data

The repository includes a reusable Docker Compose fetch pipeline for processed EvidenceBase documents stored in Redis and Qdrant.

Use the wrapper script when you want to fetch a file by collection and filename:

```bash
scripts/fetch_evidencebase_source.sh evaluation gertler_ch8.pdf
```

The command above writes JSON outputs to:

`data/evidencebase/evaluation/gertler_ch8/`

You can also call the compose service directly:

```bash
docker compose -f docker-compose.evidencebase-fetch.yml run --rm \
  evidencebase-fetch \
  --collection evaluation \
  --source gertler_ch8.pdf
```

By default, the compose file joins an external Docker network named `evidencebase_net` and expects:

- Redis at `evidencebase-redis:6379`, DB `2`
- Qdrant at `evidencebase-qdrant:6333`
- Qdrant collection naming like `evidencebase_<collection>`

If your local EvidenceBase stack uses a different external network name, override it when running. For example, on this machine the active network is `mcp-evidencebase_default`:

```bash
scripts/fetch_evidencebase_source.sh evaluation gertler_ch8.pdf mcp-evidencebase_default
```

Or:

```bash
EVIDENCEBASE_NETWORK=mcp-evidencebase_default \
docker compose -f docker-compose.evidencebase-fetch.yml run --rm \
  evidencebase-fetch \
  --collection evaluation \
  --source gertler_ch8.pdf
```

The fetcher also accepts a full source key:

```bash
docker compose -f docker-compose.evidencebase-fetch.yml run --rm \
  evidencebase-fetch \
  --source-key evaluation/gertler_ch8.pdf
```

Files produced per fetched source:

- `manifest.json`
- `source_meta.json`
- `document_meta.json`
- `document_sources.json`
- `partition.json`
- `sections.json`
- `chunks.json`
