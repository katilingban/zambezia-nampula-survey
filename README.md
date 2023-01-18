
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Improving Nutrition Status for under 5 children in Zambezia and Nampula project baseline and endline survey data analysis and reporting workflow

<!-- badges: start -->

[![test analysis
workflow](https://github.com/katilingban/zambezia-nampula-survey/actions/workflows/test-analysis-workflow.yaml/badge.svg)](https://github.com/katilingban/zambezia-nampula-survey/actions/workflows/test-analysis-workflow.yaml)
<!-- badges: end -->

This repository is a
[`docker`](https://www.docker.com/get-started)-containerised,
[`{targets}`](https://docs.ropensci.org/targets/)-based,
[`{renv}`](https://rstudio.github.io/renv/articles/renv.html)-enabled
[`R`](https://cran.r-project.org/) workflow developed for the data
management, data analysis, and reporting of the **Improving Nutrition
Status for under 5 Children in Zambezia and Nampula project baseline and
endline survey**.

## Repository Structure

- `data/` contains intermediate data outputs produced by the workflow
  including an endline survey codebook describing all variables of the
  endline survey;
- `R/` contains functions created for use in this workflow;
- `reports/` contains literate code for R Markdown reports generated in
  the workflow;
- `outputs/` contains compiled reports and figures;
- `auth/` contains credentials for Google service account (see below for
  more information);
- `docs/` contains archived high frequency data checks reports produced
  during the implementation of the survey;
- `did/` contains R script for performing the difference-in-difference
  analysis using the results tables produced by the analysis
  pipeline/workflow;
- `_targets.R` file defines the steps in this workflow’s data management
  and analysis pipeline.

## Reproducibility

### R package dependencies

This project requires `R 4.2.2 (2022-11-10 r83330)`. This project uses
the `{renv}` framework to record R package dependencies and versions.
Packages and versions used are recorded in `renv.lock` and code used to
manage dependencies is in `renv/` and other files in the root project
directory. On starting an R session in the working directory,
`run renv::restore()` to install R package dependencies.

### System requirements

This project’s data management and analysis pipeline was built to
utilise multi-threaded parallel computing to make calculations faster
and more efficient. This project’s pipeline uses 4 computing threads. To
run without issues, ensure that you use a machine with at least 5
computing threads when reproducing this pipeline (4 threads for the
pipeline and the remaininig threads to run the machines regular
processes).

With multi-threaded computation, the entire pipeline completes between
30 minutes to an hour (depending on CPU speed and RAM size). Without
multi-threading, the entire pipeline completes between 3-5 hours.

### Data management and analysis

This project uses the `{targets}` package to create its data management
and analysis pipeline as defined in the `_targets.R` file.

To execute the data management and processing workflow for baseline and
endline survey data, run:

``` r
targets::tar_make(baseline_data_weighted)
targets::tar_make(endline_data_weighted)
```

The schematic figure below summarises the steps in the data management
and processing workflow:

✔ Successfully auto-authenticated via
auth/mozambique-s3m-e9da207bc2a3.json

``` mermaid
graph LR
  subgraph Graph
    xaa8b198e46c12205(["baseline_data_downloads"]):::uptodate --> x34303849e9b0482a(["baseline_raw_data_spss"]):::uptodate
    x3292f12efa6fc224(["endline_raw_data"]):::uptodate --> x9fee25520ed83a1a(["endline_sample_weight"]):::uptodate
    xa54f854256062899(["survey_sampling_list_endline"]):::uptodate --> x9fee25520ed83a1a(["endline_sample_weight"]):::uptodate
    x8c16f11d9b877f97(["endline_data_processed"]):::uptodate --> x1a4d1d913b004605(["endline_data_weighted"]):::uptodate
    x9fee25520ed83a1a(["endline_sample_weight"]):::uptodate --> x1a4d1d913b004605(["endline_data_weighted"]):::uptodate
    xcbf2ec7505f9fcf3(["baseline_raw_data_stata"]):::uptodate --> xaa47d78bda713fd1(["baseline_sample_weight"]):::uptodate
    x500c8b308ead6406(["survey_sampling_list"]):::uptodate --> xaa47d78bda713fd1(["baseline_sample_weight"]):::uptodate
    x500c8b308ead6406(["survey_sampling_list"]):::uptodate --> xa54f854256062899(["survey_sampling_list_endline"]):::uptodate
    xaa8b198e46c12205(["baseline_data_downloads"]):::uptodate --> xcbf2ec7505f9fcf3(["baseline_raw_data_stata"]):::uptodate
    x3292f12efa6fc224(["endline_raw_data"]):::uptodate --> x8c16f11d9b877f97(["endline_data_processed"]):::uptodate
    xb093acd1bf068c99(["survey_endline_choices"]):::uptodate --> x8c16f11d9b877f97(["endline_data_processed"]):::uptodate
    x34303849e9b0482a(["baseline_raw_data_spss"]):::uptodate --> x3fb2eec55e3093c2(["baseline_data_processed"]):::uptodate
    xcbf2ec7505f9fcf3(["baseline_raw_data_stata"]):::uptodate --> x3fb2eec55e3093c2(["baseline_data_processed"]):::uptodate
    x13d5233859395ad9(["survey_endline_form_id"]):::uptodate --> xb093acd1bf068c99(["survey_endline_choices"]):::uptodate
    x3fb2eec55e3093c2(["baseline_data_processed"]):::uptodate --> xcbc9dbd4b57c3d95(["baseline_data_weighted"]):::uptodate
    xaa47d78bda713fd1(["baseline_sample_weight"]):::uptodate --> xcbc9dbd4b57c3d95(["baseline_data_weighted"]):::uptodate
    x2ada552bbfadb16c(["survey_sampling_list_download"]):::uptodate --> x500c8b308ead6406(["survey_sampling_list"]):::uptodate
    x500c8b308ead6406(["survey_sampling_list"]):::uptodate --> x3292f12efa6fc224(["endline_raw_data"]):::uptodate
    xa54f854256062899(["survey_sampling_list_endline"]):::uptodate --> x3292f12efa6fc224(["endline_raw_data"]):::uptodate
  end
  classDef uptodate stroke:#000000,color:#ffffff,fill:#354823;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
```

This produces the following outputs:

- R object for processed baseline data that includes probability weights
  for use in weighted analysis;
- R object for processed endline data that includes probability weights
  for use in weighted analysis.

To execute the data analysis workflow for the baseline and endline
survey data, run:

``` r
targets::tar_make(dplyr::starts_with("baseline"))
targets::tar_make(dplyr::starts_with("endline"))
```

The schematic figure below summarises the steps in the data analysis
workflow:

✔ Successfully auto-authenticated via
auth/mozambique-s3m-e9da207bc2a3.json

``` mermaid
graph LR
  subgraph Graph
    x2ae00276a0a5b418(["overall_results_all"]):::outdated --> x0bbe9f288492eeec(["overall_results_xlsx"]):::outdated
    xb8de1d18ad2cc4f6(["baseline_results_all"]):::outdated --> xaa3f6c085cefc626(["baseline_results_csv"]):::outdated
    xa8885ae938ad1519(["overall_results_subset"]):::outdated --> x14c4c0c6cf2ca972(["baseline_results_subset"]):::outdated
    x2ae00276a0a5b418(["overall_results_all"]):::outdated --> x02fbd9452a16eea6(["baseline_survey_results_report"]):::outdated
    xcf2fba5ab534b33c(["endline_results_subset"]):::outdated --> x6ddc0eabdf53e510(["endline_results_subset_csv"]):::outdated
    x14c4c0c6cf2ca972(["baseline_results_subset"]):::outdated --> x969d7dd50bd8a411(["baseline_results_subset_csv"]):::outdated
    xa8885ae938ad1519(["overall_results_subset"]):::outdated --> xcf2fba5ab534b33c(["endline_results_subset"]):::outdated
    x2ae00276a0a5b418(["overall_results_all"]):::outdated --> xcd1b6eeb6bb99aba(["endline_survey_results_report"]):::outdated
    x2ae00276a0a5b418(["overall_results_all"]):::outdated --> xdf25b6550ed0d67d(["endline_results_all"]):::outdated
    xb8de1d18ad2cc4f6(["baseline_results_all"]):::outdated --> xe30152fe1d53bcbe(["baseline_results_xlsx"]):::outdated
    xdf25b6550ed0d67d(["endline_results_all"]):::outdated --> xa5c7be8027626ce4(["endline_results_csv"]):::outdated
    xdf25b6550ed0d67d(["endline_results_all"]):::outdated --> x87d5a544c4ad6d2e(["endline_results_xlsx"]):::outdated
    x2ae00276a0a5b418(["overall_results_all"]):::outdated --> xb8de1d18ad2cc4f6(["baseline_results_all"]):::outdated
    x26387a2c59ebbcd2(["endline_survey_food_security_results_report"]):::outdated --> x26387a2c59ebbcd2(["endline_survey_food_security_results_report"]):::outdated
  end
  classDef outdated stroke:#000000,color:#000000,fill:#78B7C5;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
```

This pipeline produces the following outputs:

- Baseline results tables in CSV and in XLSX format;
- Baseline results tables subset in CSV format which is used for the
  difference-in-difference analysis;
- Endline results tables in CSV and in XLSX format;
- Endline results tables subset in CSV format which is used for the
  difference-in-difference analysis.

These outputs are saved by the pipeline in the `outputs` directory with
the following filenames:

- `baseline_results.csv`;
- `baseline_results.xlsx`;
- `baseline_results_subset.csv`;
- `endline_results.csv`;
- `endline_results.xlsx`;
- `endline_results_subset.csv`.

### Difference-in-difference analysis

The **difference-in-difference** analysis was performed using a single
`R` script found in the `did/` directory.

To run the **difference-in-difference** analysis, one should first run
the data analysis pipeline describe above using the following command in
R:

    targets::tar_make(dplyr::starts_with("baseline"))
    targets::tar_make(dplyr::starts_with("endline"))

which will produce the necessary results outputs needed for the
**difference-in-difference** analysis.

Then, one should run the **difference-in-difference** R script using the
following R command:

    source("did/did.R")

The outputs of the **difference-in-difference** R script are:

- an HTML file called `analysisDiD.html`;
- all the plots (in `.png` format) showing the
  **difference-in-difference** results for each indicator on which the
  analysis was performed.

These outputs are saved in the `did/` directory after the
**difference-in-difference** R script is run.

This repository doesn’t contain the outputs of data management and
analysis workflows described above. The user is expected to use this
code along with the data from **UNICEF Mozambique** (restricted; request
access only; see below) to run the workflows and produce the outputs
themselves. All outputs are available from **UNICEF Mozambique** on
request.

## Encryption

This repository uses [`git-crypt`](https://github.com/AGWA/git-crypt) to
enable transparent encryption and decryption of the `.env` file and the
`auth/` directory.

The `.env` file contains:

- variables for authenticating with Google Cloud services account setup
  for this project for accessing the baseline survey data;

- variables for accessing the endline survey data direct from ONA;

- variables for `SMTP_PASSWORD` and `EMAIL_RECIPIENTS` which are used
  for sending the email updates for the high frequency data checks.

The `auth/` directory contains credentials for Google service account.

Those who would like to reproduce the results of this project will
require ability to decrypt the `.env` file and the `auth/` directory.

To be able to work on this repository, a user/collaborator on this
project will need to:

- Create their own **PGP (Pretty Good Privacy) public and private
  keys**; and,

- Share their public key to the authors and request for it to be added
  to the repository.

Once added, a collaborator can now decrypt the `.env` file and the
`auth/` directory after pulling/cloning the repository by running:

    git-crypt unlock

on the terminal.

## Authors

- Mark Myatt
- Ernest Guevarra

## License

The datasets for both baseline and endline survey (not included in this
repository) are owned by **UNICEF Mozambique**. Access to these datasets
is restricted. Communicate with **UNICEF Mozambique** to request access
to the survey datasets.

The code included in this repository is owned by the authors and is
licensed under a [GNU General Public License 3
(GPL-3)](https://opensource.org/licenses/GPL-3.0).

## Feedback

Feedback, bug reports and feature requests are welcome; file issues or
seek support
[here](https://github.com/katilingban/zambezia-nampula-survey/issues).
