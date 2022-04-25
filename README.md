This is repository for the data examination and model estimation for the
January-February 2021 study on predictors for behavioral change in the
contexts of the climate crisis and the COVID-19 pandemic.

This repository contains all materials to reproduce our analysis for the
paper '??'.

## Folder structure

-   `_targets` contains the files for the targets pipeline
    which enables a reproducible workflow.
-   `data` contains all data required for this project and code to
    handle data perparation, anonymization, and OSF up/downloads
-   `docs` contains the website data.
-   `figures` contains generated figures
-   `renv` contains information for libraries used
-   `R` contains scripts with helper functions
-   `reports` contains the Rmd reports for the iterative
    analysis and development of the models, as well as
    additional analysis and survey information for use on
    the website. The website is rendered from this folder.

## Instructions for reproducibility

This project uses `renv` for reproducibility. You can install the
libraries we used by calling `renv::restore()` from the R terminal.

The workflow up until model analysis uses `targets`.
