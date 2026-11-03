[![R version](https://img.shields.io/badge/R-4.6.1-blue.svg)](https://cran.r-project.org/)
[![Reproducible Environment: renv](https://img.shields.io/badge/reproducible%20environment-renv-lightgreen.svg)](https://rstudio.github.io/renv/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-orange.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Formatted with air v0.7.1](https://img.shields.io/badge/formatted%20with-air%200.7.1-purple)](https://github.com/posit-dev/air)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io/shafayetshafee/within--group--gcomp-blue?logo=docker)](https://ghcr.io/shafayetshafee/within-group-gcomp)
[![Workflow](https://github.com/shafayetShafee/within-group-gcomp/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/shafayetShafee/within-group-gcomp/actions/workflows/docker-publish.yml)
[![DOI](https://img.shields.io/badge/DOI-10.6084%2Fm9.figshare.32676144-cyan)](https://doi.org/10.6084/m9.figshare.32676144)


> [!NOTE]
> This repository contains the data and R code necessary to replicate the findings
> of the simulation and real data application associated with the preprint manuscript,
>
> > Shafee, S. K., Sarker, B., & Sium, M. N. I. (2026). G-computation for causal 
> effect estimation from observational hierarchical data with unmeasured cluster 
> context. arXiv [Stat.ME]. Retrieved from http://arxiv.org/abs/2606.14131.


## Directories & files descriptions

```yaml
├── analysis-figures/        # Figures generated from the real-data application
|
├── create_rprofile.sh       # Configures .Rprofile inside the Docker container
├── docker-compose-dev.yml   # Compose setup for local development
├── docker-compose.yml       # Compose setup for reproducing the analysis
├── Dockerfile               # Docker image build configuration
|
├── LICENSE.md               # CCBY 4.0 License
├── mics_raw_data/           # Raw MICS survey data files (child and women modules)
|
├── R/                       # R scripts for simulations, application, and functions
│   ├── functions/           # Reusable helper functions shared across scripts
│   ├── haz-erp-application/ # Scripts for the real-data application analysis
│   ├── renv-clear.R         # Lists extra packages to retain during renv cleanup
│   ├── simulations/         # Simulation scripts organized by scenario
│   └── simulations-result/  # Processed simulation outputs for summary and figures
|
├── README.md                # Project documentation
├── renv/                    # Project-local package library managed by renv
├── renv.lock                # Exact package versions for reproducibility
├── system-deps.md           # System library to R package dependency mapping
|
├── simulation-figures/      # Figures generated from simulation study results
└── within-group-gcomp.Rproj # RStudio project file
```
