[![R version](https://img.shields.io/badge/R-4.6.1-blue.svg)](https://cran.r-project.org/)
[![Reproducible Environment: renv](https://img.shields.io/badge/reproducible%20environment-renv-lightgreen.svg)](https://rstudio.github.io/renv/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-orange.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Formatted with air v0.13.0](https://img.shields.io/badge/formatted%20with-air%200.13.0-purple)](https://github.com/posit-dev/air)

> [!NOTE]
> This repository contains the data and R code necessary to replicate the findings
> of the simulation associated with the preprint manuscript,
>
> > Sium, M. N. I., Shafee, S. K., Patwary, M. H., & Sarker, B. (2026). Quantifying 
> Inconsistent Mediation: The Mediated Magnitude Share and the Degree of Suppression. 
> arXiv preprint arXiv:2609.33846 [Stat.ME]. Retrieved from https://arxiv.org/abs/2609.33846.


## Directories & files descriptions

```yaml
├── create_rprofile.sh                         # Configures .Rprofile inside the Docker container
├── docker-compose-dev.yml                     # Compose setup for local development
├── docker-compose.yml                         # Compose setup for reproducing the analysis
├── Dockerfile                                 # Docker image build configuration
|
├── figures/                                   # Manuscript figures in EPS, PDF, and TIFF formats
│   ├── Figure-1.{eps,pdf,tiff}                # Figure 1
│   └── Figure-2.{eps,pdf,tiff}                # Figure 2
|
├── LICENSE.md                                 # CCBY 4.0 License
├── mms_sigma.Rproj                            # RStudio project file
|
├── R/                                         # R scripts for simulations, figures, and helper functions
│   ├── figures.R                              # Generates the manuscript figures from simulation results
│   ├── simulations/                           # Simulation scripts organized by scenario
│   │   ├── binary-outcome-two-pathway-case.R  # Binary outcome with two mediation pathways
│   │   ├── multiple-pathways-case.R           # Multiple mediation pathways
│   │   └── two-pathway-case.R                 # Continuous outcome with two mediation pathways
│   └── utils.R                                # Reusable helper functions shared across scripts
|
├── README.md                                  # Project documentation
├── renv/                                      # Project-local package library managed by renv
├── renv.lock                                  # Exact package versions for reproducibility
|
└── tables/                                    # Simulation summary and LaTeX tables stored as .rds
    ├── binary-outcome-*-table.rds             # Binary outcome case (summary and LaTeX)
    ├── multiple-pathway-*-table.rds           # Multiple pathways case (summary and LaTeX)
    └── two-pathway-*-table.rds                # Two-pathway case (summary and LaTeX)
```


## Reproducing the simulation results using Docker

**Prerequisite:** [Docker](https://docs.docker.com/get-docker/) installed on your system.

**Steps:**

1. Pull and run the container using the pre-built image from GitHub Container Registry:

   ```bash
   docker run -d -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true \
      --name mms_sigma ghcr.io/shafayetshafee/mms_sigma:0.0.9
   ```

   > This image includes the correct R version, RStudio Server, the simulation
   code, and all R package dependencies exactly as used in the study. The port is
   bound to `localhost` only, because authentication is disabled.

   Alternatively, clone this repository and use Docker Compose, which also saves
   the generated figures and tables to your machine:

   ```bash
   docker compose up -d
   ```

2. Open your browser and go to `http://localhost:8787`.

3. You will be connected to an RStudio Server session with the project pre-loaded
   and all dependencies installed. Run the simulation scripts in `R/simulations/`
   and `R/figures.R` directly from within RStudio Server.

4. To stop the container when done:

   ```bash
   docker rm -f mms_sigma      # if started with docker run
   docker compose down         # if started with docker compose
   ```

