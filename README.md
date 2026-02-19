
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FaaSr-Local <img src='man/figures/FaaSr.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/FaaSr)](https://cran.r-project.org/package=FaaSr)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.07027/status.svg)](https://doi.org/10.21105/joss.07027)

<!-- badges: end -->

## Overview

**FaaSr-Local** is the local testing and development environment for
[FaaSr](https://faasr.io) workflows. As of version 2.0.0, the core
backend execution logic has been migrated to
[FaaSr-Backend](https://github.com/FaaSr/FaaSr-Backend), which handles
production deployment across GitHub Actions, AWS Lambda, OpenWhisk,
Google Cloud, and SLURM. Use this package to develop and validate your
workflows locally before deploying to any cloud FaaS platform.

FaaSr-Local enables you to:

- **Test workflows locally** - Validate your FaaSr workflows before
  cloud deployment
- **No cloud required** - Run everything on your local machine without
  any cloud credentials or accounts
- **Simulated S3 storage** - File operations
  (upload/download/delete/list) work with local filesystem
- **Support advanced features** - Conditional branching (True/False
  paths), parallel rank execution, and more
- **Validate workflows** - JSON schema always fetched fresh from
  [FaaSr-Backend](https://github.com/FaaSr/FaaSr-Backend) for
  up-to-date compliance checking and cycle detection
- **Debug easily** - Local logging for monitoring and troubleshooting

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("FaaSr/FaaSr-Local")
```

## Quick Start

### 1. Create Your Project Structure

FaaSr-Local automatically creates a `faasr_data/` directory to simulate
cloud infrastructure:

``` r
library(FaaSr)

# Directory structure (created automatically):
# your-project/
# ├── workflow.json              # Your FaaSr workflow configuration
# ├── faasr_data/                # Created by faasr_test()
# │   ├── functions/             # (Optional) User R functions
# │   ├── files/                 # Simulated S3 storage
# │   ├── logs/                  # Log files
# │   └── temp/                  # Temporary execution files
# └── my_functions.R             # (Optional) Your R functions
```

### 2. Test Your Workflow

``` r
# Execute your FaaSr workflow locally
faasr_test("path/to/workflow.json")
```

That's it! Your workflow runs locally with full support for:

- Conditional branching (True/False paths)
- Parallel rank execution
- File operations (simulated S3)
- Logging and monitoring

## Production Deployment

Once your workflow is validated locally, deploy it to the cloud using
the [FaaSr-Backend](https://github.com/FaaSr/FaaSr-Backend), which
supports GitHub Actions, AWS Lambda, OpenWhisk, Google Cloud, and SLURM.

For more information, visit <https://faasr.io>
