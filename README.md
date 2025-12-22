# FaaSrLocal

<!-- badges: start -->
[![R-CMD-check](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/FaaSrLocal)](https://CRAN.R-project.org/package=FaaSrLocal)
<!-- badges: end -->

FaaSrLocal provides a local execution environment for testing and developing FaaSr workflows without requiring cloud infrastructure. It enables you to run FaaSr-compatible workflows on your local machine with support for:
- Conditional branching (True/False paths)
- Parallel rank execution
- Workflow validation and cycle detection
- FaaSr-compatible file operations and logging

## Installation

You can install the development version of FaaSrLocal from GitHub:

```r
# install.packages("devtools")
devtools::install_github("FaaSr/FaaSr-Local")
```

Or from CRAN (once available):

```r
install.packages("FaaSrLocal")
```

## Quick Start

### 1. Create Your Workflow Functions

Create R functions that will be executed in your workflow:
```r
# my_functions.R
create_data <- function() {
  data <- data.frame(x = 1:10, y = rnorm(10))
  write.csv(data, "data.csv", row.names = FALSE)
  faasr_put_file(local_file = "data.csv", remote_file = "data.csv")
}

process_data <- function() {
  faasr_get_file(remote_file = "data.csv", local_file = "data.csv")
  data <- read.csv("data.csv")
  result <- sum(data$y)
  faasr_log(paste("Sum:", result))
}
```

### 2. Define Your Workflow JSON

Create a workflow definition in JSON format:
```json
{
  "FunctionInvoke": "CreateData",
  "ActionList": {
    "CreateData": {
      "FunctionName": "create_data",
      "InvokeNext": ["ProcessData"]
    },
    "ProcessData": {
      "FunctionName": "process_data"
    }
  }
}
```

### 3. Run the Workflow

```r
library(FaaSrLocal)

# Source your functions
source("my_functions.R")

# Run the workflow
faasr_test("workflow.json")
```

## Features

### File Operations

FaaSrLocal provides FaaSr-compatible file operations for local storage:

```r
# Upload a file to local storage
faasr_put_file(local_file = "data.csv", remote_file = "output/data.csv")

# Download a file from local storage
faasr_get_file(remote_file = "output/data.csv", local_file = "local_data.csv")

# Delete a file from local storage
faasr_delete_file(remote_file = "output/data.csv")

# List files in storage
files <- faasr_get_folder_list(faasr_prefix = "output/")
```

### Logging

```r
faasr_log("Processing step completed")
```

### Parallel Execution (Ranks)

Define parallel execution using rank notation in `InvokeNext`:
```json
{
  "InvokeNext": ["ParallelTask(3)"]
}
```

Access rank information in your function:
```r
rank_info <- faasr_rank()
# rank_info$Rank = "1", "2", or "3"
# rank_info$MaxRank = "3"
```

### Conditional Branching

Define conditional branches based on function return values:
```json
{
  "InvokeNext": [
    {"True": ["SuccessPath"], "False": ["FailurePath"]}
  ]
}
```

Your function should return `TRUE` or `FALSE` to control the branch taken.

## Workflow Validation

FaaSrLocal automatically validates your workflow:
- **Cycle Detection**: Ensures the workflow is a valid DAG (no cycles)
- **Schema Validation**: Validates JSON structure (if `jsonvalidate` package is available)
- **Predecessor Consistency**: Ensures consistent predecessor types

## Environment Variables

- `FAASR_DATA_ROOT`: Override the default data storage directory (defaults to `faasr_data` in the current working directory)

## Differences from FaaSr

| Feature | FaaSr | FaaSrLocal |
|---------|-------|------------|
| Execution | Cloud (AWS Lambda, GitHub Actions, etc.) | Local machine |
| Storage | S3, MinIO, etc. | Local filesystem |
| Use Case | Production workflows | Development & testing |
| Dependencies | Cloud credentials required | None |

## License

Apache License 2.0

