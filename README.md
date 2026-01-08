# FaaSr

<!-- badges: start -->
[![R-CMD-check](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FaaSr/FaaSr-Local/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/FaaSr)](https://CRAN.R-project.org/package=FaaSr)
<!-- badges: end -->

FaaSr provides a local execution environment for testing and developing FaaSr workflows without requiring cloud infrastructure. It enables you to run FaaSr-compatible workflows on your local machine with support for:

- Conditional branching (True/False paths)
- Parallel rank execution
- Workflow validation and cycle detection
- FaaSr-compatible file operations and logging

## Installation

You can install the development version of FaaSr from GitHub:

```r
# install.packages("devtools")
devtools::install_github("FaaSr/FaaSr-Local")
```

Once released on CRAN, you can install with:

```r
install.packages("FaaSr")
```

## Quick Start

### 1. Organize Your Project

FaaSr uses a `faasr_data/` directory structure for your workflows:

```r
library(FaaSr)

# Create project structure
dir.create("faasr_data/functions", recursive = TRUE)
dir.create("faasr_data/workflows", recursive = TRUE)
```

### 2. Create Your Functions

Store your custom functions in `faasr_data/functions/`:

```r
# Define a simple function
my_function <- function() {
  faasr_log("Hello from my function!")
  return(TRUE)
}

# Save your function to faasr_data/functions/
writeLines("my_function <- function() {
  faasr_log('Hello from my function!')
  return(TRUE)
}", "faasr_data/functions/my_function.R")
```

### 3. Create a Workflow JSON

Store your workflow definitions in `faasr_data/workflows/`:

```r
library(jsonlite)

workflow <- list(
  FunctionInvoke = "my-action",
  WorkflowName = "simple-workflow",
  DefaultDataStore = "LocalStorage",
  
  ComputeServers = list(
    LocalCompute = list(
      FaaSType = "GitHubActions",
      UserName = "test",
      ActionRepoName = "test-repo"
    )
  ),
  
  DataStores = list(
    LocalStorage = list(
      Bucket = "my-bucket"
    )
  ),
  
  ActionList = list(
    `my-action` = list(
      FunctionName = "my_function",
      FaaSServer = "LocalCompute",
      Type = "R",
      InvokeNext = list()
    )
  )
)

write_json(workflow, "faasr_data/workflows/my_workflow.json", auto_unbox = TRUE, pretty = TRUE)
```

### 4. Execute the Workflow

```r
# Run your workflow locally
result <- faasr_test("faasr_data/workflows/my_workflow.json")

# Check the logs
logs <- list.files("faasr_data/logs", pattern = "\\.log$", full.names = TRUE)
readLines(logs[1])
```

## Project Structure

When using FaaSr, organize your project like this:

```
my-faasr-project/
├── faasr_data/
│   ├── functions/          # Your custom R functions (sourced automatically)
│   │   ├── process.R
│   │   ├── analyze.R
│   │   └── ...
│   ├── workflows/          # Your workflow JSON files
│   │   ├── workflow1.json
│   │   ├── workflow2.json
│   │   └── ...
│   ├── files/              # File storage (created automatically)
│   │   └── ...
│   ├── logs/               # Execution logs (created automatically)
│   │   └── ...
│   └── temp/               # Temporary execution files (created automatically)
│       └── ...
└── ...
```

**Important**: 
- Place your **custom functions** in `faasr_data/functions/` - they will be sourced automatically
- Place your **workflow JSON files** in `faasr_data/workflows/` - for organization
- The `files/`, `logs/`, and `temp/` directories are created automatically during execution

## Core Functions

### File Operations

```r
# Upload a file
faasr_put_file(
  local_file = "data.csv",
  remote_folder = "input",
  remote_file = "data.csv"
)

# Download a file
faasr_get_file(
  remote_folder = "output",
  remote_file = "results.csv",
  local_file = "results.csv"
)

# List files
files <- faasr_get_folder_list(faasr_prefix = "input/")

# Delete a file
faasr_delete_file(
  remote_folder = "temp",
  remote_file = "temp_data.csv"
)
```

### Logging

```r
# Log messages during workflow execution
faasr_log("Processing started")
faasr_log(paste("Processing", nrow(data), "rows"))
```

### Rank Information

For parallel execution:

```r
rank_info <- faasr_rank()
if (length(rank_info) > 0) {
  cat("Rank:", rank_info$Rank, "of", rank_info$MaxRank, "\n")
}
```

## Conditional Branching

FaaSr supports conditional workflow paths based on function return values:

```r
ActionList = list(
  CheckData = list(
    FunctionName = "check_data",
    FaaSServer = "LocalCompute",
    Type = "R",
    InvokeNext = list(
      list(
        True = list("ProcessData"),
        False = list("HandleError")
      )
    )
  ),
  ProcessData = list(
    FunctionName = "process",
    FaaSServer = "LocalCompute",
    Type = "R",
    InvokeNext = list()
  ),
  HandleError = list(
    FunctionName = "handle_error",
    FaaSServer = "LocalCompute",
    Type = "R",
    InvokeNext = list()
  )
)
```

## Parallel Execution

Execute multiple ranks in parallel:

```r
ActionList = list(
  StartAction = list(
    FunctionName = "prepare",
    FaaSServer = "LocalCompute",
    Type = "R",
    InvokeNext = list("ParallelAction(3)")  # Run 3 parallel instances
  ),
  ParallelAction = list(
    FunctionName = "process_parallel",
    FaaSServer = "LocalCompute",
    Type = "R",
    InvokeNext = list()
  )
)
```

## Function Discovery

FaaSr automatically discovers and sources your functions in this priority order:

1. **`faasr_data/functions/`** - Your custom functions (recommended location)
2. **`R/`** - Functions in your package root
3. **`inst/extdata/functions/`** - Example functions from the FaaSr package

Place all your workflow functions in `faasr_data/functions/` for the cleanest organization.

## Local Data Storage

FaaSr stores data in a local `faasr_data/` directory:

- `faasr_data/functions/` - Your custom R functions (you create)
- `faasr_data/workflows/` - Your workflow JSON files (you create)
- `faasr_data/files/` - Uploaded/downloaded files (auto-created)
- `faasr_data/logs/` - Execution logs (auto-created)
- `faasr_data/temp/` - Temporary execution directories (auto-created)

You can customize the root directory with the `FAASR_DATA_ROOT` environment variable:

```r
Sys.setenv(FAASR_DATA_ROOT = "/custom/path/faasr_data")
```

## Workflow Validation

FaaSr automatically validates your workflow:

- JSON schema validation
- Cycle detection in workflow graph
- Required field verification
- Function existence checks

```r
# Validation happens automatically when you call faasr_test()
result <- faasr_test("workflow.json")
```

## Differences from FaaSr Cloud

| Feature | FaaSr (Cloud) | FaaSr (Local) |
|---------|---------------|---------------|
| Execution | Cloud (AWS Lambda, GitHub Actions, etc.) | Local machine |
| Storage | S3, MinIO, etc. | Local filesystem |
| Use Case | Production workflows | Development & testing |
| Dependencies | Cloud credentials required | None |

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

Apache License 2.0 - see [LICENSE](LICENSE) for details.

## Citation

If you use FaaSr in your research, please cite:

```bibtex
@software{faasr,
  title = {FaaSr: Local Execution Environment for FaaSr Workflows},
  author = {FaaSr Development Team},
  year = {2025},
  url = {https://github.com/FaaSr/FaaSr-Local}
}
```

