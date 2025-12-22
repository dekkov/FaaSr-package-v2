# Helper functions for workflow tests

# Create a minimal valid workflow JSON for testing
create_minimal_workflow <- function(action_list, function_invoke = names(action_list)[1]) {
  list(
    ComputeServers = list(
      TestServer = list(
        FaaSType = "GitHubActions",
        UserName = "test",
        UseSecretStore = FALSE,
        ActionRepoName = "test-repo",
        Branch = "main",
        Token = "TEST_TOKEN"
      )
    ),
    DataStores = list(
      TestBucket = list(
        Bucket = "test",
        Endpoint = "https://test.endpoint",
        Region = "us-east-1",
        Writable = "TRUE",
        AccessKey = "TEST_ACCESS_KEY",
        SecretKey = "TEST_SECRET_KEY"
      )
    ),
    ActionList = action_list,
    ActionContainers = setNames(
      lapply(names(action_list), function(x) "test-container"),
      names(action_list)
    ),
    FunctionGitRepo = list(),
    FunctionInvoke = function_invoke,
    InvocationID = "TEST_INVOCATION_ID",
    FaaSrLog = "FaaSrLog",
    LoggingDataStore = "TestBucket",
    DefaultDataStore = "TestBucket",
    WorkflowName = "test-workflow"
  )
}

# Write workflow JSON to a temp file
write_workflow_json <- function(workflow, filename = NULL) {
  if (is.null(filename)) {
    filename <- tempfile(fileext = ".json")
  }
  jsonlite::write_json(workflow, filename, auto_unbox = TRUE, pretty = TRUE)
  filename
}

# Create a simple test function in the global environment
define_test_function <- function(name, body_fn) {
  assign(name, body_fn, envir = .GlobalEnv)
}

# Clean up global environment after tests
cleanup_test_functions <- function(names) {
  for (name in names) {
    if (exists(name, envir = .GlobalEnv)) {
      rm(list = name, envir = .GlobalEnv)
    }
  }
}

# Clean up faasr_data directory after tests
cleanup_faasr_data <- function() {
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  if (dir.exists(faasr_data_dir)) {
    unlink(faasr_data_dir, recursive = TRUE)
  }
}

