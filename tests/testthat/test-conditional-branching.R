# Test: Conditional Branching in Workflows
# Tests the conditional branching functionality defined in test_conditional_execution.json
# Verifies that True/False paths are taken correctly based on function return values

test_that("TRUE branch is taken when function returns TRUE", {
  # Get the workflow JSON path
  json_path <- file.path("..", "..", "inst", "extdata", "workflows", "test_conditional_execution.json")
  if (!file.exists(json_path)) {
    json_path <- system.file("extdata", "workflows", "test_conditional_execution.json", 
                             package = "FaaSr")
  }
  
  if (!file.exists(json_path)) {
    skip("test_conditional_execution.json not found")
  }
  
  # Clean up before test
  cleanup_faasr_data()
  
  on.exit({
    cleanup_faasr_data()
  })
  
  # Execute workflow
  result <- faasr_test(json_path)
  
  # Verify execution
  expect_true(result)
  
  # Verify log file was created
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  logs_dir <- file.path(faasr_data_dir, "logs")
  expect_true(dir.exists(logs_dir))
  
  # Check that log files exist
  log_files <- list.files(logs_dir, pattern = "\\.log$")
  expect_true(length(log_files) > 0)
  
  # Read log content to verify operations
  log_file <- file.path(logs_dir, log_files[1])
  log_content <- readLines(log_file)
  
  # Verify start-function ran and returned TRUE
  expect_true(any(grepl("Returning value: TRUE", log_content)))
  
  # Verify end-function (TRUE branch) was executed
  expect_true(sum(grepl("Returning value: TRUE", log_content)) >= 2)
  
  # Verify rank-function (FALSE branch) was NOT executed
  expect_false(any(grepl("Rank info:.*Executing rank", log_content)))
})

test_that("FALSE branch is taken when function returns FALSE", {
  # Create a modified workflow where start-function returns FALSE
  workflow <- list(
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
    ActionList = list(
      `start-function` = list(
        FunctionName = "return",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(value = FALSE),
        InvokeNext = list(list(True = list("end-function"), False = list("upload-file")))
      ),
      `end-function` = list(
        FunctionName = "return",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(value = TRUE),
        InvokeNext = list()
      ),
      `upload-file` = list(
        FunctionName = "upload",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(folder = "testing"),
        InvokeNext = list()
      )
    ),
    ActionContainers = list(
      `start-function` = "test-container",
      `end-function` = "test-container",
      `upload-file` = "test-container"
    ),
    FunctionGitRepo = list(
      `return` = "test/test-repo",
      upload = "test/test-repo"
    ),
    FunctionInvoke = "start-function",
    InvocationID = "TEST_INVOCATION_ID",
    FaaSrLog = "FaaSrLog",
    LoggingDataStore = "TestBucket",
    DefaultDataStore = "TestBucket",
    WorkflowName = "test-conditional-false"
  )
  
  # Write workflow to temp file
  json_path <- write_workflow_json(workflow)
  
  # Clean up before test
  cleanup_faasr_data()
  
  on.exit({
    unlink(json_path)
    cleanup_faasr_data()
  }, add = TRUE)
  
  # Define test return function that returns FALSE
  define_test_function("return", function(value = FALSE) {
    faasr_log(sprintf("Returning value: %s", value))
    value
  })
  
  on.exit({
    cleanup_test_functions(c("return"))
  }, add = TRUE)
  
  # Execute workflow
  result <- faasr_test(json_path)
  
  # Verify execution
  expect_true(result)
  
  # Verify log file was created
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  logs_dir <- file.path(faasr_data_dir, "logs")
  expect_true(dir.exists(logs_dir))
  
  # Check that log files exist
  log_files <- list.files(logs_dir, pattern = "\\.log$")
  expect_true(length(log_files) > 0)
  
  # Read log content to verify operations
  log_file <- file.path(logs_dir, log_files[1])
  log_content <- readLines(log_file)
  
  # Verify start-function ran and returned FALSE
  expect_true(any(grepl("Returning value: FALSE", log_content)))
  
  # Verify upload-file (FALSE branch) was executed
  expect_true(any(grepl("Listing before upload", log_content)))
  expect_true(any(grepl("Listing after upload", log_content)))
  
  # Verify start-function returned FALSE (taking FALSE branch)
  expect_true(any(grepl("Returning value: FALSE", log_content)))
})

test_that("conditional branching with both paths defined works", {
  # Create workflow with conditional that can take either path
  define_test_function("conditional_func", function(choice) {
    faasr_log(sprintf("Choice: %s", choice))
    return(choice)
  })
  
  define_test_function("true_path", function() {
    faasr_log("Executed TRUE path")
    TRUE
  })
  
  define_test_function("false_path", function() {
    faasr_log("Executed FALSE path")
    TRUE
  })
  
  workflow <- list(
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
    ActionList = list(
      StartAction = list(
        FunctionName = "conditional_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(choice = TRUE),
        InvokeNext = list(list(True = list("TrueAction"), False = list("FalseAction")))
      ),
      TrueAction = list(
        FunctionName = "true_path",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list()
      ),
      FalseAction = list(
        FunctionName = "false_path",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list()
      )
    ),
    ActionContainers = list(
      StartAction = "test-container",
      TrueAction = "test-container",
      FalseAction = "test-container"
    ),
    FunctionGitRepo = list(
      conditional_func = "test/test-repo",
      true_path = "test/test-repo",
      false_path = "test/test-repo"
    ),
    FunctionInvoke = "StartAction",
    InvocationID = "TEST_INVOCATION_ID",
    FaaSrLog = "FaaSrLog",
    LoggingDataStore = "TestBucket",
    DefaultDataStore = "TestBucket",
    WorkflowName = "test-both-branches"
  )
  
  json_path <- write_workflow_json(workflow)
  cleanup_faasr_data()
  
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("conditional_func", "true_path", "false_path"))
    cleanup_faasr_data()
  }, add = TRUE)
  
  # Execute workflow (default returns TRUE)
  result <- faasr_test(json_path)
  expect_true(result)
  
  # Verify TRUE path was taken
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  logs_dir <- file.path(faasr_data_dir, "logs")
  log_files <- list.files(logs_dir, pattern = "\\.log$")
  log_file <- file.path(logs_dir, log_files[1])
  log_content <- readLines(log_file)
  
  expect_true(any(grepl("Executed TRUE path", log_content)))
  expect_false(any(grepl("Executed FALSE path", log_content)))
})

test_that("conditional with only True branch works", {
  define_test_function("cond_true_only", function() {
    faasr_log("Condition evaluated")
    TRUE
  })
  
  define_test_function("after_true", function() {
    faasr_log("After TRUE executed")
    TRUE
  })
  
  workflow <- list(
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
    ActionList = list(
      StartAction = list(
        FunctionName = "cond_true_only",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list(list(True = list("AfterTrue")))
      ),
      AfterTrue = list(
        FunctionName = "after_true",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list()
      )
    ),
    ActionContainers = list(
      StartAction = "test-container",
      AfterTrue = "test-container"
    ),
    FunctionGitRepo = list(
      cond_true_only = "test/test-repo",
      after_true = "test/test-repo"
    ),
    FunctionInvoke = "StartAction",
    InvocationID = "TEST_INVOCATION_ID",
    FaaSrLog = "FaaSrLog",
    LoggingDataStore = "TestBucket",
    DefaultDataStore = "TestBucket",
    WorkflowName = "test-true-only"
  )
  
  json_path <- write_workflow_json(workflow)
  cleanup_faasr_data()
  
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("cond_true_only", "after_true"))
    cleanup_faasr_data()
  }, add = TRUE)
  
  result <- faasr_test(json_path)
  expect_true(result)
  
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  logs_dir <- file.path(faasr_data_dir, "logs")
  log_files <- list.files(logs_dir, pattern = "\\.log$")
  log_file <- file.path(logs_dir, log_files[1])
  log_content <- readLines(log_file)
  
  expect_true(any(grepl("Condition evaluated", log_content)))
  expect_true(any(grepl("After TRUE executed", log_content)))
})

test_that("conditional with only False branch works", {
  define_test_function("cond_false_only", function() {
    faasr_log("Condition evaluated to FALSE")
    FALSE
  })
  
  define_test_function("after_false", function() {
    faasr_log("After FALSE executed")
    TRUE
  })
  
  workflow <- list(
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
    ActionList = list(
      StartAction = list(
        FunctionName = "cond_false_only",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list(list(False = list("AfterFalse")))
      ),
      AfterFalse = list(
        FunctionName = "after_false",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list()
      )
    ),
    ActionContainers = list(
      StartAction = "test-container",
      AfterFalse = "test-container"
    ),
    FunctionGitRepo = list(
      cond_false_only = "test/test-repo",
      after_false = "test/test-repo"
    ),
    FunctionInvoke = "StartAction",
    InvocationID = "TEST_INVOCATION_ID",
    FaaSrLog = "FaaSrLog",
    LoggingDataStore = "TestBucket",
    DefaultDataStore = "TestBucket",
    WorkflowName = "test-false-only"
  )
  
  json_path <- write_workflow_json(workflow)
  cleanup_faasr_data()
  
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("cond_false_only", "after_false"))
    cleanup_faasr_data()
  }, add = TRUE)
  
  result <- faasr_test(json_path)
  expect_true(result)
  
  faasr_data_dir <- file.path(getwd(), "faasr_data")
  logs_dir <- file.path(faasr_data_dir, "logs")
  log_files <- list.files(logs_dir, pattern = "\\.log$")
  log_file <- file.path(logs_dir, log_files[1])
  log_content <- readLines(log_file)
  
  expect_true(any(grepl("Condition evaluated to FALSE", log_content)))
  expect_true(any(grepl("After FALSE executed", log_content)))
})

