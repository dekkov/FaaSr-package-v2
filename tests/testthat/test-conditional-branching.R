# Test: Conditional Branching in Workflows
# Tests the conditional branching functionality defined in test_conditional_execution.json
# Verifies that True/False paths are taken correctly based on function return values

test_that("TRUE branch is taken when function returns TRUE", {
  # Get the workflow JSON path
  json_path <- file.path("..", "..", "inst", "extdata", "workflows", "test_conditional_execution_true.json")
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
  # Get the workflow JSON path for FALSE branch test
  json_path <- file.path("..", "..", "inst", "extdata", "workflows", "test_conditional_execution_false.json")
  if (!file.exists(json_path)) {
    json_path <- system.file("extdata", "workflows", "test_conditional_execution_false.json", 
                             package = "FaaSr")
  }
  
  if (!file.exists(json_path)) {
    skip("test_conditional_execution_false.json not found")
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
  
  # Verify start-function ran and returned FALSE
  expect_true(any(grepl("Returning value: FALSE", log_content)))
  
  # Verify rank-function (FALSE branch) was executed (with 3 ranks)
  expect_true(any(grepl("Rank info:.*Executing rank", log_content)))
  expect_true(any(grepl("1/3", log_content)))
  expect_true(any(grepl("2/3", log_content)))
  expect_true(any(grepl("3/3", log_content)))
  
})

