# Test: Parallel Rank Execution
# Tests the workflow defined in test_rank_execution.json:
# start-function -> rank-function(3) - executes rank function with 3 parallel ranks
# Verifies that rank information is correctly logged

test_that("parallel rank execution workflow executes correctly with rank logging", {
  # Get the workflow JSON path
  # Try source tree first (tests/testthat/ is working dir when running tests)
  json_path <- file.path("..", "..", "inst", "extdata", "workflows", "test_rank_execution.json")
  if (!file.exists(json_path)) {
    # Fall back to installed package location
    json_path <- system.file("extdata", "workflows", "test_rank_execution.json", 
                             package = "FaaSr")
  }
  
  # Skip if workflow file doesn't exist
  if (!file.exists(json_path)) {
    skip("test_rank_execution.json not found")
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
  
  # Find log lines for each rank execution
  line_rank_1 <- which(grepl("Rank info:.*Executing rank 1/3", log_content))
  line_rank_2 <- which(grepl("Rank info:.*Executing rank 2/3", log_content))
  line_rank_3 <- which(grepl("Rank info:.*Executing rank 3/3", log_content))
  
  # Verify all expected rank log messages exist
  expect_true(length(line_rank_1) > 0)
  expect_true(length(line_rank_2) > 0)
  expect_true(length(line_rank_3) > 0)
  
  # Verify that all ranks are logged
  expect_true(any(grepl("Rank info:.*Executing rank 1/3", log_content)))
  expect_true(any(grepl("Rank info:.*Executing rank 2/3", log_content)))
  expect_true(any(grepl("Rank info:.*Executing rank 3/3", log_content)))
  
  # Verify the upload function ran first
  expect_true(any(grepl("Listing before upload.*testing", log_content)))
  expect_true(any(grepl("Listing after upload.*testing", log_content)))
})

