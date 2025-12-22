# Test: File Upload/Download/Delete Workflow
# Tests the workflow defined in test_basic_execution.json:
# upload-file -> download-file -> delete-file
# Verifies that log messages appear in the correct order

test_that("file upload/download/delete workflow executes correctly with ordered logs", {
  # Get the workflow JSON path
  # Try source tree first (tests/testthat/ is working dir when running tests)
  json_path <- file.path("..", "..", "inst", "extdata", "workflows", "test_basic_execution.json")
  if (!file.exists(json_path)) {
    # Fall back to installed package location
    json_path <- system.file("extdata", "workflows", "test_basic_execution.json", 
                             package = "FaaSr")
  }
  
  # Skip if workflow file doesn't exist
  if (!file.exists(json_path)) {
    skip("test_basic_execution.json not found")
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
  
  # Find line numbers for each expected log message
  line_before_upload <- which(grepl("Listing before upload.*testing", log_content))
  line_after_upload <- which(grepl("Listing after upload.*testing.*test\\.txt", log_content))
  line_before_download <- which(grepl("Listing before download.*testing", log_content))
  line_after_download <- which(grepl("Listing after download.*testing", log_content))
  line_before_delete <- which(grepl("Listing before delete.*testing.*test\\.txt", log_content))
  line_after_delete <- which(grepl("Listing after delete.*testing", log_content))
  
  # Verify all expected log messages exist
  expect_true(length(line_before_upload) > 0)
  expect_true(length(line_after_upload) > 0)
  expect_true(length(line_before_download) > 0)
  expect_true(length(line_after_download) > 0)
  expect_true(length(line_before_delete) > 0)
  expect_true(length(line_after_delete) > 0)
  
  # Verify log messages appear in the correct order
  expect_true(line_before_upload[1] < line_after_upload[1])
  expect_true(line_after_upload[1] < line_before_download[1])
  expect_true(line_before_download[1] < line_after_download[1])
  expect_true(line_after_download[1] < line_before_delete[1])
  expect_true(line_before_delete[1] < line_after_delete[1])
  
  # Verify specific content
  expect_true(any(grepl("Listing before upload.*testing.*: *$", log_content)))
  expect_true(any(grepl("Listing after upload.*testing.*test\\.txt", log_content)))
  expect_true(any(grepl("Content of the downloaded file.*THIS IS TESTING CONTENT", log_content)))
  expect_true(any(grepl("Listing before delete.*testing.*test\\.txt", log_content)))
  expect_true(any(grepl("Listing after delete.*testing.*: *$", log_content)))
  expect_true(any(grepl("File.*test\\.txt.*is deleted", log_content)))
})

