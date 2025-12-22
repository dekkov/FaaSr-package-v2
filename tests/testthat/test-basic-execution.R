# Test 1: Basic Workflow Execution
# Tests simple linear workflow: A -> B -> C

test_that("basic linear workflow executes in correct order", {
  skip_on_cran()  # Skip on CRAN due to file system operations
  
  # Track execution order
  execution_log <- character()
  
  # Define test functions
  define_test_function("func_a", function() {
    execution_log <<- c(execution_log, "A")
    TRUE
  })
  define_test_function("func_b", function() {
    execution_log <<- c(execution_log, "B")
    TRUE
  })
  define_test_function("func_c", function() {
    execution_log <<- c(execution_log, "C")
    TRUE
  })
  
  # Create workflow: A -> B -> C
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionB")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionC")
      ),
      ActionC = list(
        FunctionName = "func_c",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("func_a", "func_b", "func_c"))
    cleanup_faasr_data()
  })
  
  # Execute workflow
  result <- faasr_test(json_path)
  
  # Verify execution
  expect_true(result)
  expect_equal(execution_log, c("A", "B", "C"))
})

test_that("workflow with function arguments works correctly", {
  skip_on_cran()
  
  captured_args <- list()
  
  define_test_function("func_with_args", function(x, y) {
    captured_args <<- list(x = x, y = y)
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      TestAction = list(
        FunctionName = "func_with_args",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(x = 10, y = "hello"),
        InvokeNext = list()
      )
    )
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("func_with_args"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_equal(captured_args$x, 10)
  expect_equal(captured_args$y, "hello")
})

test_that("workflow fails on missing function", {
  skip_on_cran()
  
  workflow <- create_minimal_workflow(
    action_list = list(
      TestAction = list(
        FunctionName = "nonexistent_function",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    )
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_faasr_data()
  })
  
  expect_error(faasr_test(json_path), "Function not found")
})

test_that("workflow fails on missing JSON file", {
  expect_error(faasr_test("nonexistent_file.json"), "Workflow JSON not found")
})

test_that("workflow fails on invalid JSON", {
  invalid_json <- tempfile(fileext = ".json")
  writeLines("{ invalid json content", invalid_json)
  on.exit(unlink(invalid_json))
  
  expect_error(faasr_test(invalid_json), "JSON parsing error")
})

