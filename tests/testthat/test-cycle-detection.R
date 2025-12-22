# Test 2: Cycle Detection
# Tests that workflows with cycles are rejected

test_that("simple cycle is detected: A -> B -> A", {
  skip_on_cran()
  
  # Define dummy functions
  define_test_function("func_a", function() TRUE)
  define_test_function("func_b", function() TRUE)
  
  # Create workflow with cycle: A -> B -> A
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
        InvokeNext = list("ActionA")  # Creates cycle back to A
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("func_a", "func_b"))
    cleanup_faasr_data()
  })
  
  # Should fail with cycle error
  expect_error(faasr_test(json_path), "cycle")
})

test_that("longer cycle is detected: A -> B -> C -> A", {
  skip_on_cran()
  
  define_test_function("func_a", function() TRUE)
  define_test_function("func_b", function() TRUE)
  define_test_function("func_c", function() TRUE)
  
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
        InvokeNext = list("ActionA")  # Creates cycle back to A
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
  
  expect_error(faasr_test(json_path), "cycle")
})

test_that("self-loop cycle is detected: A -> A", {
  skip_on_cran()
  
  define_test_function("func_a", function() TRUE)
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionA")  # Self-loop
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("func_a"))
    cleanup_faasr_data()
  })
  
  expect_error(faasr_test(json_path), "cycle")
})

test_that("DAG without cycles passes validation", {
  skip_on_cran()
  
  define_test_function("func_a", function() TRUE)
  define_test_function("func_b", function() TRUE)
  define_test_function("func_c", function() TRUE)
  define_test_function("func_d", function() TRUE)
  
  # Create diamond-shaped DAG: A -> B, A -> C, B -> D, C -> D
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionB", "ActionC")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionD")
      ),
      ActionC = list(
        FunctionName = "func_c",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("ActionD")
      ),
      ActionD = list(
        FunctionName = "func_d",
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
    cleanup_test_functions(c("func_a", "func_b", "func_c", "func_d"))
    cleanup_faasr_data()
  })
  
  # Should succeed without cycle error
  result <- faasr_test(json_path)
  expect_true(result)
})

