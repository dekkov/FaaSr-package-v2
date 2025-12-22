# Test: Cycle Detection in Workflows
# Tests that workflow validation properly detects circular dependencies

test_that("simple two-node cycle is detected", {
  # Create workflow with A -> B -> A
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionB")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionA")
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path), add = TRUE)
  
  # Should fail with cycle detection error
  expect_error(faasr_test(json_path), "cycle")
})

test_that("three-node cycle is detected", {
  # Create workflow with A -> B -> C -> A
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionB")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionC")
      ),
      ActionC = list(
        FunctionName = "func_c",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionA")
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path), add = TRUE)
  
  # Should fail with cycle detection error
  expect_error(faasr_test(json_path), "cycle")
})

test_that("self-referencing cycle is detected", {
  # Create workflow with A -> A (self-cycle)
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionA")
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path), add = TRUE)
  
  # Should fail with cycle detection error
  expect_error(faasr_test(json_path), "cycle")
})

test_that("complex cycle with branching is detected", {
  # Create workflow with branching where one path creates a cycle
  # A -> B -> C -> A (cycle)
  #   -> D (no cycle)
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionB", "ActionD")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionC")
      ),
      ActionC = list(
        FunctionName = "func_c",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionA")
      ),
      ActionD = list(
        FunctionName = "func_d",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list()
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path), add = TRUE)
  
  # Should fail with cycle detection error
  expect_error(faasr_test(json_path), "cycle")
})

test_that("acyclic workflow passes cycle detection", {
  # Create valid acyclic workflow: A -> B -> C
  define_test_function("func_a", function() TRUE)
  define_test_function("func_b", function() TRUE)
  define_test_function("func_c", function() TRUE)
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "func_a",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionB")
      ),
      ActionB = list(
        FunctionName = "func_b",
        FaaSServer = "TestServer",
        Type = "R",
        InvokeNext = list("ActionC")
      ),
      ActionC = list(
        FunctionName = "func_c",
        FaaSServer = "TestServer",
        Type = "R",
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
  }, add = TRUE)
  
  # Should succeed without cycle error
  result <- faasr_test(json_path)
  expect_true(result)
})

