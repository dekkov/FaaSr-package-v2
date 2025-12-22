# Test 5: Schema Validation
# Tests workflow configuration validation

test_that("missing ActionList is detected", {
  skip_on_cran()
  
  # Create workflow without ActionList
  workflow <- list(
    FunctionInvoke = "TestAction"
    # Missing ActionList
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path))
  
  expect_error(faasr_test(json_path), "ActionList")
})

test_that("missing FunctionInvoke is detected", {
  skip_on_cran()
  
  # Create workflow without FunctionInvoke
  workflow <- list(
    ActionList = list(
      TestAction = list(
        FunctionName = "test_func",
        FaaSServer = "TestServer",
        Type = "R"
      )
    )
    # Missing FunctionInvoke
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path))
  
  expect_error(faasr_test(json_path), "FunctionInvoke")
})

test_that("invalid InvokeNext target is detected", {
  skip_on_cran()
  
  define_test_function("test_func", function() TRUE)
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "test_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("NonexistentAction")  # Invalid target
      )
    ),
    function_invoke = "ActionA"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("test_func"))
    cleanup_faasr_data()
  })
  
  expect_error(faasr_test(json_path), "invalid function trigger")
})

test_that("invalid FunctionInvoke start node is detected", {
  skip_on_cran()
  
  define_test_function("test_func", function() TRUE)
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ActionA = list(
        FunctionName = "test_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "NonexistentStartAction"  # Invalid start node
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("test_func"))
    cleanup_faasr_data()
  })
  
  expect_error(faasr_test(json_path), "invalid start node")
})

test_that("predecessor consistency check detects mixed types", {
  skip_on_cran()
  
  define_test_function("cond_func", function() TRUE)
  define_test_function("direct_func", function() TRUE)
  define_test_function("merge_func", function() TRUE)
  
  # Create workflow where MergeAction has both unconditional (from DirectAction)

# and conditional (from CondAction) predecessors - this should fail
  workflow <- create_minimal_workflow(
    action_list = list(
      StartAction = list(
        FunctionName = "cond_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("CondAction", "DirectAction")
      ),
      CondAction = list(
        FunctionName = "cond_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list(
          list(True = list("MergeAction"), False = list())
        )
      ),
      DirectAction = list(
        FunctionName = "direct_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("MergeAction")  # Unconditional edge
      ),
      MergeAction = list(
        FunctionName = "merge_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "StartAction"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("cond_func", "direct_func", "merge_func"))
    cleanup_faasr_data()
  })
  
  expect_error(faasr_test(json_path), "mixed predecessor")
})

test_that("empty workflow is rejected", {
  skip_on_cran()
  
  workflow <- list(
    ActionList = list(),
    FunctionInvoke = ""
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit(unlink(json_path))
  
  expect_error(faasr_test(json_path))
})

test_that("invocation ID generation works", {
  skip_on_cran()
  
  captured_id <- NULL
  
  define_test_function("id_capture_func", function() {
    captured_id <<- faasr_invocation_id()
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      TestAction = list(
        FunctionName = "id_capture_func",
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
    cleanup_test_functions(c("id_capture_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_true(!is.null(captured_id))
  expect_true(nzchar(captured_id))
})

