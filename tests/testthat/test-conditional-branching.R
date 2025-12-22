# Test 3: Conditional Branching
# Tests True/False conditional paths

test_that("conditional branch follows True path when function returns TRUE", {
  skip_on_cran()
  
  execution_log <- character()
  
  define_test_function("condition_func", function() {
    execution_log <<- c(execution_log, "condition")
    return(TRUE)  # Will trigger True branch
  })
  define_test_function("true_path_func", function() {
    execution_log <<- c(execution_log, "true_path")
    TRUE
  })
  define_test_function("false_path_func", function() {
    execution_log <<- c(execution_log, "false_path")
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ConditionAction = list(
        FunctionName = "condition_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list(
          list(True = list("TrueAction"), False = list("FalseAction"))
        )
      ),
      TrueAction = list(
        FunctionName = "true_path_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      ),
      FalseAction = list(
        FunctionName = "false_path_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "ConditionAction"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("condition_func", "true_path_func", "false_path_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_equal(execution_log, c("condition", "true_path"))
  expect_false("false_path" %in% execution_log)
})

test_that("conditional branch follows False path when function returns FALSE", {
  skip_on_cran()
  
  execution_log <- character()
  
  define_test_function("condition_func", function() {
    execution_log <<- c(execution_log, "condition")
    return(FALSE)  # Will trigger False branch
  })
  define_test_function("true_path_func", function() {
    execution_log <<- c(execution_log, "true_path")
    TRUE
  })
  define_test_function("false_path_func", function() {
    execution_log <<- c(execution_log, "false_path")
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      ConditionAction = list(
        FunctionName = "condition_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list(
          list(True = list("TrueAction"), False = list("FalseAction"))
        )
      ),
      TrueAction = list(
        FunctionName = "true_path_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      ),
      FalseAction = list(
        FunctionName = "false_path_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "ConditionAction"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("condition_func", "true_path_func", "false_path_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_equal(execution_log, c("condition", "false_path"))
  expect_false("true_path" %in% execution_log)
})

test_that("nested conditional branches work correctly", {
  skip_on_cran()
  
  execution_log <- character()
  
  define_test_function("first_condition", function() {
    execution_log <<- c(execution_log, "first_condition")
    return(TRUE)
  })
  define_test_function("second_condition", function() {
    execution_log <<- c(execution_log, "second_condition")
    return(FALSE)
  })
  define_test_function("path_a", function() {
    execution_log <<- c(execution_log, "path_a")
    TRUE
  })
  define_test_function("path_b", function() {
    execution_log <<- c(execution_log, "path_b")
    TRUE
  })
  define_test_function("final_func", function() {
    execution_log <<- c(execution_log, "final")
    TRUE
  })
  
  # Workflow: first_condition -> (True: second_condition, False: path_b)
  #           second_condition -> (True: path_a, False: final_func)
  workflow <- create_minimal_workflow(
    action_list = list(
      FirstCondition = list(
        FunctionName = "first_condition",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list(
          list(True = list("SecondCondition"), False = list("PathB"))
        )
      ),
      SecondCondition = list(
        FunctionName = "second_condition",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list(
          list(True = list("PathA"), False = list("FinalAction"))
        )
      ),
      PathA = list(
        FunctionName = "path_a",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      ),
      PathB = list(
        FunctionName = "path_b",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      ),
      FinalAction = list(
        FunctionName = "final_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "FirstCondition"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("first_condition", "second_condition", "path_a", "path_b", "final_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  # first_condition returns TRUE -> goes to SecondCondition
  # second_condition returns FALSE -> goes to FinalAction
  expect_equal(execution_log, c("first_condition", "second_condition", "final"))
})

