# Test 4: Parallel Rank Execution
# Tests rank notation like FunctionName(3)

test_that("rank execution runs correct number of instances", {
  skip_on_cran()
  
  rank_executions <- list()
  
  define_test_function("start_func", function() {
    TRUE
  })
  define_test_function("ranked_func", function() {
    rank_info <- faasr_rank()
    rank_executions <<- c(rank_executions, list(rank_info))
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      StartAction = list(
        FunctionName = "start_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("RankedAction(3)")  # 3 parallel ranks
      ),
      RankedAction = list(
        FunctionName = "ranked_func",
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
    cleanup_test_functions(c("start_func", "ranked_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_equal(length(rank_executions), 3)
  
  # Check rank values
  ranks <- sapply(rank_executions, function(r) as.integer(r$Rank))
  max_ranks <- sapply(rank_executions, function(r) as.integer(r$MaxRank))
  
  expect_equal(sort(ranks), c(1, 2, 3))
  expect_true(all(max_ranks == 3))
})

test_that("single execution has no rank info", {
  skip_on_cran()
  
  captured_rank <- NULL
  
  define_test_function("single_func", function() {
    captured_rank <<- faasr_rank()
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      SingleAction = list(
        FunctionName = "single_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      )
    ),
    function_invoke = "SingleAction"
  )
  
  json_path <- write_workflow_json(workflow)
  on.exit({
    unlink(json_path)
    cleanup_test_functions(c("single_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  # For single execution (rank 1/1), rank info file is removed
  expect_equal(length(captured_rank), 0)
})

test_that("rank execution with successors works correctly", {
  skip_on_cran()
  
  execution_log <- character()
  
  define_test_function("start_func", function() {
    execution_log <<- c(execution_log, "start")
    TRUE
  })
  define_test_function("ranked_func", function() {
    rank_info <- faasr_rank()
    execution_log <<- c(execution_log, paste0("ranked_", rank_info$Rank))
    TRUE
  })
  define_test_function("final_func", function() {
    execution_log <<- c(execution_log, "final")
    TRUE
  })
  
  workflow <- create_minimal_workflow(
    action_list = list(
      StartAction = list(
        FunctionName = "start_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("RankedAction(2)")
      ),
      RankedAction = list(
        FunctionName = "ranked_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list("FinalAction")
      ),
      FinalAction = list(
        FunctionName = "final_func",
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
    cleanup_test_functions(c("start_func", "ranked_func", "final_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_true("start" %in% execution_log)
  expect_true("ranked_1" %in% execution_log)
  expect_true("ranked_2" %in% execution_log)
  expect_true("final" %in% execution_log)
})

test_that("conditional with rank notation works", {
  skip_on_cran()
  
  rank_executions <- list()
  
  define_test_function("condition_func", function() {
    return(TRUE)
  })
  define_test_function("ranked_true_func", function() {
    rank_info <- faasr_rank()
    rank_executions <<- c(rank_executions, list(rank_info))
    TRUE
  })
  define_test_function("false_func", function() {
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
          list(True = list("TrueAction(2)"), False = list("FalseAction"))
        )
      ),
      TrueAction = list(
        FunctionName = "ranked_true_func",
        FaaSServer = "TestServer",
        Type = "R",
        Arguments = list(),
        InvokeNext = list()
      ),
      FalseAction = list(
        FunctionName = "false_func",
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
    cleanup_test_functions(c("condition_func", "ranked_true_func", "false_func"))
    cleanup_faasr_data()
  })
  
  result <- faasr_test(json_path)
  
  expect_true(result)
  expect_equal(length(rank_executions), 2)
})

