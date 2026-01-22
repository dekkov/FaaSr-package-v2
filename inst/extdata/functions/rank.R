rank <- function() {
  rank_info <- faasr_rank()
  
  # Log and print the rank information
  msg <- sprintf("Rank info: %s", if (length(rank_info) > 0) {
    paste0("Executing rank ", rank_info$rank, "/", rank_info$max_rank)
  } else {
    "not available"
  })

  faasr_log(msg)
  
  invisible(rank_info)
}