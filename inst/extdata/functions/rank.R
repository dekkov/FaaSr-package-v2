rank <- function() {
  rank_info <- faasr_rank()
  
  # Log and print the rank information
  msg <- sprintf("Rank info: %s", if (length(rank_info) > 0) {
    paste0("Executing rank ", rank_info$Rank, "/", rank_info$MaxRank)
  } else {
    "not available"
  })
  
  faasr_log(msg)
  cat(msg, "\n")
  
  invisible(rank_info)
}