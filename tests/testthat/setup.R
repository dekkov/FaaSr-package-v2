# Setup for testthat tests - source package R files when testing from source

# Get all R files from the package's R directory
r_dir <- file.path("..", "..", "R")
if (dir.exists(r_dir)) {
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) {
    source(f)
  }
}

