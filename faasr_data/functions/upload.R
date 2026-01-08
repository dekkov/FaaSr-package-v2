upload <- function(folder) {

  # list files before upload
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing before upload (", folder, "): ", paste(listed, collapse = ", ")))

  # Create a temporary test file and interact with local FaaSr storage
  writeLines("THIS IS TESTING CONTENT", "test.txt")

  # Upload the file to the specified remote folder
  faasr_put_file(local_file = "test.txt", remote_folder = folder, remote_file = "test.txt")

  # List files after upload
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing after upload (", folder, "): ", paste(listed, collapse = ", ")))
}

