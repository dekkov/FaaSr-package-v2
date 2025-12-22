delete <- function(folder, file) {
  # list the files in the folder
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing before delete (", folder, "): ", paste(listed, collapse = ", ")))

  # Delete the file from the specified remote folder
  faasr_delete_file(remote_folder = folder, remote_file = file)

  # List files after delete
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing after delete (", folder, "): ", paste(listed, collapse = ", ")))

  # Check if the file is deleted
  if (file %in% listed) {
    faasr_log(paste0("File (", file, ") is not deleted"))
  } else {
    faasr_log(paste0("File (", file, ") is deleted"))
  }
}