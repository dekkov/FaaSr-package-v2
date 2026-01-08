download <- function(folder, file) {
  # list the files in the folder before download
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing before download (", folder, "): ", paste(listed, collapse = ", ")))

  # Download the file from the specified remote folder
  faasr_get_file(remote_folder = folder, remote_file = file, local_file = "downloaded_file.txt")

  # list the files in the folder after download
  listed <- faasr_get_folder_list(faasr_prefix = folder)
  faasr_log(paste0("Listing after download (", folder, "): ", paste(listed, collapse = ", ")))

  # Log the content of the downloaded file
  content <- readLines("downloaded_file.txt")
  faasr_log(paste0("Content of the downloaded file (", file, "): ", paste(content, collapse = ", ")))
}