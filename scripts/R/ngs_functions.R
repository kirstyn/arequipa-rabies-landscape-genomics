# ngs functions

concat_fasta <- function(input_dir, output_file) {
  # Find all FASTA files in the folder
  fasta_files <- list.files(input_dir, pattern = "\\.fasta$|\\.fa$", full.names = TRUE, ignore.case = TRUE)
  
  # Check if any FASTA files found
  if (length(fasta_files) == 0) {
    stop("No FASTA files found in the specified directory.")
  }
  
  # Read and concatenate contents
  fasta_contents <- lapply(fasta_files, readLines)
  all_lines <- unlist(fasta_contents)
  
  # Write to output file
  writeLines(all_lines, output_file)
  
  message("Concatenated ", length(fasta_files), " FASTA files into: ", output_file)
}

# Example usage:
# concat_fasta("path/to/folder", "combined.fasta")