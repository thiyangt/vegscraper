#' Extract data from daily price reports published by the Central Bank, Sri Lanka
#'
#'@param input_dir location of the folder where you have daily reports
#'@param out_dir location of the folder where you need to save the outputs
#'@return tidy datasets
#'
#'@export
datascraper <- function(input_dir, output_dir){

  # Get list of all CSV files
  csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

  # Loop through each CSV
  for (file in csv_files) {
    df <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)

    # Remove rows 1 to 5 and 15
    df <- df[-c(1:5, 15), ]

    # Remove columns B and E (2 and 4)
    df <- df[, -c(2, 5)]

    # Rename columns
    colnames(df) <- c("Item", "Unit",
                      "Wholesale (Pettah, Yesterday)",
                      "Retail (Pettah, Today)",
                      "Wholesale (Pettah, Today)",
                      "Wholesale (Dambulla, Yesterday)",
                      "Wholesale (Dambulla, Today)",
                      "Retail (Pettah, Yesterday)",
                      "Retail (Dambulla, Yesterday)",
                      "Retail (Dambulla, Today)",
                      "Retail (Narahenpita, Yesterday)",
                      "Retail (Narahenpita, Today)")

    # Reorder columns to move "Retail (Pettah, Today)"
    col_order <- c("Item", "Unit",
                   "Wholesale (Pettah, Yesterday)",
                   "Wholesale (Pettah, Today)",
                   "Wholesale (Dambulla, Yesterday)",
                   "Wholesale (Dambulla, Today)",
                   "Retail (Pettah, Yesterday)",
                   "Retail (Pettah, Today)",
                   "Retail (Dambulla, Yesterday)",
                   "Retail (Dambulla, Today)",
                   "Retail (Narahenpita, Yesterday)",
                   "Retail (Narahenpita, Today)")
    df <- df[, col_order]

    # Create 3-row header
    header1 <- c("", "", rep("Wholesale", 4), rep("Retail", 6))
    header2 <- c("", "", "Pettah", "Pettah", "Dambulla", "Dambulla",
                 "Pettah", "Pettah", "Dambulla","Dambulla", "Narahenpita", "Narahenpita", "")
    header3 <- c("Item", "Unit", "Yesterday", "Today", "Yesterday", "Today",
                 "Yesterday", "Today", "Yesterday","Today", "Yesterday", "Today")

    # Output path
    file_out <- file.path(output_dir, paste0("cleaned_", basename(file)))

    # Write file with 3-row header
    cat(paste(header1, collapse = ","), "\n", file = file_out)
    cat(paste(header2, collapse = ","), "\n", file = file_out, append = TRUE)
    cat(paste(header3, collapse = ","), "\n", file = file_out, append = TRUE)
    write.table(df, file = file_out, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE, quote = TRUE)
  }

  message("All files processed and saved to: ", output_dir)



}
