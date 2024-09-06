#****************************************************       HELPER      ********************************************************
#****************************************************     FUNCTIONS     ********************************************************
#****************************************************        TS         ********************************************************

#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 29. August 2024
# Contact: tobiaschnei@gmail.com, www.drtobiasschneider.com

# Reference: Schneider, T., & Castaneda, I.S. (2024). "GaDGeT – GDGT calculations simplified: an adaptable R-toolbox 
# for rapid GDGT index calculations." Organic Geochemistry. DOI: xxxx/yyyy



# DISCLAIMER

#   This script is provided "as is" without any warranties.Please 
#   contact the author for troubleshooting or modifications.

#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# This script contains helper functions used for the main script to read in data etc.





############################################################################################################################
############################################# HELPER FUNCTIONS #############################################################
############################################################################################################################

# ================ DIRECTORIES CREATION =========================
# Function to create directories if they don't exist
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}



# ================ READ IN DATA =========================
# Function to read data with consistent precision for both .xlsx and .csv files
read_data <- function(file_path) {
  if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    # Read Excel file with full precision (readxl does this by default)
    data <- read_xlsx(file_path, sheet = "GDGTs")
  } else if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    # Read CSV file with high precision using readr
    # Ensure the first column is read as data, not as row names
    data <- read_csv(file_path, col_types = cols(.default = col_double()))
    
    # If the first column contains non-numeric data (e.g., an ID), you should adjust the column type
    # For example, assuming the first column is a string:
    first_col <- read_csv(file_path, col_types = cols(.default = col_character()))[, 1]
    data <- read_csv(file_path, col_types = cols(.default = col_double()))
    data <- cbind(first_col, data)
    
    # Rename the first column if needed
    colnames(data)[1] <- "ID"  # Replace "ID" with the actual column name
  } else {
    stop("Unsupported file type: ", file_path)
  }
  return(data)
}






# ================ Export FA TO CSV =========================


# Helper function to generate CSV export configurations and write them to files
export_data_to_csv <- function(data_sets, output_directory, data_set_name) {
  
  # Define a list of datasets and corresponding filenames
  csv_exports <- list(
    list(data = data_sets$brGDGT.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-FULL_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.7Me.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-FULL_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.MI.FA[, c(1,2,5,6,11,12,3,7,8,13,14,4,9,10,15,16)], 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-MI_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.METH.5Mep.FA[, c(1,2,5,6,3,7,8,4,9,10)], 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-METH-5Mep_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.METH.6Mep.FA[, c(1,2,5,6,3,7,8,4,9,10)], 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-METH-6Mep_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.METH.5Me.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-METH-5Me_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.METH.6Me.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-METH-6Me_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.METH.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-METH_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.CYCL.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-CI_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.CYCL.5Me.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-CYC-5Me_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.CYCL.6Me.FA, 
         file = paste(output_directory$DirFA.br, "/", data_set_name, "_FA-CYC-6Me_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$brGDGT.FA, 
         file = paste(output_directory$DirFA, "/", data_set_name, "_FAs_brGDGTs_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$fGDGTs.FA, 
         file = paste(output_directory$DirFA, "/", data_set_name, "_FAs_isoGDGTs_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$fOHGDGTs.FA, 
         file = paste(output_directory$DirFA, "/", data_set_name, "_FAs_OHGDGTs_", Sys.Date(), ".csv", sep = "")),
    
    list(data = data_sets$fGMGTs.FA, 
         file = paste(output_directory$DirFA, "/", data_set_name, "_FAs_GMGTs_", Sys.Date(), ".csv", sep = ""))
  )
  
  # Write each dataset to its corresponding file
  lapply(csv_exports, function(x) {
    write.csv(x$data, row.names = FALSE, file = x$file)
  })
}









# ================ SAVE SESSION INFO =========================


# Helper function to save session information for reproducibility
save_session_info <- function(output_dir, data_set_name, software_version = "GaDGeT v1.0") {
  # Prepare a file to store session information including software version
  session_info_file <- paste0(output_dir, "/SESSION_INFO_", data_set_name, "_", Sys.Date(), ".txt")
  
  # Open the file in write mode
  session_info_con <- file(session_info_file, open = "wt")
  
  # Write the software version at the top of the session info file
  writeLines(paste("Software Version:", software_version), session_info_con)
  
  # Write the current date and time
  writeLines(paste("Date:", Sys.time()), session_info_con)
  
  # Append the session information
  writeLines(capture.output(sessionInfo()), session_info_con)
  
  # Close the connection
  close(session_info_con)
}








# ================ READ AND PROCESS DATAFILES =========================



# Helper function to read data files and handle parsing issues
read_and_process_files <- function(file_paths, working_directory) {
  # Initialize list for data compilation
  data_sets <- list()
  
  # Loop through each file path and read the data
  suppressWarnings(for (i in seq_along(file_paths)) {
    file_path <- paste0(working_directory, "/Input/", file_paths[i])
    
    # Read data using the updated function to ensure consistent precision
    data_sets[[i]] <- tryCatch({
      data <- read_data(file_path)
      parse_problems <- problems(data)
      if (nrow(parse_problems) > 0) {
        message("Parsing issues detected in file: ", file_path)
        print(parse_problems)
      }
      data  # Return the data if no critical issues
    }, error = function(e) {
      stop("Error reading file: ", file_path, "\n", e)
    })
  })
  
  # Get dataset names by removing the file extension
  data_sets_names <- tools::file_path_sans_ext(file_paths)
  
  return(list(data_sets = data_sets, data_sets_names = data_sets_names))
}



#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#