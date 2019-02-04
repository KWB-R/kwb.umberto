#' Import rawdata
#'
#' @param csv_dir path to directory with .csv files 
#' @param sep Single character used to separate fields within a record (default: 
#' in GERMAN format, i.e. sep = ";")
#' @param ... further arguments passed to either readr::read_csv2 (default for sep == ';') or 
#' readr::read_csv (if sep == ',')
#' @return data.frame with all imported raw data
#' @importFrom readr read_csv2 read_csv
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
#' package = "kwb.umberto")
#' umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = umberto7_csv_dir)
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' head(umberto7_rawdata)
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' head(umberto10_rawdata)
import_rawdata <- function(csv_dir, sep = ";", ...)
{
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) < 1) {
    
    stop_(sprintf("No result files (*.csv) in folder '%s/'", csv_dir))
  }

  # Fail early if an unexpected separator is given
  if (! identical(sep, ",") && ! identical(sep, ";")) {
    
    stop_(
      "\nThe fields of the CSV input files need to use one of the following ", 
      "separators: ';' or ','\nPlease specify the 'sep' argument correctly!"
    )
  }

  # Assign the read function that corresponds to the separator
  read_input <- if (sep == ";") {
    
    readr::read_csv2 
    
  } else {
    
    readr::read_csv
  }
  
  # Import all files
  contents <- lapply(csv_files, function(csv_file) {
    
    message(sprintf("Importing csv file '%s'", csv_file))
    
    janitor::clean_names(read_input(csv_file, ...))
  })

  # If only one file was read, return the only data frame in the list
  if (length(contents) == 1) {
    
    return(contents[[1]])
  }

  # Otherwise check if all data frames have the same column names
  stop_on_differing_names(stats::setNames(contents, basename(csv_files)))

  # Row-bind the data frames together
  do.call(rbind, contents)
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...) stop(..., call. = FALSE)

# stop_on_differing_names ------------------------------------------------------
stop_on_differing_names <- function(x)
{
  # We expect a list of data frames as input
  stopifnot(is.list(x), all(sapply(x, is.data.frame)))

  # Return if there are not at least two data frames  
  if (length(x) < 2) {
    
    return()
  }
  
  # Get the column names of the first data frame of the list
  names_1 <- names(x[[1]])
  
  # Compare with the column names of the other data frames
  for (i in seq_along(x)[-1]) {
    
    names_i <- names(x[[i]])
    
    if (! identical(names_1, names_i)) {
      
      stop_(
        "There are differing column names:\n", 
        "  ", names(x)[1], ": ", paste(names_1, collapse = ", "),
        "\n  ", names(x)[i], ": ", paste(names_i, collapse = ", ")
      )
    }
  }
}
