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
import_rawdata <- function(csv_dir, sep = ";", ...) {
  
  
  csv_files <- list.files(path = csv_dir,
                          pattern = ".csv", 
                          full.names = TRUE)
  
  
  if (length(csv_files) < 1) {
    stop(sprintf("No result '.csv' files in folder '%s/'", csv_dir))
  }
  
  
  if(sep == ";") {
    read_input <- function(...) {readr::read_csv2(...)}
  } else if(sep == ",") {
    read_input <- function(...) {readr::read_csv(...)}
  } else {
    msg <- sprintf("\nThe fields of the CSV input file '%s' need use one of the 
following separators: ';' or ','\nPlease specify the 'sep' argument correctly!", 
csv_file)
    stop(msg)
  }
  
  for (csv_file in csv_files) {
    print(sprintf("Importing csv file '%s'", csv_file))

    tmp <- read_input(csv_file, ...) %>%
      janitor::clean_names()
    
    
    if (csv_file == csv_files[1]) {
      rawdata  <- tmp
    } else {
      rawdata <- rbind(rawdata, tmp)
    }
  }
  
  return(rawdata)
  
  }

