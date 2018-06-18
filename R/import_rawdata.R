#' Import rawdata
#'
#' @param csv_dir path to directory with .csv files (in GERMAN format, i.e. 
#' sep = ";" and dec = ",") to be imported
#' @return data.frame with all imported raw data
#' @importFrom readr read_csv2
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' zipfile <- system.file("extdata/Beispiel_Auswertung.zip", package = "kwb.lca")
#' temp <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = temp)
#' rawdata <- kwb.lca::import_rawdata(csv_dir = temp)
#' head(rawdata)
import_rawdata <- function(csv_dir) {
  
  
  csv_files <- list.files(path = csv_dir,
                          pattern = ".csv", 
                          full.names = TRUE)
  
  
  if (length(csv_files) < 1) {
    stop(sprintf("No result '.csv' files in folder '%s/'", csv_dir))
  }
  
  for (csv_file in csv_files) {
    print(sprintf("Importing csv file '%s'", csv_file))

    tmp <- readr::read_csv2(file = csv_file,
                            locale = readr::locale("de"), 
                            col_types = "ccccccccccdc") %>%
      janitor::clean_names()
    
    
    if (csv_file == csv_files[1]) {
      rawdata  <- tmp
    } else {
      rawdata <- rbind(rawdata, tmp)
    }
  }
  
  return(rawdata)
  
}
