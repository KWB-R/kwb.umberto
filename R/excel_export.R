#' Write results to EXCEL
#' @param data_pivot_list a list as retrieved by function create_pivot_list()
#' @param path  relative or full path to be used for exporting the results to 
#' EXCEL (default: "results.xlsx")
#' @importFrom openxlsx write.xlsx
#' @return writes results in EXCEl file defined in "path", where each sheet 
#' @export
#' @examples 
#' zipfile <- system.file("extdata/Beispiel_Auswertung.zip", package = "kwb.lca")
#' temp <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = temp)
#' rawdata <- kwb.lca::import_rawdata(csv_dir = temp)
#' data_grouped <- kwb.lca::group_data(rawdata)
#' data_pivot <- kwb.lca::pivot_data(data_grouped)
#' data_pivot_list <- kwb.lca::create_pivot_list(data_pivot)
#' export_path <- file.path(getwd(), "results.xlsx")
#' print(sprintf("Exporting aggregated results to %s", export_path))
#' write_xlsx(data_pivot_list, path = export_path)
write_xlsx <- function(data_pivot_list,
                       path = "results.xlsx") {
  
  openxlsx::write.xlsx(data_pivot_list,
                       file = path)
  
}