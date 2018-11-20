#' Write results to EXCEL
#' @param data_pivot_list a list as retrieved by function create_pivot_list()
#' @param path  relative or full path to be used for exporting the results to 
#' EXCEL (default: "results.xlsx")
#' @importFrom openxlsx write.xlsx
#' @return writes results in EXCEl file defined in "path", where each sheet 
#' @export
#' @examples 
#' zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
#' package = "kwb.umberto")
#' umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = umberto7_csv_dir)
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
#' umberto7_data_pivot <- kwb.umberto::pivot_data(umberto7_data_grouped)
#' umberto7_data_pivot_list <- kwb.umberto::create_pivot_list(umberto7_data_pivot)
#' export_path <- file.path(getwd(), "umberto7_results.xlsx")
#' print(sprintf("Exporting aggregated results to %s", export_path))
#' write_xlsx(umberto7_data_pivot_list, path = export_path)
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
#' umberto10_data_pivot <- kwb.umberto::pivot_data(umberto10_data_grouped)
#' umberto10_data_pivot_list <- kwb.umberto::create_pivot_list(umberto10_data_pivot)
#' export_path <- file.path(getwd(), "umberto10_results.xlsx")
#' print(sprintf("Exporting aggregated results to %s", export_path))
#' write_xlsx(umberto10_data_pivot_list, path = export_path)
write_xlsx <- function(data_pivot_list,
                       path = "results.xlsx") {
  
  openxlsx::write.xlsx(data_pivot_list,
                       file = path)
  
}