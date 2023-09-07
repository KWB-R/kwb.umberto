#' Group data
#'
#' @param raw_data as retrieved by function import_rawdata()
#' @param grouping_paras cleaned column names used for grouping. 
#' (default: c("lci_method", "model", "process", "unit"))
#' @param grouping_function R function used for grouping (default: "sum") 
#' @param summarise_col column name used for summarising (default: "quantity"), 
#' i.e. for which the "grouping_function" should be applied
#' @importFrom dplyr across group_by summarise_at rename_with
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @return aggregated data according 
#' @export
#' @examples 
#' zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
#' package = "kwb.umberto")
#' umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = umberto7_csv_dir)
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
#' head(umberto7_data_grouped )
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
#' head(umberto10_data_grouped)
group_data <- function(
    raw_data,
    grouping_paras = c("lci_method", "model", "process", "unit"),
    grouping_function = "sum",
    summarise_col = "quantity"
)
{
  summarise_col_fun <- function(summarise_col) {
    sprintf("%s_%s", summarise_col, grouping_function)
  }
  
  raw_data %>% 
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(grouping_paras))
    ) %>% 
    dplyr::rename_with(
      .fn = summarise_col_fun,  
      .cols = summarise_col
    ) %>% 
    dplyr::summarise_at(
      .vars = summarise_col_fun(summarise_col),
      .funs = grouping_function
    )
}


#' Make pivot data
#' @param rawdata_grouped grouped rawdata as retrieved by function group_data()
#' @param cols_to_ignore column names not to be considered for transforming the 
#' data from the "list" to the "wide" format with tidyr::spread()
#' (default: "unit")
#' @param key_col  column name containing the key for transforming the 
#' data from the "list" to the "wide" format with tidyr::spread() (default: "model")
#' @param value_col  column name containing the values that will be used during 
#' transforming the data from the "list" to the "wide" format with tidyr::spread() 
#' (default: "quantity_sum")
#' @importFrom dplyr select 
#' @importFrom tidyr spread
#' @return data.frame with the aggregated results for all different"lci_method"   
#' @export
#' @examples 
#' zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
#' package = "kwb.umberto")
#' umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = umberto7_csv_dir)
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
#' umberto7_data_pivot <- kwb.umberto::pivot_data(umberto7_data_grouped)
#' head(umberto7_data_pivot)
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
#' umberto10_data_pivot <- kwb.umberto::pivot_data(umberto10_data_grouped)
#' head(umberto10_data_pivot)
#' 
pivot_data <- function(
    rawdata_grouped, 
    cols_to_ignore = "unit",
    key_col = "model",
    value_col = "quantity_sum"
)
{
  rawdata_grouped %>% 
    dplyr::select(
      tidyselect::all_of(
        setdiff(names(rawdata_grouped), cols_to_ignore)
      )
    ) %>% 
    tidyr::spread(key = key_col, value = value_col)
}

#' Create pivot list
#'
#' @param pivot_data privot data as retrieved from function pivot_data()
#' @param arrange_cols columns used for arranging the data (default: "process")
#' @param method_col name of the column containing the method 
#'   (default: "lci_method"). Depending on your Umberto version you may need to
#'   set method_col to "lcia_method".
#' @return a list of results, where each element contains the result table for 
#' one lci_method
#' @importFrom dplyr right_join arrange
#' @importFrom kwb.utils selectColumns
#' @export
#' @examples 
#' 
#' zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
#' package = "kwb.umberto")
#' umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = umberto7_csv_dir)
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
#' umberto7_data_pivot <- kwb.umberto::pivot_data(umberto7_data_grouped)
#' umberto7_data_pivot_list <- kwb.umberto::create_pivot_list(umberto7_data_pivot)
#' head(umberto7_data_pivot_list)
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
#' umberto10_data_pivot <- kwb.umberto::pivot_data(umberto10_data_grouped)
#' umberto10_data_pivot_list <- kwb.umberto::create_pivot_list(umberto10_data_pivot)
#' head(umberto10_data_pivot_list)
#' 
create_pivot_list <- function(
    pivot_data, 
    arrange_cols = "process", 
    method_col = "lci_method"
)
{
  method_vector <- kwb.utils::selectColumns(pivot_data, method_col)
  
  methods <- unique(method_vector)
  
  indices <- seq_along(methods)
  
  lapply(
    X = indices, 
    FUN = function(i) {
      
      selected_lci_method <- methods[i]
      
      processes <- data.frame(
        METHOD = selected_lci_method,
        process = unique(kwb.utils::selectColumns(pivot_data, "process")),
        stringsAsFactors = FALSE
      ) %>%
        kwb.utils::renameColumns(list(
          METHOD = method_col
        ))
      
      pivot_data[method_vector == selected_lci_method, ] %>% 
        dplyr::right_join(processes) %>% 
        dplyr::arrange(arrange_cols)
    }
  ) %>% 
    stats::setNames(sprintf("%s%d", method_col, indices))
}
