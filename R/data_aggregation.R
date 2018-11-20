#' Group data
#'
#' @param raw_data as retrieved by function import_rawdata()
#' @param grouping_paras cleaned column names used for grouping. 
#' (default: c("lci_method", "model", "process", "unit"))
#' @param grouping_function R function used for grouping (default: "sum") 
#' @importFrom magrittr "%>%" 
#' @importFrom dplyr group_by_ summarise_
#' @importFrom stats setNames
#' @return aggregated data according 
#' @export
#' @examples 
#' umberto7_csv_dir <- system.file("extdata/umberto-nxt_v7.1.0.13.503", 
#' package = "kwb.umberto")
#' umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
#' umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
#' head(umberto7_data_grouped )
#' 
#' umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
#' package = "kwb.umberto")
#' umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
#' umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
#' head(umberto10_data_grouped)
group_data <- function(raw_data,
                       grouping_paras = c("lci_method", "model", "process", "unit"),
                       grouping_function = "sum") {
  
  res <- raw_data %>% 
    dplyr::group_by_(.dots = grouping_paras) %>%  
    dplyr::summarise_(.dots=stats::setNames(sprintf("%s(quantity)", 
                                                    grouping_function), 
                                     sprintf("quantity_%s", grouping_function))) 
  return(res)
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
#' @importFrom tidyr spread_
#' @return data.frame with the aggregated results for all different"lci_method"   
#' @export
#' @examples 
#' umberto7_csv_dir <- system.file("extdata/umberto-nxt_v7.1.0.13.503", 
#' package = "kwb.umberto")
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
pivot_data <- function(rawdata_grouped, 
                       cols_to_ignore = "unit",
                       key_col = "model",
                       value_col = "quantity_sum") {
  
  rawdata_grouped %>% 
    dplyr::select_(.dots = setdiff(names(rawdata_grouped), cols_to_ignore)) %>% 
    tidyr::spread_(key_col,
                   value_col)
}

#' Create pivot list
#'
#' @param pivot_data privot data as retrieved from function pivot_data()
#' @param arrange_cols columns used for arranging the data (default: "process")
#' @return a list of results, where each element contains the result table for 
#' one lci_method
#' @importFrom dplyr right_join arrange
#' @export
#' @examples 
#' 
#' umberto7_csv_dir <- system.file("extdata/umberto-nxt_v7.1.0.13.503", 
#' package = "kwb.umberto")
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
create_pivot_list <- function(pivot_data, 
                              arrange_cols = "process") {
  
  myList <- list()
  lci_methods <- unique(pivot_data$lci_method)
  for (i in seq_along(lci_methods)) {
    
    selected_lci_method <- unique(pivot_data$lci_method)[i]
    
    processes <- data.frame(lci_method = selected_lci_method,
                            process = unique(pivot_data$process),
                            stringsAsFactors = FALSE)
    
    
    tmp_data <- pivot_data[pivot_data$lci_method ==  selected_lci_method,] %>% 
      dplyr::right_join(processes) %>% 
      dplyr::arrange_(arrange_cols)
    
    myList[[i]] <- tmp_data
  }
  names(myList) <- sprintf("lci_method%d", seq_along(lci_methods))
  return(myList)
}