# import_json_files_to_excel ---------------------------------------------------

#' Import JSON files to Excel File
#' 
#' @param json_dir path to directory containing .json files
#' @param file path to Excel file to be created. Default: 
#'   \code{"umberto-results.xlsx"} within \code{json_dir}
#' @param open logical indicating whether or not to open the created Excel file
#' @return path to created Excel file
#' @importFrom kwb.utils hsOpenWindowsExplorer substSpecialChars
#' @importFrom writexl write_xlsx
#' @export
import_json_files_to_excel <- function(
    json_dir, 
    file = file.path(json_dir, "umberto-results.xlsx"),
    open = TRUE
)
{
  sheets <- json_dir %>%
    import_rawdata_json(add_place = TRUE, old_format = FALSE) %>%
    get_core_data() %>%
    core_data_to_wide() %>%
    split_by_columns("indicator")
  
  writexl::write_xlsx(sheets, file)
  
  if (open) {
    try(kwb.utils::hsOpenWindowsExplorer(file))
  }
  
  file
}

# get_core_data ----------------------------------------------------------------
get_core_data <- function(data)
{
  result <- kwb.utils::renameAndSelect(data, list(
    model = "model",
    process_name = "process",
    place_name = "place",
    entry_exchange = "exchange",
    indicator_name = "indicator",
    product_quantity = "quantity",
    product_unit = "unit"
  ))
  
  result <- result[!grepl("^Connection", result$place), ]
  
  duplicates <- kwb.utils::findPartialDuplicates(
    result, setdiff(names(result), "quantity")
  )
  
  stopifnot(is.null(duplicates))
  
  result
}

# core_data_to_wide ------------------------------------------------------------
core_data_to_wide <- function(data)
{
  data %>%
    tidyr::pivot_wider(
      id_cols = c(
        "indicator",
        "process", 
        "place", 
        "exchange", 
        "unit"
      ),
      names_from = "model",
      values_from = "quantity"
    ) %>%
    kwb.utils::orderBy(c(
      "indicator", 
      "process", 
      "place", 
      "exchange"
    ))
}
