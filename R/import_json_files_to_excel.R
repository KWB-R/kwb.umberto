# import_json_files_to_excel ---------------------------------------------------

#' Import JSON files to Excel File
#' 
#' @param json_dir path to directory containing .json files
#' @param file path to Excel file to be created. Default: 
#'   \code{"umberto-results.xlsx"} within \code{json_dir}
#' @param overwrite whether or not to overwrite the Excel \code{file} if it 
#'   exists. Default: \code{FALSE}.
#' @param open logical indicating whether or not to open the created Excel file
#' @param expand_keys If this argument is not \code{NULL} but a vector of (key)
#'   column names, all sheets are expanded to the same number of rows. All
#'   possible combinations of values in these key columns are then given in each
#'   sheet even though there are no values for these key value combinations.
#'   Default: \code{c("indicator", "process", "place", "exchange")}
#' @return path to created Excel file
#' @importFrom kwb.utils hsOpenWindowsExplorer substSpecialChars
#' @importFrom writexl write_xlsx
#' @export
import_json_files_to_excel <- function(
    json_dir, 
    file = file.path(json_dir, "umberto-results.xlsx"),
    overwrite = FALSE,
    open = TRUE,
    expand_keys = c("process", "place", "exchange")
)
{
  #kwb.utils::assignPackageObjects("kwb.umberto");`%>%` <- magrittr::`%>%`
  
  sheets <- json_dir %>%
    import_rawdata_json(add_place = TRUE, old_format = FALSE) %>%
    get_core_data() %>%
    core_data_to_wide() %>%
    split_by_columns("indicator")
  
  names(sheets) <- sprintf("m%02d", seq_along(sheets))
  
  if (expand) {
    sheets <- expand_to_all_key_combinations(sheets, keys = expand_keys)
  }
  
  file_exists <- file.exists(file)
  quoted_file <- dQuote(file, '"')
  
  if (file_exists && !overwrite) {
    stop(
      "The Excel file exists:\n  ",
      quoted_file, 
      "\nPlease choose another file name or set overwrite = TRUE.", 
      call. = FALSE
    )
  }
  
  kwb.utils::catAndRun(
    paste(
      ifelse(file_exists, "Overwriting", "Writing"), 
      "Excel file", 
      quoted_file
    ),
    writexl::write_xlsx(sheets, file)
  )
  
  if (open) {
    try(kwb.utils::hsOpenWindowsExplorer(path.expand(file)))
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

# expand_to_all_key_combinations -----------------------------------------------
expand_to_all_key_combinations <- function(sheets, keys)
{
  level_combis <- unique(do.call(rbind, lapply(
    sheets, 
    FUN = function(sheet) unique(kwb.utils::selectColumns(sheet, keys))
  )))

  lapply(names(sheets), function(indicator) {
    dplyr::left_join(
      x = data.frame(indicator = indicator, level_combis),
      y = sheets[[indicator]], 
      by = c("indicator", keys)
    )
  })
}
