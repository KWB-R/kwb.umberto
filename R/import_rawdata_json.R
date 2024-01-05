if (FALSE)
{
  json_dir = "~/../Downloads/S/support/fabian/R-Umberto/Umberto11"
  
  files <- dir(json_dir, "\\.json$", full.names = TRUE)
  
  result <- kwb.umberto:::import_rawdata_json(json_dir, add_place = TRUE)

  results <- lapply(files, FUN = function(file) {
    kwb.umberto:::import_rawdata_json(files = file, add_place = TRUE)
  })
  
  stopifnot(identical(do.call(rbind, results), result))
  
  kwb.utils::assignPackageObjects("kwb.umberto")
  contents <- read_json_files(json_dir)
}

# import_rawdata_json ----------------------------------------------------------

#' Import Umberto results from .json files
#' 
#' @param json_dir path to directory containing .json files. All .json files
#'   will be read.
#' @param old_format if TRUE (the default) the same columns are provided that
#'   are provided by \code{\link{import_rawdata}} that imports .csv files
#' @param add_place With add_place = TRUE, the "place" is contained in the 
#'   result even if old_format = TRUE
#' @param files optional. If given and not \code{NULL} this is expected to be a
#'   vector of character with the full paths to the \code{.json} files to be 
#'   read.
#' @return data frame
#' @export
import_rawdata_json <- function(
    json_dir, 
    old_format = TRUE, 
    add_place = FALSE, 
    files = NULL
)
{
  #kwb.utils::assignPackageObjects("kwb.umberto")
  contents <- read_json_files(json_dir, files = files)
  
  result_tables <- lapply(contents, to_tables)
  
  data_frames <- lapply(result_tables, merge_json_tables)
  
  result <- kwb.utils::rbindAll(data_frames, nameColumn = "model")

  if (!old_format) {
    return(result)
  }
  
  fetch <- kwb.utils::createAccessor(result)
  
  result <- data.frame(
    project = "not-used",
    model = fetch("model"),
    net = "not-used",
    timestamp = fetch("timestamp"),
    product = "not-used",
    product_name = fetch("ref_flow_exchange"),
    product_arrow = fetch("product_arrow"),
    product_flow_amount = fetch("product_amount"),
    lcia_method = fetch("indicator_name"),
    phase = "not-used",
    process = fetch("process_name"),
    material_type = fetch("entry_materialType"),
    material = "not-used",
    quantity = fetch("product_quantity"),
    unit = fetch("product_unit"),
    scenario = fetch("scenario_name")
  )
  
  if (add_place) {
    result <- cbind(result, place = fetch("place_name"))
  }
  
  result
}

# read_json_files --------------------------------------------------------------
read_json_files <- function(json_dir, files = NULL)
{
  if (is.null(files)) {
    files <- list_json_files_or_stop(json_dir)
  }
  
  files %>% 
    lapply(jsonlite::read_json) %>%
    stats::setNames(basename(files))
}

# list_json_files_or_stop ------------------------------------------------------
list_json_files_or_stop <- function(json_dir)
{
  json_files <- list.files(
    json_dir, 
    pattern = "\\.json$", 
    full.names = TRUE
  )
  
  if (length(json_files) < 1L) {
    kwb.utils::stopFormatted(
      "No result files (*.json) in folder '%s/'", json_dir
    )
  }
  
  json_files
}
