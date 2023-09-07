if (FALSE)
{
  json_dir = "~/../Downloads/S/support/fabian/R-Umberto/Umberto11"

  result <- import_rawdata_json(json_dir)
  
  View(result)
}

# import_rawdata_json ----------------------------------------------------------
import_rawdata_json <- function(json_dir)
{
  contents <- read_json_files(json_dir)
  
  result <- lapply(contents, to_tables)
  
  data_frames <- lapply(result, merge_json_tables)
  
  do.call(rbind, data_frames)
}

# read_json_files --------------------------------------------------------------
read_json_files <- function(json_dir)
{
  json_files <- list_json_files_or_stop(json_dir)
  
  lapply(json_files, jsonlite::read_json)
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

# to_tables --------------------------------------------------------------------
to_tables <- function(content)
{
  #content <- contents[[1L]]
  
  fetch <- kwb.utils::createAccessor(content)
  
  list(
    products = fetch("products") %>%
      convert_and_bind(to_product) %>%
      prefix_columns("product_"),
    entries = fetch("entries") %>%
      convert_and_bind(to_entry) %>%
      kwb.utils::removeColumns("exchangeFullName") %>%
      prefix_columns("entry_"),
    processes = fetch("processes") %>%
      convert_and_bind(to_process) %>%
      prefix_columns("process_"),
    places = fetch("places") %>%
      convert_and_bind(to_place) %>%
      prefix_columns("place_"),
    indicators = fetch("indicators") %>%
      convert_and_bind(to_indicator) %>%
      prefix_columns("indicator_"),
    scenarios = fetch("scenarios") %>%
      convert_and_bind(to_scenario) %>%
      prefix_columns("scenario_"),
    evaluationMethods = fetch("evaluationMethods") %>%
      convert_and_bind(to_evaluationMethod) %>%
      prefix_columns("evaluationMethod_")
  )
}

# to_product -------------------------------------------------------------------
to_product <- function(x)
{
  #x <- contents[[1]]$products[[1]]
  
  flat <- get_flat_part(x)
  
  lcia_list <- get_remaining(x, flat) %>%
    remove_zero_length_entries() %>%
    kwb.utils::selectElements("lcia")
  
  #str(lcia_list)
  
  #convert_and_bind(lcia_list, get_flat_part)
  
  lcia <- lcia_list %>%
    convert_and_bind(to_lcia) %>%
    kwb.utils::renameColumns(list(
      "entryId" = "lciaEntryId"
    ))
  
  cbind(
    flat %>%
      remove_uuid(), 
    lcia
  )
}

# get_flat_part ----------------------------------------------------------------
get_flat_part <- function(x)
{
  check_for_list(x)
  
  is_flat <- lengths(x) == 1L & !sapply(x, is.list)
  
  if (!any(is_flat)) {
    return(NULL)
  }
  
  as.data.frame(x[is_flat])
}

# check_for_list ---------------------------------------------------------------
check_for_list <- function(x)
{
  stopifnot(is.list(x))
}

# check_for_data_frame ---------------------------------------------------------
check_for_data_frame <- function(x)
{
  stopifnot(is.data.frame(x))
}

# get_remaining ----------------------------------------------------------------
get_remaining <- function(x, flat_part)
{
  check_for_list(x)
  check_for_data_frame(flat_part)
  
  x[setdiff(names(x), names(flat_part))]
}

# remove_zero_length_entries ---------------------------------------------------
remove_zero_length_entries <- function(x)
{
  check_for_list(x)
  
  x[lengths(x) > 0L]
}

# to_lcia ----------------------------------------------------------------------
to_lcia <- function(x)
{
  #x <- lcia_list[[2L]]
  flat <- get_flat_part(x)
  
  remaining <- get_remaining(x, flat) %>%
    remove_zero_length_entries()
  
  #str(remaining, 2)
  
  get_inout <- function(side) {
    sublist <- remaining[[paste0(side, "s")]]
    if (!is.null(sublist)) {
      sublist %>%
        convert_and_bind(get_flat_part) %>%
        cbind(side = side)
    } # else NULL
  }
  
  inputs <- get_inout("input")
  outputs <- get_inout("output")
  
  cbind(flat, rbind(inputs, outputs))
}

# to_entry ---------------------------------------------------------------------
to_entry <- function(xz)
{
  #xz <- contents[[1]]$entries[[1]]
  flat <- get_flat_part(xz)
  
  kwb.utils::removeColumns(flat, c(
    "X.id",
    "exchangeId", 
    "classificationId"
  )) 
}

# to_process -------------------------------------------------------------------
to_process <- function(x)
{
  x %>%
    get_flat_part() %>%
    remove_xid()
}

# remove_xid -------------------------------------------------------------------
remove_xid <- function(df)
{
  kwb.utils::removeColumns(df, "X.id")
}

# remove_uuid ------------------------------------------------------------------
remove_uuid <- function(df)
{
  kwb.utils::removeColumns(df, "uuid")
}

# to_place ---------------------------------------------------------------------
to_place <- function(x)
{
  x %>%
    get_flat_part() %>%
    remove_xid()
}

# to_scenario ------------------------------------------------------------------
to_scenario <- function(x)
{
  x %>%
    get_flat_part() %>%
    remove_xid()
}

# to_evaluationMethod ----------------------------------------------------------
to_evaluationMethod <- function(x)
{
  x %>%
    get_flat_part() %>%
    remove_xid() %>%
    remove_uuid()
}

# to_indicator -----------------------------------------------------------------
to_indicator <- function(x)
{
  #x <- contents[[1L]]$indicators[[1L]]
  flat <- get_flat_part(x)
  
  path_parts <- get_remaining(x, flat) %>%
    kwb.utils::selectElements("path")
  
  stopifnot(all(lengths(path_parts) == 1L))
  
  flat %>%
    remove_uuid() %>%
    cbind(indicatorPath = paste0(path_parts[[1L]], "->", path_parts[[2L]]))
}

# convert_and_bind -------------------------------------------------------------
convert_and_bind <- function(x_list, converter)
{
  lapply(x_list, converter) %>%
    kwb.utils::safeRowBindAll()
}

# flatten_all ------------------------------------------------------------------
flatten_all <- function(x)
{
  do.call(rbind, lapply(x, flatten))  
}

# flatten ----------------------------------------------------------------------
flatten <- function(x, depth = 0L, max_depth = 3L)
{
  if (depth > max_depth) {
    return(x)
  }
  
  check_for_list(x)
  
  # If the list is empty, return NULL
  if (length(x) == 0L) {
    return(NULL)
  }
  
  # If the elements are not named, flatten and merge them with a
  # specific function
  if (is.null(names(x))) {
    return(flatten_list_of_unnamed_elements(x))
  }
  
  # Get the part that is already flat
  part_1 <- get_flat_part(x)
  
  remaining <- get_remaining(x, part_1)
  
  # Loop through the other parts
  part_2 <- if (length(remaining)) {
    
    result <- lapply(names(remaining), function(element) {
      cat(kwb.utils::indent(element, depth), "\n")
      flatten(
        remaining[[element]],
        depth = depth + 1L, 
        max_depth = max_depth
      )
    })
    
    names(result) <- elements
    
    result <- kwb.utils::excludeNULL(result, dbg = FALSE)
  } # else NULL
  
  if (is.null(part_2)) {
    return(part_1)
  } 
  
  if (kwb.utils::allAreIdentical(lapply(part_2, names))) {
    part_2 <- do.call(rbind, part_2)
    kwb.utils::resetRowNames(
      cbind(data.frame(id = rownames(part_2)), part_2)
    )
  }
  
  if (is.null(part_1)) {
    return(part_2)
  }
  
  if (
    is.data.frame(part_1) && 
    is.data.frame(part_2) && 
    nrow(part_1) == 1L
  ) {
    return(cbind(part_1, part_2))
  }
  
  list(part_1 = part_1, part_2 = part_2)
}

# flatten_list_of_unnamed_elements ---------------------------------------------
flatten_list_of_unnamed_elements <- function(x)
{
  stopifnot(!is.null(names(x)))
  
  # We expect all elements to be lists with identical names
  stopifnot(all_have_identical_names(x))
  
  do.call(rbind, lapply(x, flatten))
}

# all_have_identical_names -----------------------------------------------------
all_have_identical_names <- function(x)
{
  suppressMessages(kwb.utils::allAreIdentical(lapply(x, names)))
}

# prefix_columns ---------------------------------------------------------------
prefix_columns <- function(df, prefix = deparse(substitute(df)))
{
  check_for_data_frame(df)
  
  stats::setNames(df, paste0(prefix, names(df)))
}
