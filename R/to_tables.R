# to_tables --------------------------------------------------------------------
to_tables <- function(content)
{
  #content <- contents[[1L]]
  
  fetch <- kwb.utils::createAccessor(content)
  
  result <- list(
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
  
  structure(
    result, 
    timestamp = fetch("timestamp")
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

