# to_tables --------------------------------------------------------------------
to_tables <- function(content)
{
  #content <- contents[[1L]]
  
  # Try this general function for the list-type elements
  # kwb.umberto:::flatten(content$products)
  # kwb.umberto:::flatten(content$entries)
  # kwb.umberto:::flatten(content$lifeCycleStages)
  # ...
  
  fetch <- kwb.utils::createAccessor(content)

  products <- fetch("products") %>%
    convert_and_bind(to_product) %>%
    prefix_columns("product_")
  
  entries <- fetch("entries") %>%
    convert_and_bind(to_entry) %>%
    kwb.utils::removeColumns("exchangeFullName") %>%
    prefix_columns("entry_")
  
  processes <- fetch("processes") %>%
    convert_and_bind(to_process) %>%
    prefix_columns("process_")
  
  places <- fetch("places") %>%
    convert_and_bind(to_place) %>%
    prefix_columns("place_")
  
  indicators <- fetch("indicators") %>%
    convert_and_bind(to_indicator) %>%
    prefix_columns("indicator_")
  
  scenarios <- fetch("scenarios") %>%
    convert_and_bind(to_scenario) %>%
    prefix_columns("scenario_")
  
  evaluationMethods <- fetch("evaluationMethods") %>%
    convert_and_bind(to_evaluationMethod) %>%
    prefix_columns("evaluationMethod_")
  
  result <- list(
    products = products,
    entries = entries,
    processes = processes,
    places = places,
    indicators = indicators,
    scenarios = scenarios,
    evaluationMethods = evaluationMethods
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
  #x <- lcia_list[[1L]];str(x)
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
  
  result_old <- cbind(flat, rbind(
    get_inout("input"),
    get_inout("output"),
    get_inout("internal")
  ))
  
  result <- flatten(x) %>%
    kwb.utils::renameColumns(list(
      inputs_outputs_internals = "side"
    ))
  
  result$side <- gsub("s$", "", result$side)
  
  check_identity(result_old, result)
  
  result
}

# to_entry ---------------------------------------------------------------------
to_entry <- function(x)
{
  #x <- contents[[1]]$entries[[1]]
  
  flat <- get_flat_part(x)
  
  result_old <- kwb.utils::removeColumns(flat, c(
    "X.id",
    "exchangeId", 
    "classificationId"
  ))
  
  result <- flatten(x) %>%
    kwb.utils::removeColumns(pattern = "(\\.i|I)d$")
  
  check_identity(
    result_old, 
    if (all(is.na(result$source))) {
      kwb.utils::removeColumns(result, "source")
    } else {
      result
    }
  )
  
  result
}

# to_process -------------------------------------------------------------------
to_process <- function(x)
{
  #x <- contents[[1L]]$processes[[1L]];str(x)
  
  result_old <- x %>%
    get_flat_part() %>%
    remove_xid()
  
  result <- flatten(x) %>%
    remove_xid()
  
  check_identity(result_old, result)
  
  result
}

# to_place ---------------------------------------------------------------------
to_place <- function(x)
{
  #x <- contents[[1L]]$places[[1L]];str(x)
  
  result_old <- x %>%
    get_flat_part() %>%
    remove_xid()
  
  result <- flatten(x) %>%
    remove_xid()

  check_identity(result_old, result)
  
  result
}

# to_indicator -----------------------------------------------------------------
to_indicator <- function(x)
{
  #x <- contents[[1L]]$indicators[[2L]];str(x)
  flat <- get_flat_part(x)
  
  path_parts <- get_remaining(x, flat) %>%
    kwb.utils::selectElements("path")
  
  stopifnot(all(lengths(path_parts) == 1L))
  
  result_old <- flat %>%
    remove_uuid() %>%
    cbind(indicatorPath = do.call(paste, c(path_parts, sep = "->")))
  
  result <- flatten(x, sep = "->") %>%
    remove_uuid() %>%
    kwb.utils::renameColumns(list(path = "indicatorPath"))

  check_identity(result_old, result)  

  result
}

# to_scenario ------------------------------------------------------------------
to_scenario <- function(x)
{
  #x <- contents[[1]]$scenarios[[1]];str(x)
  
  result_old <- x %>%
    get_flat_part() %>%
    remove_xid()

  result <- flatten(x) %>%
    remove_xid()

  check_identity(result_old, result)
  
  result
}

# to_evaluationMethod ----------------------------------------------------------
to_evaluationMethod <- function(x)
{
  #x <- contents[[1]]$evaluationMethods[[2]];str(x)
  
  result_old <- x %>%
    get_flat_part() %>%
    remove_xid() %>%
    remove_uuid()
  
  result <- flatten(x) %>%
    remove_xid() %>%
    remove_uuid()
  
  check_identity(result_old, result)
  
  result
}
