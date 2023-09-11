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

  entries2 <- fetch("entries") %>%
    flatten() %>%
    kwb.utils::removeColumns(pattern = "(\\.i|I)d$") %>%
    kwb.utils::removeColumns("exchangeFullName") %>%
    prefix_columns("entry_")
  
  check_identity(entries, entries2)

  processes <- fetch("processes") %>%
    convert_and_bind(to_process) %>%
    prefix_columns("process_")

  processes2 <- fetch("processes") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("process_")
  
  check_identity(processes, processes2)

  places <- fetch("places") %>%
    convert_and_bind(to_place) %>%
    prefix_columns("place_")
  
  places2 <- fetch("places") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("place_")
  
  check_identity(places, places2)
  
  indicators <- fetch("indicators") %>%
    convert_and_bind(to_indicator) %>%
    prefix_columns("indicator_")

  indicators2 <- fetch("indicators") %>%
    flatten(x, sep = "->") %>%
    remove_uuid() %>%
    kwb.utils::renameColumns(list(path = "indicatorPath")) %>%
    prefix_columns("indicator_")

  check_identity(indicators, indicators2)
  
  scenarios <- fetch("scenarios") %>%
    convert_and_bind(to_scenario) %>%
    prefix_columns("scenario_")

  scenarios2 <- fetch("scenarios") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("scenario_")

  check_identity(scenarios, scenarios2)
  
  evaluationMethods <- fetch("evaluationMethods") %>%
    convert_and_bind(to_evaluationMethod) %>%
    prefix_columns("evaluationMethod_")
  
  evaluationMethods2 <- fetch("evaluationMethods") %>%
    flatten() %>%
    remove_xid() %>%
    remove_uuid() %>%
    prefix_columns("evaluationMethod_")
  
  check_identity(evaluationMethods, evaluationMethods2)
  
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
  
  renaming <- list("entryId" = "lciaEntryId")
  
  lcia_old <- lcia_list %>%
    convert_and_bind(to_lcia) %>%
    kwb.utils::renameColumns(renaming)
  
  lcia <- lcia_list %>%
    flatten() %>%
    prettify_column_side() %>%
    kwb.utils::renameColumns(renaming)
  
  check_identity(lcia_old, lcia)
  
  result_old <- cbind(remove_uuid(flat), lcia)
  
  #result <- remove_uuid(flatten(x))
  #check_identity(result_old, result)
  
  result_old
}

# to_lcia ----------------------------------------------------------------------
to_lcia <- function(x)
{
  #x <- lcia_list[[1L]];str(x)

  flatten(x) %>%
    prettify_column_side()
}

# prettify_column_side ---------------------------------------------------------
prettify_column_side <- function(data)
{
  result <- kwb.utils::renameColumns(data, list(
    inputs_outputs_internals = "side"
  ))
  
  result$side <- gsub("s$", "", result$side)
  
  result
}

# to_entry ---------------------------------------------------------------------
to_entry <- function(x)
{
  #x <- contents[[1]]$entries[[1]]
  
  flatten(x) %>%
    kwb.utils::removeColumns(pattern = "(\\.i|I)d$")
}

# to_process -------------------------------------------------------------------
to_process <- function(x)
{
  #x <- contents[[1L]]$processes[[1L]];str(x)
  
  flatten(x) %>%
    remove_xid()
}

# to_place ---------------------------------------------------------------------
to_place <- function(x)
{
  #x <- contents[[1L]]$places[[1L]];str(x)
  
  flatten(x) %>%
    remove_xid()
}

# to_indicator -----------------------------------------------------------------
to_indicator <- function(x)
{
  #x <- contents[[1L]]$indicators[[2L]];str(x)
  flatten(x, sep = "->") %>%
    remove_uuid() %>%
    kwb.utils::renameColumns(list(path = "indicatorPath"))
}

# to_scenario ------------------------------------------------------------------
to_scenario <- function(x)
{
  #x <- contents[[1]]$scenarios[[1]];str(x)
  
  flatten(x) %>%
    remove_xid()
}

# to_evaluationMethod ----------------------------------------------------------
to_evaluationMethod <- function(x)
{
  #x <- contents[[1]]$evaluationMethods[[2]];str(x)
  
  flatten(x) %>%
    remove_xid() %>%
    remove_uuid()
}
