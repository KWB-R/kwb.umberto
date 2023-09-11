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

  result <- flatten(x) %>%
    kwb.utils::renameColumns(list(
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
