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

  # products2 <- fetch("products") %>%
  #   flatten() %>%
  #   ...
  # check_identity(products, products2)
  
  entries <- fetch("entries") %>%
    flatten() %>%
    kwb.utils::removeColumns(pattern = "(\\.i|I)d$") %>%
    kwb.utils::removeColumns("exchangeFullName") %>%
    prefix_columns("entry_")

  processes <- fetch("processes") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("process_")

  places <- fetch("places") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("place_")
  
  indicators <- fetch("indicators") %>%
    flatten(x, sep = "->") %>%
    remove_uuid() %>%
    kwb.utils::renameColumns(list(path = "indicatorPath")) %>%
    prefix_columns("indicator_")

  scenarios <- fetch("scenarios") %>%
    flatten() %>%
    remove_xid() %>%
    prefix_columns("scenario_")

  evaluationMethods <- fetch("evaluationMethods") %>%
    flatten() %>%
    remove_xid() %>%
    remove_uuid() %>%
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
  
  lcia <- lcia_list %>%
    flatten() %>%
    kwb.utils::renameColumns(list(
      entryId = "lciaEntryId",
      inputs_outputs_internals = "side"
    ))
  
  lcia$side <- gsub("s$", "", lcia$side)
  
  part_1 <- remove_uuid(flat)
  
  # We expect part_1 to have one row
  check_for_exactly_one_row(part_1)

  cbind(part_1[rep.int(1L, nrow(lcia)), , drop = FALSE], lcia)
}
