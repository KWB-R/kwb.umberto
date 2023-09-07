# merge_json_tables ------------------------------------------------------------
merge_json_tables <- function(tables)
{
  fetch <- kwb.utils::createAccessor(tables)
  
  fetch("products") %>%
    dplyr::left_join(
      kwb.utils::selectColumns(fetch("entries"), c(
        "entry_id",
        "entry_exchange",
        "entry_unit"
      )),
      by = c(product_entryId = "entry_id")
    ) %>%
    kwb.utils::renameColumns(list(
      entry_exchange = "ref_flow_exchange",
      entry_unit = "ref_flow_unit"
    )) %>%
    dplyr::left_join(
      fetch("entries"),
      by = c(product_lciaEntryId = "entry_id")
    ) %>%
    dplyr::left_join(
      fetch("scenarios"),
      by = c(product_scenarioId = "scenario_id")
    ) %>%
    dplyr::left_join(
      fetch("indicators"),
      by = c(product_indicatorId = "indicator_id")
    ) %>%
    dplyr::left_join(
      fetch("processes"),
      by = c(product_processId = "process_id")
    ) %>%
    dplyr::left_join(
      fetch("places"), 
      by = c(product_placeId = "place_id")
    ) %>%
    kwb.utils::moveColumnsToFront(c(
    "scenario_name"
    , "ref_flow_exchange"
    , "product_amount"
    , "ref_flow_unit" 
    , "product_arrow" 
    , "indicator_indicatorPath"
    , "indicator_name" 
    , "process_name"
    , "place_name"
    , "entry_exchange"
    , "entry_exchangeContext"
    , "product_quantity"
    , "product_unit"
    , "entry_materialType"
    , "product_side"
    , "entry_costItemGroup"
    , "entry_exchangeBoundary"
  )) %>%
    move_id_columns_right()
}

# move_id_columns_right --------------------------------------------------------
move_id_columns_right <- function(df)
{
  columns <- names(df)
  
  is_id <- grepl("(Id|Index)$", columns)
  
  df[, c(columns[!is_id], columns[is_id]), drop = FALSE]
}
