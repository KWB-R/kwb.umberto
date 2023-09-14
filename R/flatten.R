if (FALSE)
{
  xx <- kwb.umberto:::read_json_files("~/../Downloads/S/support/fabian/R-Umberto/Umberto11//")
  library(magrittr)
  
  is_list <- sapply(xx[[1]], is.list)
  
  results <- lapply(xx[[1]][is_list], flatten)
  
  r2 <- fhpredict:::flatten_recursive_list(xx[[1]]$products)
}

# flatten ----------------------------------------------------------------------
flatten <- function(x, name = NULL, sep = "|")
{
  # x must be a list
  check_for_list(x)
  
  # If the list is empty, return NULL
  if (length(x) == 0L) {
    return(NULL)
  }

  # Are the list entries lists themselves?
  is_list <- sapply(x, is.list)
  
  # Are the list elements named?
  is_named <- !is.null(names(x))
  
  # If no element is a list and all elements are of length one we return x, 
  # converted to a data frame
  if (!any(is_list) && all(lengths(x) == 1L)) {
    
    # If the elements are named, each element becomes a column
    if (is_named) {
      return(do.call(kwb.utils::noFactorDataFrame, x))
    } 
    
    # Otherwise, the "name" argument must be given. It is used as column name 
    # of the returned data frame
    stopifnot(!is.null(name))
    
    # List elements are concatenated with a separator to one string value
    result <- kwb.utils::noFactorDataFrame(do.call(paste, c(x, sep = sep)))
    
    # Name the (one and only) column
    return(stats::setNames(result, name))
  }

  # If the elements are not named, flatten and row-bind them 
  if (!is_named) {
    
    stopifnot(all(sapply(x, is.list)))
    stopifnot(all_have_identical_names(x))
    return(do.call(rbind, lapply(x, flatten, name = name, sep = sep)))
  }

  # Get the part that is already flat (get_flat_part(x))
  part_1 <- do.call(
    data.frame, 
    replace_null_with_na(x[!is_list])
  )

  # Names of the other elements (that are lists)
  elements <- names(which(is_list))
  
  # Loop through these elements, flatten them 
  part_2_tables <- elements %>%
    lapply(function(name) flatten(x[[name]], name = name, sep = sep)) %>%
    stats::setNames(elements)
  
  n_tables <- length(part_2_tables)
    
  # and row-bind them
  part_2 <- if (n_tables > 1L) {
    
    #do.call(rbind, part_2_tables)
    
    # Find a column name that does not yet exist
    name_column <- kwb.utils::hsSafeName(
      paste(elements, collapse = "_"),
      names(part_2_tables[[1L]])
    )
    
    kwb.utils::rbindAll(part_2_tables, name_column)
    
  } else if (n_tables == 1L) {
    
    part_2_tables[[1L]]
    
  } # else NULL

  if (is.null(part_2)) {
    return(part_1)
  }
  
  # We expect part_1 to have one row
  check_for_exactly_one_row(part_1)

  # Consider that part_2 may have no rows!
  cbind(part_1[rep.int(1L, nrow(part_2)), , drop = FALSE], part_2)
}
