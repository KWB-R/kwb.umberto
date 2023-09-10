if (FALSE)
{
  xx <- kwb.umberto:::read_json_files("~/../Downloads/S/support/fabian/R-Umberto/Umberto11//")
  library(magrittr)
  
  is_list <- sapply(xx[[1]], is.list)
  
  results <- lapply(xx[[1]][is_list], flatten)
  
  r2 <- fhpredict:::flatten_recursive_list(xx[[1]]$products)
}

# flatten ----------------------------------------------------------------------
flatten <- function(x, name = NULL)
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
    result <- kwb.utils::noFactorDataFrame(do.call(paste, c(x, sep = "|")))
    
    # Name the (one and only) column
    return(stats::setNames(result, name))
  }

  # If the elements are not named, flatten and merge them 
  if (!is_named) {
    
    stopifnot(all(sapply(x, is.list)))
    stopifnot(all_have_identical_names(x))
    
    return(do.call(rbind, lapply(x, flatten)))
  }

  # Get the part that is already flat (get_flat_part(x))
  part_1 <- do.call(
    data.frame, 
    replace_null_with_na(x[!is_list])
  )

  # Names of the other elements (that are lists)
  elements <- names(which(is_list))
  
  # Loop through these elements, flatten and row-bind them
  part_2 <- do.call(rbind, lapply(
    elements, function(name) flatten(x[[name]], name)
  ))
  
  if (is.null(part_2)) {
    return(part_1)
  }
  
  cbind(part_1, part_2)
}
