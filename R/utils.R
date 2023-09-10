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

# remove_zero_length_entries ---------------------------------------------------
remove_zero_length_entries <- function(x)
{
  check_for_list(x)
  
  x[lengths(x) > 0L]
}

# replace_null_with_na ---------------------------------------------------------
replace_null_with_na <- function(x)
{
  stopifnot(is.list(x))
  
  is_null <- sapply(x, is.null)
  
  x[is_null] <- as.list(rep(NA, sum(is_null)))
  
  x
}
