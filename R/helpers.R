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
