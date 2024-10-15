#' Initialize the PLUMES database connection
#'
#' Connects to the PLUMES database file. If `local` is provided, it uses the specified file path, 
#' otherwise it defaults to the internal database path and name. 
#' The function ensures the database file exists before connecting.
#'
#' @param local Optional. Path to a local SQLite database file. Defaults to `NULL` for the internal path.
#'
#' @return A DBI connection object.
#' 
#' @examples
#' \dontrun{
#' # Connect to a local database
#' con <- init_con(local = "path/to/local/database.sqlite")
#' 
#' # Connect using default settings
#' con <- init_con()
#' }
#'
#' @export
init_con <- function(local = NULL){
    if(!is.null(local)){
        db <- file.path(local)
    } else (
        db <- file.path(db_path(), db_name())
    )
    stopifnot(db |> file.exists())
    return(DBI::dbConnect(RSQLite::SQLite(), db))
}
