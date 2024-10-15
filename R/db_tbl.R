#' Insert a new row into a table
#'
#' Inserts a new row into a specified table in the database.
#' It ensures that fields, primary keys, and NOT NULL constraints are valid before inserting.
#'
#' @param con DBI connection object.
#' @param tbl Name of the target table.
#' @param ... Named column-value pairs to insert into the table.
#' 
#' @return None.
#' 
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' insert_entry(con, "my_table", id = 1, name = "Sample", value = 10)
#' }
#' 
#' @export
insert_entry <- function(con = NULL, tbl = NULL, ...) {
    fields <- list(...)
    
    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))
    check_fields_notnulls(con, tbl, names(fields))
    
    ddl <- glue::glue_sql("INSERT INTO { tbl } ({ names(fields)* }) VALUES ({ fields* });", .con = con)
    res <- DBI::dbSendStatement(con, ddl)
    
    on.exit(DBI::dbClearResult(res))
}

#' Insert multiple rows into a table
#'
#' Performs bulk insertion into a table using transactions. 
#' Rolls back if any errors occur.
#'
#' @param con DBI connection object.
#' @param tbl Name of the target table.
#' @param data A data.frame with rows to insert.
#' 
#' @return None.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' data <- data.frame(id = 1:3, name = c("A", "B", "C"), value = c(10, 20, 30))
#' bulk_insert_entry(con, "my_table", data)
#' }
#' 
#' @export
bulk_insert_entry <- function(con = NULL, tbl = NULL, data = NULL) {
    tryCatch({
        RSQLite::dbBegin(con, name = "bulk_insert")
        purrr::pmap(data, ~with(list(...), {
            insert_entry(con = con, tbl = tbl, ...)
        }))
        RSQLite::dbCommit(con, name = "bulk_insert")
    }, error = \(e) {
        RSQLite::dbRollback(con, name = "bulk_insert")
        cli::cli_abort(e)
    })
}

#' Modify a row in a table
#'
#' Updates an existing row based on primary keys. Only one row is updated.
#'
#' @param con DBI connection object.
#' @param tbl Name of the target table.
#' @param ... Named column-value pairs for the row to update.
#'
#' @return None.
#' 
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' modify_entry(con, "my_table", id = 1, name = "Updated", value = 20)
#' }
#' 
#' @export
modify_entry <- function(con = NULL, tbl = NULL, ...) {
    fields <- list(...)
    
    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))
    
    pkeys_tbl <- get_tbl_pkeys(con, tbl)
    target_row <- do.call("search_tbl", list(con = con, tbl = tbl) |> append(fields[pkeys_tbl]))
    
    if (nrow(target_row) > 1L) {
        cli::cli_abort("More than one row found with { fields[pkeys_tbl] }")
    } else {
        pkeys_values <- fields[which(names(fields) %in% pkeys_tbl)]
        update_values <- fields[-which(names(fields) %in% pkeys_tbl)]
        
        update_entries <- purrr::map(names(update_values), \(n) {
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(",")
        
        criterias <- purrr::map(names(pkeys_values), \(n) {
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(" AND ")
        
        ddl <- glue::glue_sql("
            UPDATE { tbl }
            SET { update_entries }
            WHERE { criterias };
        ", .con = con)
        
        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)
        
        on.exit(DBI::dbClearResult(res))
    }
}

#' Delete a row from a table
#'
#' Deletes a row from a table based on primary keys.
#'
#' @param con DBI connection object.
#' @param tbl Name of the target table.
#' @param ... Named primary key column-value pairs for the row to delete.
#'
#' @return None.
#' 
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' delete_entry(con, "my_table", id = 1)
#' }
#' 
#' @export
delete_entry <- function(con = NULL, tbl = NULL, ...) {
    fields <- list(...)
    
    check_fields_exist(con, tbl, names(fields))
    check_fields_pkeys(con, tbl, names(fields))
    
    pkeys_tbl <- get_tbl_pkeys(con, tbl)
    target_row <- do.call("search_tbl", list(con = con, tbl = tbl) |> append(fields[pkeys_tbl]))
    
    if (nrow(target_row) > 1L) {
        cli::cli_abort("More than one row found with { fields[pkeys_tbl] }")
    } else {
        criterias <- purrr::map(names(fields[pkeys_tbl]), \(n) {
            glue::glue("{n} = ${n}")
        }) |> glue::glue_sql_collapse(" AND ")
        
        ddl <- glue::glue_sql("
            DELETE 
            FROM { tbl }
            WHERE { criterias };
        ", .con = con)
        
        res <- DBI::dbSendStatement(con, ddl)
        DBI::dbBind(res, fields)
        
        on.exit(DBI::dbClearResult(res))
    }
}


#' Retrieve data from a specified table in the database
#'
#' This function reads and returns all the rows from a specified table in the database.
#' It uses [DBI::dbReadTable()] to extract the contents of the table and return them as a data frame.
#'
#' @param con A DBI connection object returned by [DBI::dbConnect()]. This connection is used to interact with the database.
#' @param tbl A character string specifying the name of the target table in the database.
#'
#' @return A data frame containing all rows and columns from the specified table.
#' If the table is empty or does not exist, an error or empty data frame will be returned.
#'
#' @export
get_tbl <- function(con = NULL, tbl = NULL) {
    DBI::dbReadTable(con, tbl)
}

#' Retrieve joined data from the database
#'
#' This function retrieves data by performing a series of `INNER JOIN` operations across multiple tables in the database, 
#' including `lab_measurement`, `analyte`, `lab_sample`, `lab_field_sample`, `field_sample`, `sites`, and `species`.
#' The result is a combined dataset containing related information from all these tables.
#'
#' @param con A DBI connection object. The database connection used to query the data.
#'
#' @return A lazy data frame (tibble) representing the joined data. The query is not executed until explicitly requested by the user (e.g., with `dplyr::collect()`).
#'
#' @export
get_db <- function(con = NULL) {
    dplyr::tbl(
        con, dplyr::sql("
        SELECT * FROM lab_measurement
            INNER JOIN analyte USING (id_analyte)
            INNER JOIN lab_sample USING (id_lab_sample)
            INNER JOIN lab_field_sample USING (id_lab_sample)
            INNER JOIN field_sample USING (id_field_sample)
            INNER JOIN sites USING (id_site)
            INNER JOIN species USING (id_species)")
    )
}
