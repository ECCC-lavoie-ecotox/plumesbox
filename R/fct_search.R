#' Get full database to cascade filter
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

#' Get full database cacheds
#' @export
get_db_cached <- memoise::memoise(get_db)
