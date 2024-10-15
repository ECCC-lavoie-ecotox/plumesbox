#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_search_ui <- function(id){
  ns <- NS(id)
  tagList(
    waiter::useWaiter(),
    waiter::waiterShowOnLoad(html = tagList(
      waiter::spin_inner_circles(),
      h5("Data fetching..."),
    )),
    bslib::page_fillable(
      bslib::layout_column_wrap(
        fill = FALSE,
        style = htmltools::css(grid_template_columns = "3fr 3fr 2fr"),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Spatial criterias", 
            class = "d-flex justify-content-between align-items-center bg-dark",
            div(class="btn-group",
              actionButton(ns("reset_filter_spatial"),
                class = "btn btn-success btn-sm m-1",
                icon = icon("arrows-rotate"),
                label = "Reset filters"
              ),
              actionButton(ns("apply_filter_spatial"),
                class = "btn btn-primary btn-sm m-1",
                label = "Apply"
              )
            )
          ),
          bslib::card_body(
            mapedit::selectModUI(ns("map")),
            class = "p-0"
          )
        ),
        bslib::card(
          bslib::card_header(
            "Search criterias", 
            class = "d-flex justify-content-between align-items-center bg-dark",
            div(class="btn-group",
              actionButton(ns("reset_filter_fields"),
                class = "btn btn-success btn-sm m-1",
                icon = icon("arrows-rotate"),
                label = "Reset filters"
              ),
              actionButton(ns("apply_filter_fields"),
                class = "btn btn-primary btn-sm m-1",
                label = "Apply"
              )
            )
          ),
          bslib::card_body(
            div(
              class = "container",
              div(
                class = "row",
                selectInput(ns("species"), "Species", choices = as.character(get_tbl(con, "species")$vernacular_en), multiple = TRUE, width = "50%"),
                selectInput(ns("sites"), "Sites", choices = as.character(get_tbl(con, "sites")$id_site), multiple = TRUE, width = "50%"),
              ),
              div(
                class = "row",
                radioButtons(
                  ns("search_by"),
                  "Search by",
                  c("Family" = "family", "Compound" = "compound"),
                  selected = "family",
                  inline = TRUE,
                  width = "30%"
                ),
                uiOutput(ns("compound"))
              ),
              div(
                class = "row",
                selectInput(ns("tissue"), "Tissue sample", choices = unique(as.character(get_tbl(con, "field_sample")$tissue)), multiple = TRUE, width = "50%"),
                selectInput(ns("age"), "Age", choices = unique(as.character(get_tbl(con, "field_sample")$age)), multiple = TRUE, width = "50%")
              ),
              div(
                class = "row",
                sliderInput(ns("sampling_range"), "Sampling period", min = min(as.Date(get_tbl(con, "field_sample")$collection_date, format = "%Y"), na.rm = TRUE), max = max(as.Date(get_tbl(con, "field_sample")$collection_date, format = "%Y"), na.rm = TRUE), value = c(min(as.Date(get_tbl(con, "field_sample")$collection_date, format = "%Y"), na.rm = TRUE), max(as.Date(get_tbl(con, "field_sample")$collection_date, , format = "%Y"), na.rm = TRUE)), width = "100%", timeFormat="%Y")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header("Options", class = "bg-dark"),
          bslib::card_body(
            div(
              class = "container",
              div(
                class = "row",
                selectInput(ns("tables"), "Include tables in export", choices = unname(unlist(get_golem_config("tables"))), multiple = TRUE, width = "100%")
              ),
              div(
                class = "row",
                div(class = "col-md-8",
                  "Simplify view"
                ),
                div(class = "col-md-4",
                  shinyWidgets::materialSwitch(
                    inputId = ns("simplify"),
                    status = "primary",
                    value = FALSE
                  )
                )
              ),
              div(
                class = "row",
                div(class = "col-md-8",
                  "Keep unknown / NA values?"
                ),
                div(class = "col-md-4",
                  shinyWidgets::materialSwitch(
                    inputId = "keep_na",
                    status = "primary",
                    value = FALSE
                  )
                )
              )
            )
          )
        )),
        bslib::card(
          min_height = "600px",
          bslib::card_header("Data",
            downloadButton(ns("download"), "Export in xlsx", class = "btn btn-success btn-sm m-1"),
            class = "bg-dark d-flex justify-content-between align-items-center"
          ),
          bslib::card_body(
            reactable::reactableOutput(ns("data"))
          )
        )
      )
    )
}
    
#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tables <- get_golem_config("tables") |>
      unlist() |>
      unname()

    init_datasets <- function() {
      lapply(tables, \(x) dplyr::tbl(con, x) |> dplyr::collect()) |>
        setNames(tables)
    }

    db <- reactiveValues(data = init_datasets(), simplified = NULL)

    w <- waiter::Waiter$new(html = tagList(
      waiter::spin_inner_circles(),
      h5("Data filtering...")
    ))

    ### Map section
    sites_sf <- get_tbl(con, "sites") |>
      dplyr::filter(!is.na(lat) & !is.na(lon)) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326), remove = FALSE)

    edits <- callModule(
      mapedit::editMod,
      leafmap = leaflet::leaflet() |>
        leaflet::addTiles(group = "OpenStreetMap") |>
        leaflet::addCircleMarkers(data = sites_sf, lng = ~lon, lat = ~lat, radius = 3, color = "#66003b", opacity = 0.8, fill = TRUE) |>
        leaflet.extras::addDrawToolbar(
          polylineOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE)
        ),
      id = "map",
      record = FALSE,
      sf = TRUE
    )

    observeEvent(input$search_by, {
      if (input$search_by == "family") {
        output$compound <- renderUI({
          selectInput(ns("family_compound"), "Family compounds", choices = unique(as.character(get_tbl(con, "analyte")$family)), multiple = TRUE, width = "70%")
        })
      } else {
        output$compound <- renderUI({
          selectInput(ns("compound"), "Specific compounds", choices = as.character(get_tbl(con, "analyte")$name), multiple = TRUE, width = "70%")
        })
      }
    })

    observe({
      nRes <- nrow(db$data$lab_measurement)
      if (nRes == 0) {
        showNotification(
          glue::glue("Found { nRes } measurements with these criterias."),
          type = "warning"
        )
      } else {
        showNotification(
          glue::glue("Found { nRes } measurements with these criterias."),
          type = "message"
        )
      }
    })

    observeEvent(input$apply_filter_fields, {
      w$show()
      # Set criterias
      ccond <- list(
        vernacular_en = input$species,
        id_site = input$sites,
        tissue = input$tissue,
        age = input$age,
        family = input$family_compound,
        name = input$compound
      ) |>
        purrr::discard(is.null) |>
        purrr::imap(
          function(slice, name) {
            colname <- rlang::sym(name)
            rlang::quo({{ colname }} %in% slice)
          }
        ) |>
        unname()

      # Execute textual criterias
      tmp_db <- get_db_cached(con) |>
        dplyr::filter(!!!ccond) |>
        dplyr::collect()

      # Execute date range criterias w$hide()
      if (length(input$sampling_range) == 2) {
        tmp_db <- tmp_db |>
          dplyr::mutate(collection_date = as.Date(collection_date)) |>
          dplyr::filter(collection_date >= input$sampling_range[1] & collection_date <= input$sampling_range[2])
      }

      # Split large db to original
      db$data <- purrr::map(tables, \(t){
        cn <- get_tbl_info(con, t)$name
        dplyr::select(tmp_db, any_of(cn)) |> dplyr::distinct()
      }) |> setNames(tables)

      w$hide()
    })

    output$data <- reactable::renderReactable({
      tmp <- db$data$lab_measurement |>
        dplyr::left_join(
          dplyr::select(
            db$data$analyte, id_analyte, name, unit, is_dry_weight, on_isolated_lipid
          ),
          by = "id_analyte"
        ) |>
        dplyr::left_join(
          dplyr::select(
            db$data$lab_field_sample, id_lab_sample, id_field_sample
          ),
          by = "id_lab_sample"
        ) |>
        dplyr::left_join(
          dplyr::select(
            db$data$field_sample, id_field_sample, id_site, id_species, age, tissue, collection_date
          ),
          by = "id_field_sample"
        ) |>
        dplyr::left_join(
          dplyr::select(
            db$data$species, id_species, vernacular_en
          ),
          by = "id_species"
        ) |>
        dplyr::group_by(across(c(-id_field_sample))) |>
        tidyr::nest() |>
        dplyr::rename(field_samples = data) |>
        dplyr::ungroup() 
      
      table1 <- tmp |>
        dplyr::select(
          Species = vernacular_en,
          id_species,
          Site = id_site,
          date = collection_date,
          Contaminant = name,
          Unit = unit,
          Value = value, 
          `% Lipid` = percent_lipid,
          `% Moisture` = percent_moisture,
          `Censored value?` = is_censored
        )
      
      reactable::reactable(table1, defaultPageSize = 100)

      # orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)

      # reactable::reactable(
      #   table1,
      #   columns = list(
      #     nMeas = reactable::colDef(
      #       name = "Count measurements",
      #       style = function(value) {
      #         normalized <- (value - min(table1$nMeas)) / (max(table1$nMeas) - min(table1$nMeas))
      #         color <- orange_pal(normalized)
      #         list(background = color)
      #       }
      #     ),
      #     nTrunc = reactable::colDef(
      #       name = "Count truncated",
      #       style = function(value) {
      #         normalized <- (value - min(table1$nTrunc)) / (max(table1$nTrunc) - min(table1$nTrunc))
      #         color <- orange_pal(normalized)
      #         list(background = color)
      #       }
      #     ),
      #     Species = reactable::colDef(
      #       cell = function(species, idx){
      #         return(paste0(species, " (", table1$id_species[idx] ,")"))
      #       }
      #     ),
      #     id_species = reactable::colDef(show = FALSE)
      #   ),
      #   defaultPageSize = 100
      # )

      # Table Digest - Tableau 2: Site, espèces, id_lab_sample, field_sample, contaminants in col
      # Fig with panel: Select site / species / contaminants = frequence histogramme
    })

    waiter::waiter_hide()
  })
}
