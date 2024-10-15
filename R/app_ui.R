#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Leave this function for adding external resources
    waiter::useWaiter()
    waiter::waiterPreloader()
    golem_add_external_resources()
    bslib::page_navbar(
      id = "page",
      window_title = "contaminR",
      title = a(class="navbar-brand",
          img(
            src = "www/logo.png",
            width = "80px"
          )),
      theme = bslib::bs_theme(
          bootswatch = "default",
          version = 5
      ),
      bslib::nav_panel("Dashboard", icon = shiny::icon("gauge")),
      bslib::nav_panel(
        "Search and export", 
        icon = shiny::icon("magnifying-glass"),
        mod_search_ui("search")
      ),
      bslib::nav_panel("View and edit tables", 
        icon = shiny::icon("table"),
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              mod_nav_tables_ui("table_nav"),
              open = "always"
            ), 
            mod_table_view_ui("table_viewer"))
        )
      ),
      bslib::nav_panel("Import data",  icon = shiny::icon("upload")),
      bslib::nav_menu(
        "Documentation",
        icon = shiny::icon("book"),
        bslib::nav_item(tags$a("R package")),
        bslib::nav_item(tags$a("How this database has been build?")),
      ),
      tags$head(
        tags$style(".card{ overflow: visible !important; }"),
        tags$style(".card-body{ overflow: visible !important; }")
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "plumesbox"
    )
  )
}
