#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  # callModule(mod_original_server, "original_ui_1")
  callModule(mod_top_ui_server, "top_ui_ui_1")
  
}
