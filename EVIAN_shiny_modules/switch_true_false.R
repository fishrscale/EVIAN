# --------------------------------------------
#
# switch_true_false.R
# Contains the switchTrueFalse UI and Server functions.
#   Generates a customized switch button.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # switchTrueFalseUI
# # switchTrueFalseServer
#
# --------------------------------------------

# switchTrueFalseUI
#If onStatus or offStatus is/are used, 
# status must be one of the following default status:
#   c("info", "success", "danger", "primary", "warning")
switchTrueFalseUI <- function(id, 
                              switch_title = "Condition name",
                              default_value = FALSE, 
                              onStatus = NULL, 
                              offStatus = NULL,
                              customOnColor = "#000000",
                              customOffColor = "#000000",
                              customOnBackColor = "#FFFFFF",
                              customOffBackColor = "#FFFFFF") {
  tagList(
    
    #Define custom status and associated colors for switch ---
    tags$head(tags$style(paste0(
      ".bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-",id,"_on {
        background: ",customOnBackColor,";
        color: ",customOnColor,";
      }"
    ))),
    tags$head(tags$style(paste0(
      ".bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-",id,"_off {
        background: ",customOffBackColor,";
        color: ",customOffColor,";
      }"
    ))),
    
    #Switch ---
    h5(switch_title),
    switchInput(inputId = NS(id,"switch"),  
                value = default_value, 
                onStatus = ifelse(is.null(onStatus), 
                                  paste0(id, "_on"), onStatus),
                offStatus = ifelse(is.null(offStatus), 
                                   paste0(id, "_off"), offStatus))
    
  )
}

# switchTrueFalseServer
switchTrueFalseServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive(input$switch))
  })
}


