# --------------------------------------------
#
# close_button_header.R
# Contains the closeButtonHeader UI and Server functions.
#   Generates an action button that closes the application if 
#   the user confirm th following window.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # closeButtonHeaderUI
# # closeButtonHeaderServer
#
# --------------------------------------------

# closeButtonHeaderUI
closeButtonHeaderUI <- function(id) {
  actionButton(NS(id, "close_button"), 
               "Close interface")
}

# closeButtonHeaderServer
closeButtonHeaderServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    #If the close button is activated, ask user for confirmation
    observeEvent(input$close_button, {
      
      ask_confirmation(
        inputId = "confirm_close",
        title = "Close the interface?",
        text = "Do you want to close the interface? 
        Any unsaved progress will be lost.",
        type = "info"
      )
      
    })
    
    #If a confirm dialog is given, check user decision
    observeEvent(input$confirm_close, {
      if(input$confirm_close){
        #script to close browser window if required - server part
        session$sendCustomMessage(type = "closeWindow", message = "message")
        
        #Close App
        stopApp()
      }
    }, ignoreNULL = TRUE)
    
  })
}

