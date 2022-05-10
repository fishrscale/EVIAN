# --------------------------------------------
#
# generate_command.R
# Contains the generateCommand UI and Server functions.
#   Generates a command-line and print or launch it.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # generateCommandUI
# # generateCommandServer
#
# --------------------------------------------

# generateCommandUI
generateCommandUI <- function(id, 
                         button_text = "Launch/Generate",
                         placeholder_command_print = FALSE,
                         file_button_class = "generateCommandButt",
                         file_button_icon = icon("file-download")) {
  tagList(
    #script to close browser window if required - ui part
    tags$head(tags$script(HTML("Shiny.addCustomMessageHandler(
                                   'closeWindow', function(m) 
                                   {window.close();});"))),
    
    #action button
    actionButton(NS(id,"generate_command"), label = button_text, 
                 buttonType = "default", class = file_button_class, 
                 icon = file_button_icon),
    verbatimTextOutput(NS(id,"show_command"), 
                       placeholder = placeholder_command_print),
    checkboxInput(
      inputId = NS(id,"auto_close_window"),
      label = "Automatically close the app after launching the command?", 
      value = TRUE
    ),
    checkboxInput(
      inputId = NS(id,"print_command_instead"),
      label = "Print command instead of launching it?", 
      value = FALSE
    )
    
  )
}

# generateCommandServer
generateCommandServer <- function(id, 
                                  script_launch_command, 
                                  script_path, 
                                  param_named_vector,
                                  params_to_check = c(""),
                                  block_if_missing = FALSE,
                                  text_warning_check =
                                    "Some main parameters are missing!",
                                  text_warning_bypass = 
                                    "Generate report anyway?") {
  
  moduleServer(id, function(input, output, session) {
    confirmCommandGeneration <- reactiveVal(FALSE)
    commandToReturn <- reactiveVal("")
    
    observeEvent(input$generate_command, {
      
      param_tmp <- param_named_vector()[param_named_vector() != ""]
      if(length(param_tmp) > 0){
        argsToPrint <- paste0("--", names(param_tmp), "=", param_tmp, 
                              collapse = " ")
      } else {
        argsToPrint <- ""
      }
      commandToPrint <- paste(script_launch_command, 
                              script_path,
                              argsToPrint, 
                              sep = " ")
      
      # If some parameters to be checked are missing, return an error.
      missing_params <- which(!params_to_check %in% names(param_named_vector()))
      if(length(missing_params) > 0){
        stop(paste("Some parameters to be checked are missing! 
                 Check the application's code to fix this issue. 
                 The following parameter(s) is/are missing: ", 
                   paste(params_to_check[missing_params], collapse = " ")))
      } 
      
      # If some parameters to be checked are empty, return a warning alert.
      confirmCommandGeneration(FALSE)
      params_check <- param_named_vector()[
        names(param_named_vector()) %in% params_to_check]
      if(any(params_check == "")){
        # If check failed, should the command be automatically blocked 
        #   or ask user?
        if(block_if_missing){
          sendSweetAlert(
            session = session,
            title = "Error!",
            text = text_warning_check,
            type = "error"
          )
        } else {
          ask_confirmation(
            inputId = "bypass_warning_check",
            title = "Warning!",
            text = paste(text_warning_check,
                         text_warning_bypass, 
                         sep = " "),
            type = "warning"
          )
        }
        
      } else {
        confirmCommandGeneration(TRUE)
      }
      
      #Save the command to be returned
      commandToReturn(commandToPrint)
      
    })
    
    
    #If a confirm dialog is given, check user decision
    observeEvent(input$bypass_warning_check, {
      confirmCommandGeneration(input$bypass_warning_check)
    }, ignoreNULL = TRUE)
    
    #If command generation is confirmed, return the command
    observeEvent(confirmCommandGeneration(), {
      if(confirmCommandGeneration()){
        # Condition: either print command only or launch it
        if(input$print_command_instead){
          #Print command only
          output$show_command <- renderText({ commandToReturn() })
          
        } else {
          #Print command and launch it
          
          w = Waiter$new(
            html = tagList(
              spin_circle(),
              h1("Generating command..."),
              h2(
                ifelse(
                  input$auto_close_window,
                  "This window will now automatically close.",
                  "Please do not shutdown this page before the end of the task."
                )
              ),
              h4("Progression can be followed directly on the console.")
            )
          )
          
          w$show()
          
          Sys.sleep(5)
          
          # If window must be automatically closed, command is launched outside 
          #   of the app else it will directly use a system command.
          if(input$auto_close_window){
            #script to close browser window if required - server part
            session$sendCustomMessage(type = "closeWindow", message = "message")
            
            #Close App and retrieve command to launch
            stopApp(  commandToReturn()  )
            
          } else {
            
            #print command
            print( commandToReturn() )
            #launch command
            system( commandToReturn() )
            
          }
          
          w$hide()
        }
        
      }
    }, ignoreNULL = TRUE)
    
    
  })
}

