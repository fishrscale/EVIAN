# --------------------------------------------
#
# get_path_input.R
# Contains the getPathInput UI and Server functions.
#   Generates a search path button and print+return value.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # getPathInputUI
# # getPathInputServer
#
# --------------------------------------------

# getPathInputUI
getPathInputUI <- function(id, 
                           file_button_title = "Select files...",
                           file_button_text = "Select files...",
                           title_choose_panel = "Select files...",
                           multiple_files = FALSE, 
                           file_button_class = "pathButt",
                           file_button_icon = icon("file"), 
                           text_path_class = "fileLoaded") {
  tagList(
    h5(file_button_title),
    shinyFilesButton(NS(id,"path"), 
                     label = file_button_text,
                     title = title_choose_panel, 
                     multiple = multiple_files, 
                     buttonType = "default", class = file_button_class, 
                     icon = file_button_icon),
    tagAppendAttributes(textOutput(NS(id,"data_path")), 
                        class = text_path_class),
    
  )
}

# getPathInputServer
getPathInputServer <- function(id, roots = reactive(c(".", "~"))) {
  
  moduleServer(id, function(input, output, session) {
    data_path <- reactiveVal("")
    
    #Input retrieve and check file paths ----
    observe({  
      shinyFileChoose(input, "path", 
                      roots = roots, 
                      session = session) 
      
      if(!is.null(input$path)){
        #Retrieve and show path ----
        data_path(
          paste0(
            parseFilePaths(roots, input$path)$datapath,
            collapse=","
          )
        )
        output$data_path <- renderText(data_path())
        
      }
      
    })
    
    return(data_path)
  })
}

