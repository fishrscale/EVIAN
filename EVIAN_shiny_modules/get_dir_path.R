# --------------------------------------------
#
# get_dir_path.R
# Contains the getDirPath UI and Server functions.
#   Generates a search dir button and print+return value.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # getDirPathUI
# # getDirPathServer
#
# --------------------------------------------

# getDirPathUI
getDirPathUI <- function(id, 
                         dir_button_title = "Select directory...",
                         dir_button_text = "Select directory...",
                         title_choose_panel = "Select directory...",
                         file_button_class = "pathButt",
                         file_button_icon = icon("folder")) {
  tagList(
    h4(dir_button_title),
    shinyDirButton(NS(id,"path"), 
                   label = dir_button_text,
                   "Upload",
                   title = title_choose_panel,
                   class = file_button_class, 
                   icon = file_button_icon),
    verbatimTextOutput(NS(id,"dir_path"), placeholder = TRUE)
    
  )
}

# getDirPathServer
getDirPathServer <- function(id, roots = reactive(c(".", "~")),
                             default_directory = ".", 
                             filetypes_to_show = c('', 'txt', 'bigWig', "tsv", 
                                                   "csv", "bw")) {
  
  moduleServer(id, function(input, output, session) {
    dir_path <- reactiveVal("")
    
    #Input retrieve and check file paths ----
    observe({  
      shinyDirChoose(
        input, 'path',
        roots = roots(),
        session = session,
        filetypes = filetypes_to_show
      )
      
      directory_path <- parseDirPath(roots(), input$path) 
      
      #If directory_path is null or no path provided, set dir to current ----
      if(!is.null(directory_path)){
        if(length(directory_path)==0){
          directory_path <- default_directory
        }
      } else{
        directory_path <- default_directory
      }

      #Print output directory selected
      output$dir_path <- renderText({directory_path})
      
      dir_path(directory_path)
    })
    
    return(dir_path)
  })
}


