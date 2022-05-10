# --------------------------------------------
#
# get_path_filt_cols_input.R
# Contains the getPathFiltColsInput UI and Server functions.
#   Generates a search path button, a pickerInput to select the columns 
#   to keep and print+return value.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # getPathFiltColsInputUI
# # getPathFiltColsInputServer
#
# --------------------------------------------

# getPathFiltColsInputUI
getPathFiltColsInputUI <- function(id, 
                                   file_button_title = "Select files...",
                                   file_button_text = "Select files...",
                                   title_choose_panel = "Select files...",
                                   title_picker_input = "Select columns...",
                                   multiple_files = TRUE, 
                                   select_multiple_columns = TRUE,
                                   choices_picker_input = "",
                                   options_picker_input = 
                                     list(
                                       `live-search` = TRUE,
                                       `actions-box` = TRUE
                                       ),
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
    tagAppendAttributes(textOutput(NS(id,"data_paths")), 
                        class = text_path_class),
    
    br(),
    
    pickerInput(
      inputId = NS(id,"columns_to_keep"),
      label = h5(title_picker_input), 
      choices = choices_picker_input,
      options = options_picker_input, 
      multiple = select_multiple_columns
    )
    
    
  )
}

# getPathFiltColsInputServer
getPathFiltColsInputServer <- function(id, roots = reactive(c(".", "~"))) {
  
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
        output$data_paths <- renderText(data_path())
        
        #Load header of file(s) to retrieve column names --------------------------------
        data_path_split <- unlist(strsplit(data_path(), split = ","))
        
        #If some path(s)
        if(length(data_path_split) > 0){
          
          #load header of first path
          data_head = read.csv(data_path_split[1], row.names = 1, 
                               header=TRUE, nrows = 1)
          
          #If more than one path, combine with other headers
          if(length(data_path_split) > 1){
            for (x in data_path_split[-1]) {
              data_head_tmp = read.csv(x, row.names = 1, 
                                       header=TRUE, nrows = 1)
              data_head <- cbind(data_head, 
                                 data_head_tmp[
                                   match(rownames(data_head), 
                                         rownames(data_head_tmp)),])
            }
          }
          
          #Update filter to select some columns
          updatePickerInput(session = session, 
                            inputId = "columns_to_keep", 
                            choices = colnames(data_head))
          
        } else {
          #If no path, update filter to show that no columns can be selected
          updatePickerInput(session = session, 
                            inputId = "columns_to_keep", 
                            choices = "")
        }
        
      }
      
    })
    
    return(list(
      "data_path" = data_path,
      "columns_to_keep" = reactive(input$columns_to_keep)
      ))
  })
}

