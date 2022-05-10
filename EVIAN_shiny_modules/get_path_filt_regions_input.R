# --------------------------------------------
#
# get_path_filt_regions_input.R
# Contains the getPathFiltRegionsInput UI and Server functions.
#   Generates a search path button, pickerInputs to filter the regions 
#   provided and print+return value.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # getPathFiltRegionsInputUI
# # getPathFiltRegionsInputServer
#
# --------------------------------------------

# getPathFiltRegionsInputUI
getPathFiltRegionsInputUI <- function(id, 
                                   file_button_title = "Select files...",
                                   file_button_text = "Select files...",
                                   title_choose_panel = "Select files...",
                                   title_group_input = "Select groups...",
                                   title_status_input = "Select status...",
                                   title_control_input = "Select control groups...",
                                   multiple_files = TRUE, 
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
      inputId = NS(id,"regions_group_to_check"),
      label = h5(title_group_input),
      choices = rep("",1),
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE), 
      multiple = TRUE
    ),
    
    pickerInput(
      inputId = NS(id,"regions_status"),
      label = h5(title_status_input),
      choices = rep("",1),
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE), 
      multiple = TRUE
    ),
    
    pickerInput(
      inputId = NS(id,"group_as_ctrls"),
      label = h5(title_control_input),
      choices = rep("",1),
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE), 
      multiple = TRUE
    )
    
    
  )
}

# getPathFiltRegionsInputServer
getPathFiltRegionsInputServer <- function(id, roots = reactive(c(".", "~"))) {
  
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
        
        # ----
        # Import region files. If more than 1 region file is provided,
        #   a loop is used to bind all regions into 1 dataframe. ----
        #Load header of file(s) to retrieve column names --------------------------------
        data_path_split <- unlist(strsplit(data_path(), split = ","))
        
        #If some path(s)
        if(length(data_path_split) > 0){
          
          #load first file
          data_regions = read.table(data_path_split[1], 
                                 header = TRUE, sep = "\t")
          
          #If more than one file, combine with other files
          if(length(data_path_split) > 1){
            for (x in data_path_split[-1]) {
              data_regions_tmp = read.table(x, header = TRUE, sep = "\t")
              data_regions <- 
                rbind(data_regions, 
                      data_regions_tmp[, match(colnames(data_regions), 
                                            colnames(data_regions_tmp))])
              
              
            }
          }
          
          #Retrieve available groups and status
          list_groups <- unique(gsub(
            pattern = "^ *| *$",
            replacement = "",
            unlist(strsplit(data_regions$group, split = ",|;"))
          ))
          list_status <- unique(gsub(
            pattern = "^ *| *$",
            replacement = "",
            unlist(strsplit(data_regions$status, split = ",|;"))
          ))
          
          #Update with available groups and status
          updatePickerInput(session = session, 
                            inputId = "regions_group_to_check", 
                            choices = list_groups)
          
          updatePickerInput(session = session, 
                            inputId = "regions_status", 
                            choices = list_status)
          
          updatePickerInput(session = session, 
                            inputId = "group_as_ctrls", 
                            choices = list_groups)
          
          
        } else {
          # If no region file, update pickers to show that no 
          #   groups/status/controls can be selected.
          updatePickerInput(session = session, 
                            inputId = "regions_group_to_check", 
                            choices = "")
          
          updatePickerInput(session = session, 
                            inputId = "regions_status", 
                            choices = "")
          
          updatePickerInput(session = session, 
                            inputId = "group_as_ctrls", 
                            choices = "")
        }
        
      }
      
    })
    
    #Update Ctrl region Picker input with selected regions ----
    observeEvent(input$regions_group_to_check, {  
      updatePickerInput(session = session, 
                        inputId = "group_as_ctrls", 
                        choices = "")
      updatePickerInput(session = session, 
                        inputId = "group_as_ctrls", 
                        choices = input$regions_group_to_check)
    })
    
    
    return(list(
      "data_path" = data_path,
      "regions_group_to_check" = reactive(input$regions_group_to_check),
      "regions_status" = reactive(input$regions_status),
      "group_as_ctrls" = reactive(input$group_as_ctrls)
    ))
  })
}










