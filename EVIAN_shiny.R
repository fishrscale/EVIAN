# --------------------------------------------
#
# EVIAN_shiny.R
# Generate an interface to return command lines and launch additional tools.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Get/set current and scripts directories
# # Loading libraries
# # Loading modules
# # Define UI interface for app
# # Define server to associate to the UI
# # Compile, launch app and retrieve command to be launched
#
# --------------------------------------------

#######################
# Current/Script directories settings
#######################
#Get the current dir of the user
userCurrentDir <- getwd()

#Set the script dir as the current dir
args <- commandArgs()
dirScript <- dirname(gsub(args[ grep(args, pattern = "--file") ], 
                          pattern = "--file=", replacement = ""))
if(length(dirScript)==0){ 
  if(rstudioapi::isAvailable()){
    dirScript = normalizePath(
      file.path(dirname(rstudioapi::getSourceEditorContext()$path))
    )
  } else {
    warning("Could not determine script directory. 
            Setting current directory as script directory.
            This could create issues.")
    dirScript="." 
  }
}
setwd(dirScript)
dirScript <- getwd()
#######################


#######################
# Loading libraries
#######################
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(rmarkdown)
library(waiter)
#######################



#######################
# Modules
#######################
source("./EVIAN_shiny_modules/close_button_header.R")
source("./EVIAN_shiny_modules/get_path_filt_cols_input.R")
source("./EVIAN_shiny_modules/get_path_input.R")
source("./EVIAN_shiny_modules/get_path_filt_regions_input.R")
source("./EVIAN_shiny_modules/switch_true_false.R")
source("./EVIAN_shiny_modules/get_dir_path.R")
source("./EVIAN_shiny_modules/generate_command.R")
source("./EVIAN_shiny_modules/texts_full_list.R")
source("./EVIAN_shiny_modules/tab_ui_dnam_pct.R")
source("./EVIAN_shiny_modules/tab_server_dnam_pct.R")
source("./EVIAN_shiny_modules/tab_ui_idat_process.R")
source("./EVIAN_shiny_modules/tab_server_idat_process.R")
#######################



#######################
# Interface Layout
#######################

# Dashboard header
evian_header <- dashboardHeader(title = "EVIAN interface",
                                tags$li(class = "dropdown", 
                                        closeButtonHeaderUI("mod_close_app")))

# Dashboard sidebar
evian_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Processing IDAT files...", 
             tabName = "idat_process_tab", 
             icon = icon("filter")),
    menuItem(text = "DNAm Pct report...", 
             tabName = "dnam_pct_tab",
             icon = icon("chart-bar"))
  )
)

# Dashboard body
evian_body <- dashboardBody(
  # Waiter and SweetAlert ui functions ----
  useWaiter(),
  useSweetAlert(),
  
  # Define Global styles ----
  tags$head(tags$style(".pathButt{background-color:#458C30;} 
                       .pathButt{color: #DFDFDF;}")),
  tags$head(tags$style(".fileLoaded{color: #458C30; font-size: 16px; 
                       font-style: italic; background-color:#DFDFDF; 
                       overflow: hidden; position: relative; 
                       text-align: center; word-break:break-all; 
                       word-wrap:break-word; white-space:pre-wrap;}")),
  tags$head(tags$style(".generateCommandButt{background-color:#00529B;} 
                       .generateCommandButt{color: #DFDFDF;} 
                       .generateCommandButt{padding: 40px;} 
                       .generateCommandButt{font-size: 20px;}" )),
  
  # Main body ----
  tabItems(
    
    tabIdatProcessUI(id = "mod_idat_process_tab", 
                     tabName = "idat_process_tab",
                     texts_full_list),
    
    tabDnamPctUI(id = "mod_dnam_pct_tab", 
                 tabName = "dnam_pct_tab",
                 texts_full_list)
    
  )
  
)
#######################


##############################################
# Server to be linked to UI
##############################################
evian_server  <- function(input, output, session) {
  
  # Server to be linked to UI header
  closeButtonHeaderServer("mod_close_app")
  
  
  # Server for each tab to be linked to UI
  tabIdatProcessServer("mod_idat_process_tab")
  tabDnamPctServer("mod_dnam_pct_tab")
  
}
##############################################


#######################
# EVIAN interface app
#######################
evianInterfaceApp <- function(){
  ui <- dashboardPage(
    evian_header,
    evian_sidebar,
    evian_body
  )
  
  server <- evian_server
  
  shinyApp(ui, server, 
           options = list(launch.browser = TRUE))
}

#Run app / retrieve command
commandToLaunch <- runApp(evianInterfaceApp())

#If a command is returned, print and launch this command
if(!is.null(commandToLaunch)){
  #print command
  print(commandToLaunch)
  #launch command
  system(commandToLaunch)
}
#######################

