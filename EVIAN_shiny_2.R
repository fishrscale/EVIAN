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
# # Define UI for app
# # Define server to associate to the ui
# # Compile, launch app and retrieve command to be launched
#
# --------------------------------------------

# Get the current dir of the user----
userCurrentDir <- getwd()

# Set the script dir as the current dir----
args <- commandArgs()
dirScript <- dirname(gsub(args[ grep(args, pattern = "--file") ], 
                          pattern = "--file=", replacement = ""))
if(length(dirScript)==1){ 
  setwd(dirScript)
  dirScript <- getwd()
  }

# Loading libraries----
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(rmarkdown)
library(waiter)


# Define UI for app ----
source("./EVIAN_shiny_modules/ui_idat_process_and_qc.R")
source("./EVIAN_shiny_modules/ui_dnam_pct_report.R")
source("./EVIAN_shiny_modules/ui_dmr_identification.R")

ui <- shinyUI({
  fluidPage(
    # Waiter and SweetAlert ui functions ----
    useWaiter(),
    useSweetAlert(),
    
    # App title ----
    titlePanel("EVIAN toolbox interface"),
    
    
    # List and order of tabs / tools ----
    navlistPanel(
      "Tool selection",
      tab_idat_process,
      tab_dnam_pct_report,
      tab_dmr_identification  
    )  
    
  )
})
# ----

# Define server to associate to the ui ----    
source("./EVIAN_shiny_modules/server.R")
server <- server
# ----

# Compile, launch app and retrieve command to be launched ----
#compile app
app <- shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
#run app / retrieve command
commandToLaunch <- runApp(app)
# window.close()
#print command
print(commandToLaunch)
#launch command
system(commandToLaunch)
