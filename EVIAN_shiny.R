# --------------------------------------------
#
# EVIAN_shiny.R
# Generate an interface to return command lines and launch additional tools.
# Version 1.0
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

#Get the current dir of the user----
userCurrentDir <- getwd()

#Set the script dir as the current dir----
args <- commandArgs()
dirScript <- dirname(gsub(args[ grep(args, pattern = "--file") ], 
                          pattern = "--file=", replacement = ""))
if(length(dirScript)==1){ 
  setwd(dirScript)
  dirScript <- getwd()
  }

#Loading libraries----
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(rmarkdown)
library(waiter)


# Define UI for app ----
ui <- fluidPage(
  # Waiter and SweetAlert ui functions ----
  useWaiter(),
  useSweetAlert(),
  
  # App title ----
  titlePanel("DNAm analysis tool"),
  
  navlistPanel(
    "Tool selection",
    
    
    # First tool ----
    tabPanel("Processing IDAT files...",
             h3("This is the first panel")
    ),
    
    # ----
    
    # Second tool ----
    tabPanel("DNAm visualization report...",
             
             # Global styles ----
             tags$head(tags$style(".pathButt{background-color:#458C30;} 
                                  .pathButt{color: #DFDFDF;}")),
             tags$head(tags$style(".fileLoaded{color: #458C30; 
                                  font-size: 16px; 
                                  font-style: italic; 
                                  background-color:#DFDFDF; 
                                  overflow: hidden; 
                                  position: relative; 
                                  text-align: center; 
                                  word-break:break-all; 
                                  word-wrap:break-word; 
                                  white-space:pre-wrap;}")),
             
             
             
             # Intro ----
             
             h1("DNAm visualization report"),
             p("This tool generate a report (html format) describing the 
                patterns and enrichment of DNA methylation in regions selected by the user."),
             p("The file paths are mandatory arguments and must be filled by the user:
                the path of the samples, the control, the CpG positions and the regions files."),
             p("Click on the 'Generate Report' button at the bottom of the page 
               once you filled the previous options."),
             
             # Horizontal line ---
             tags$hr(),
             
             # Input: Samples ----
             h3("Samples"),
             fluidRow(
               column(6,
                      
                      h5("Choose CSV File For Patients Data To Analyze (Beta-value)"),
                      shinyFilesButton("samples_path", 
                                       "Select file...",
                                       title = "Choose CSV File For Patients Data To Analyze (Beta-value):", 
                                       multiple = TRUE, 
                                       buttonType = "default", class = "pathButt", 
                                       icon = icon("file")),
                      
                      
                      tagAppendAttributes(textOutput("value_samples_path"), 
                                          class = 'fileLoaded'),
                      br(),
                      
                      pickerInput(
                        inputId = "samples_id_to_check",
                        label = h5("Which samples should be visualized? (example: IdSample1,IdSample2,IdSample3)"), 
                        choices = rep("",1),
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      )
                      
                      
               ),
               column(6,
                      p("Select the file(s) containing patient data to be analyzed."),
                      p("Columns: one sample per column / Rows: one CpG per row"),
                      br(),
                      p("Only 10 samples can be analyzed at the same time."),
                      p("Use the text box to indicate which samples id to keep for the analysis. 
                        If the box remains empty, all samples from the file will be analyzed.")
               )
             ),
             
             
             # Horizontal line ---
             tags$hr(),
             
             # Input: Controls ----
             h3("Control population"),
             
             fluidRow(
               column(6,
                      
                      h5("Choose CSV File For Control Population Data (SumStat)"),
                      shinyFilesButton("control_path", 
                                       "Select file...",
                                       title = "Choose CSV File For Control Population Data (SumStat):", 
                                       multiple = FALSE, 
                                       buttonType = "default", class = "pathButt", 
                                       icon = icon("file")),
                      tagAppendAttributes(textOutput("value_control_path"), 
                                          class = 'fileLoaded'),
               ),
               column(6,
                      p("Select the control population file to be compared with the samples."),
                      p("This file (usually named 'SumStat') contains the summary of 
                        DNA methylation distribution per CpG in the control population."),
                      p("Columns: stats (mean, sd, etc...) / Rows: one CpG per row")
               )
             ),
             
             # Horizontal line ---
             tags$hr(),
             
             # Input: Regions ----
             h3("Regions to analyze"),
             
             fluidRow(
               column(6,
                      
                      h5("Choose TAB File For Regions Data to be analyzed"),
                      shinyFilesButton("regions_path", 
                                       "Select file...",
                                       title = "Choose TAB File For Regions Data to be analyzed:", 
                                       multiple = TRUE, 
                                       buttonType = "default", class = "pathButt", 
                                       icon = icon("file")),
                      tagAppendAttributes(textOutput("value_regions_path"), 
                                          class = 'fileLoaded'),
                      
                      pickerInput(
                        inputId = "regions_group_to_check",
                        label = h5("Which group of regions should be visualized? (example: Syndrome1,Syndrome2,ControlRegions1,ControlRegions2)"),
                        choices = rep("",1),
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      ),
                      
                      pickerInput(
                        inputId = "regions_status",
                        label = h5("What status labels for regions can be used for computation? (example: HighConfidence,MediumConfidence,RandomLabel1)"),
                        choices = rep("",1),
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      ),
                      
                      pickerInput(
                        inputId = "group_as_ctrls",
                        label = h5("What group of regions can be used as control? (example: ControlRegions1,ControlRegions2)"),
                        choices = rep("",1),
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      )
                      
               ),
               column(6,
                      p("Provide the regions to be used for the analysis. 
                        Regions positions must use the same genome version as the 
                        CpG positions genome version."),
                      p("Columns: chr,start,end,strand,name,group,status / Rows: one region per row"),
                      br(),
                      p("Fill the 1st text box to select which group of regions to analyze. 
                        Leave it empty to analyze all regions (will take more time for the report 
                        to be generated)."),
                      br(),
                      p("Fill the 2nd text box to select which status of regions to analyze 
                        (e.g. regions with high confidence, potential DMRs, etc).
                        Removed regions could still appear in profiles if they overlap 
                        some regions that are still kept.
                        Leave it empty to not filter out regions based on status."),
                      br(),
                      p("Fill the 3rd text box to select which group of regions 
                        to be considered as control regions. 
                        Leave it empty to not display any region as control region.")
               )
             ),
             
             
             # Horizontal line ---
             tags$hr(),
             
             # Input: Export Files ----
             h3("Graphs/Tables export"),
             
             fluidRow(
               column(6,
                      
                      h5("Export Graphs and Tables?"),
                      switchInput(inputId = "export_tables_graphs",  
                                  value = FALSE, 
                                  onStatus = "success",
                                  offStatus = "danger")
                      # checkboxInput("export_tables_graphs", 
                      #               label = "Export Graphs and Tables?", 
                      #               value = FALSE),
                      
               ),
               column(6,
                      p("Check this box if you want the tables and plots to be exported 
                        as pdf files in a small directory (next to the html report).")
               )
             ),
             
             
             # Horizontal line ---
             tags$hr(),
             
             # RDS input paths ----
             div(id = "rds_input_paths", class = "collapse out", 
                 
                 # Input: CpG positions ----
                 h3("CpG positions"),
                 
                 
                 fluidRow(
                   column(6,
                          
                          h5("Choose RDS File For CpG Position Data"),
                          shinyFilesButton("cpg_positions", 
                                           "Select file...",
                                           title = "Choose RDS File For CpG Position Data:", 
                                           multiple = FALSE, 
                                           buttonType = "default", class = "pathButt", 
                                           icon = icon("file")),
                          tagAppendAttributes(textOutput("value_cpg_positions"), 
                                              class = 'fileLoaded'),
                   ),
                   column(6,
                          p("Provide the positions of CpGs with the correct genome version."),
                          p("Columns: ,Chr,Start,Strand / Rows: one region per row"),
                          p(".rds data.")
                   )
                 ),
                 
                 
                 # Input: Annot CGI ----
                 h3("Annot CGI"),
                 
                 
                 fluidRow(
                   column(6,
                          
                          h5("Choose RDS File For Annot CGI Data"),
                          shinyFilesButton("annot_cgi", 
                                           "Select file...",
                                           title = "Choose RDS File For Annot CGI Data:", 
                                           multiple = FALSE, 
                                           buttonType = "default", class = "pathButt", 
                                           icon = icon("file")),
                          tagAppendAttributes(textOutput("value_annot_cgi"), 
                                              class = 'fileLoaded'),
                   ),
                   column(6,
                          p("Provide the positions of CpG Islands with the correct genome version."),
                          p(".rds data.")
                   )
                 ),
                 
                 
                 
                 # Input: Annot Gene ----
                 h3("Annot Gene"),
                 
                 
                 fluidRow(
                   column(6,
                          
                          h5("Choose RDS File For Annot CGI Data"),
                          shinyFilesButton("annot_gene", 
                                           "Select file...",
                                           title = "Choose RDS File For Annot Gene Data:", 
                                           multiple = FALSE, 
                                           buttonType = "default", class = "pathButt", 
                                           icon = icon("file")),
                          tagAppendAttributes(textOutput("value_annot_gene"), 
                                              class = 'fileLoaded'),
                   ),
                   column(6,
                          p("Provide the positions of Genes with the correct genome version."),
                          p(".rds data.")
                   )
                 ),
                 
                 
                 
                 
                 # Input: Annot Repeats ----
                 h3("Annot Repeats"),
                 
                 
                 fluidRow(
                   column(6,
                          
                          h5("Choose RDS File For Annot Repeats Data"),
                          shinyFilesButton("annot_repeats", 
                                           "Select file...",
                                           title = "Choose RDS File For Annot Repeats Data:", 
                                           multiple = FALSE, 
                                           buttonType = "default", class = "pathButt", 
                                           icon = icon("file")),
                          tagAppendAttributes(textOutput("value_annot_repeats"), 
                                              class = 'fileLoaded'),
                   ),
                   column(6,
                          p("Provide the positions of Repeats with the correct genome version."),
                          p(".rds data.")
                   )
                 ),
                 
                 
                 
                 
             ),
             
             HTML("<button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#rds_input_paths'><span class='glyphicon glyphicon-collapse-down'></span> Click here to modify the annotation RDS default inputs.</button>"),
             
             br(),
             
             # Horizontal line ---
             tags$hr(),
             
             
             # Generate report ----
             fluidRow(
               column(3, ),
               column(3, 
                      shinyDirButton("outputDir_path", "Change output directory", "Upload", 
                                     class = "pathButt", 
                                     icon = icon("folder")),
                      h4("Output directory:"),
                      verbatimTextOutput("outputDir_path", placeholder = TRUE),
                      br(),
                      actionButton("generateReport", label = "Generate report", 
                                   buttonType = "default", class = "generateReportButt", 
                                   icon = icon("file-download")),
                      tags$head(tags$style(".generateReportButt{background-color:#00529B;} 
                                  .generateReportButt{color: #DFDFDF;} 
                                  .generateReportButt{padding: 40px;} 
                                  .generateReportButt{font-size: 20px;}"))
                      ),
               column(3, )
             ),
             
    
             
             # Horizontal line ---
             tags$hr(),
             
             
             # ----
    ),
    
    # ----
    
    # Third tool ----
    tabPanel("DMR identification...",
             h3("This is the third panel")
    )
    # ----
  )     
)
# ----

# Define server to associate to the ui ----
server <- function(input,output,session) {
  
  #Input retrieve and check file paths ----
  volumes = getVolumes()
  value_samples_path <- reactiveVal("")
  value_control_path <- reactiveVal("")
  value_regions_path <- reactiveVal("")
  value_outputDir_path  <- reactiveVal(userCurrentDir)
  
  value_cpg_positions <- reactiveVal("")
  value_annot_cgi <- reactiveVal("")
  value_annot_gene <- reactiveVal("")
  value_annot_repeats <- reactiveVal("")
  
  #samples input
  observe({  
    rootsV <- c(user_current_directory = userCurrentDir, 
                DNAm_scripts_directory = dirScript, 
                home = '~', volumes())
    shinyFileChoose(input, "samples_path", 
                    roots = rootsV, 
                    session = session)
    
    if(!is.null(input$samples_path)){
      value_samples_path( 
            paste0(
            parseFilePaths(rootsV, input$samples_path)$datapath,
            collapse=","
          )  
        
        )
      output$value_samples_path <- renderText(value_samples_path())
      
      
      #Get samples for input samples name --------------------------------
      samplesPath <- unlist(strsplit(value_samples_path(), split = ","))
      if(length(samplesPath) > 0){
        if(length(samplesPath) > 1){
          samples = read.csv(samplesPath[1], row.names = 1, header=TRUE, nrows = 1)
          for (x in samplesPath[-1]) {
            samplesTmp = read.csv(x, row.names = 1, header=TRUE, nrows = 1)
            samples <- cbind(samples, samplesTmp[match(rownames(samples), rownames(samplesTmp)),])
          }
        } else {
          samples = read.csv(samplesPath, row.names = 1, header=TRUE, nrows = 1)
        }
        
        updatePickerInput(session = session, 
                          inputId = "samples_id_to_check", 
                          choices = colnames(samples))
      }
      
    }
  })
  
  #control input
  observe({  
    rootsV <- c(DNAm_scripts_directory = dirScript, 
                user_current_directory = userCurrentDir, 
                home = '~', volumes())
    shinyFileChoose(input, "control_path", 
                    roots = rootsV, 
                    session = session)
    
    if(!is.null(input$control_path)){
      value_control_path( 
        paste0(
          parseFilePaths(rootsV, input$control_path)$datapath,
          collapse=","
        )
      )
      output$value_control_path <- renderText(value_control_path())
      
    }
  })
  
  #regions input
  observe({  
    rootsV <- c(user_current_directory = userCurrentDir, 
                DNAm_scripts_directory = dirScript, 
                home = '~', volumes())
    shinyFileChoose(input, "regions_path", 
                    roots = rootsV, 
                    session = session)
    
    if(!is.null(input$regions_path)){
      value_regions_path( 
        paste0(
          parseFilePaths(rootsV, input$regions_path)$datapath,
          collapse=","
          )
      )
      output$value_regions_path <- renderText(value_regions_path())
      
      # ----
      # Import region files. If more than 1 region file is provided,
      #   a loop is used to bind all regions into 1 dataframe. ----
      region_filepaths <- unlist(strsplit(value_regions_path(), split = ","))
      if(length(region_filepaths) > 0){
        if (length(region_filepaths) > 1) {
          regions_all <- read.table(region_filepaths[1], header = TRUE, sep = "\t")
          for (x in region_filepaths[-1]) {
            regions_all_tmp <- read.table(x, header = TRUE, sep = "\t")
            regions_all <- rbind(regions_all, regions_all_tmp)
          }
          rm(regions_all_tmp)
        } else {
          regions_all <- read.table(region_filepaths, header = TRUE, sep = "\t")
        }
        
        list_groups <- unique(split_group <- gsub(
          pattern = "^ *| *$",
          replacement = "",
          unlist(strsplit(regions_all$group, split = ",|;"))
        ))
        list_status <- unique(split_status <- gsub(
          pattern = "^ *| *$",
          replacement = "",
          unlist(strsplit(regions_all$status, split = ",|;"))
        ))
        
        updatePickerInput(session = session, 
                          inputId = "regions_group_to_check", 
                          choices = list_groups)
        
        updatePickerInput(session = session, 
                          inputId = "regions_status", 
                          choices = list_status)
        
        updatePickerInput(session = session, 
                          inputId = "group_as_ctrls", 
                          choices = list_groups)
        
      }
      
      
      
      
    }
  })
  
  
  #cpg_positions input
  observe({  
    rootsV <- c(DNAm_scripts_directory = file.path(dirScript, "DNAm_Pct_report", "annot_rds_files"), 
                user_current_directory = userCurrentDir, 
                home = '~', volumes())
    shinyFileChoose(input, "cpg_positions", 
                    roots = rootsV, 
                    session = session,
                    filetypes = c('rds'))
    
    if(!is.null(input$cpg_positions)){
      value_cpg_positions( 
        paste0(
          parseFilePaths(rootsV, input$cpg_positions)$datapath,
          collapse=","
        )  
      )
      output$value_cpg_positions <- renderText(value_cpg_positions())
      
    }
  })
  
  #annot_cgi input
  observe({  
    rootsV <- c(DNAm_scripts_directory = file.path(dirScript, "DNAm_Pct_report", "annot_rds_files"), 
                user_current_directory = userCurrentDir, 
                home = '~', volumes())
    shinyFileChoose(input, "annot_cgi", 
                    roots = rootsV, 
                    session = session,
                    filetypes = c('rds'))
    
    if(!is.null(input$annot_cgi)){
      value_annot_cgi( 
        paste0(
          parseFilePaths(rootsV, input$annot_cgi)$datapath,
          collapse=","
        )  
      )
      output$value_annot_cgi <- renderText(value_annot_cgi())
      
    }
  })
  
  #annot_gene input
  observe({  
    rootsV <- c(DNAm_scripts_directory = file.path(dirScript, "DNAm_Pct_report", "annot_rds_files"), 
                user_current_directory = userCurrentDir, 
                home = '~', volumes())
    shinyFileChoose(input, "annot_gene", 
                    roots = rootsV, 
                    session = session,
                    filetypes = c('rds'))
    
    if(!is.null(input$annot_gene)){
      value_annot_gene( 
        paste0(
          parseFilePaths(rootsV, input$annot_gene)$datapath,
          collapse=","
        )  
      )
      output$value_annot_gene <- renderText(value_annot_gene())
      
    }
  })
  
  #annot_repeats input
  observe({  
    rootsV <- c(DNAm_scripts_directory = file.path(dirScript, "DNAm_Pct_report", "annot_rds_files"), 
                user_current_directory = userCurrentDir, 
                home = '~', volumes())
    shinyFileChoose(input, "annot_repeats", 
                    roots = rootsV, 
                    session = session,
                    filetypes = c('rds'))
    
    if(!is.null(input$annot_repeats)){
      value_annot_repeats( 
        paste0(
          parseFilePaths(rootsV, input$annot_repeats)$datapath,
          collapse=","
        )  
      )
      output$value_annot_repeats <- renderText(value_annot_repeats())
      
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
  
  #Output directory and check file paths ----
  observeEvent(input$outputDir_path, {  
    rootsV <- c(user_current_directory = userCurrentDir, 
                DNAm_scripts_directory = dirScript, 
                home = '~')
    # @@@ volumes() does not work with this function
    
    shinyDirChoose(
      input,
      'outputDir_path',
      roots = rootsV, 
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    value_outputDir_path( 
      parseDirPath(rootsV, input$outputDir_path)
    )
    
    if(!is.null(value_outputDir_path())){
      if(length(value_outputDir_path())==0){
        value_outputDir_path(userCurrentDir)
      }
    } else{
      value_outputDir_path(userCurrentDir)
    }
    
  })
  
  output$outputDir_path <- renderText({
    value_outputDir_path()
  })
  
  
  #Generate command ----
  confirmReportGeneration <- reactiveVal(FALSE)
  commandToExport <- reactiveVal("")
  
  observeEvent(input$generateReport, {
    
    #Check if each param must be printed or not ---
    cond1 <- ifelse(value_samples_path() %in% c("", "NULL"), "",
                    paste0('--samples_path=', '"',value_samples_path(),'"'))
    
    samples_id_to_check <- paste0(input$samples_id_to_check, collapse = ",")
    cond2 <- ifelse(samples_id_to_check %in% c("", "NULL"), "",
                    paste0('--samples_id_to_check=', '"',
                           samples_id_to_check,
                           '"'))
    
    cond3 <- ifelse(value_control_path() %in% c("", "NULL"), "",
                    paste0('--control_path=', '"',value_control_path(),'"'))
    
    cond4 <- ifelse(value_regions_path() %in% c("", "NULL"), "",
                    paste0('--regions_path=', '"',value_regions_path(),'"'))
    
    regions_group_to_check <- paste0(input$regions_group_to_check, collapse = ",")
    cond5 <- ifelse(regions_group_to_check %in% c("", "NULL"), "",
                    paste0('--regions_group_to_check=', '"',regions_group_to_check,'"'))
    
    regions_status <- paste0(input$regions_status, collapse = ",")
    cond6 <- ifelse(regions_status %in% c("", "NULL"), "",
                    paste0('--regions_status=', '"',regions_status,'"'))
    
    group_as_ctrls <- paste0(input$group_as_ctrls, collapse = ",")
    cond7 <- ifelse(group_as_ctrls %in% c("", "NULL"), "",
                    paste0('--group_as_ctrls=', '"',group_as_ctrls,'"'))
    
    cond8 <- ifelse(input$export_tables_graphs %in% c("", "NULL"), "",
                    paste0('--export_tables_graphs=', '"',input$export_tables_graphs,'"'))
    cond9 <- paste0('--outputDir_path=', value_outputDir_path())
    
    
    
    cond10 <- ifelse(value_cpg_positions() %in% c("", "NULL"), "",
                     paste0('--cpg_positions=', '"',value_cpg_positions(),'"'))
    cond11 <- ifelse(value_annot_cgi() %in% c("", "NULL"), "",
                     paste0('--annot_cgi=', '"',value_annot_cgi(),'"'))
    cond12 <- ifelse(value_annot_gene() %in% c("", "NULL"), "",
                     paste0('--annot_gene=', '"',value_annot_gene(),'"'))
    cond13 <- ifelse(value_annot_repeats() %in% c("", "NULL"), "",
                     paste0('--annot_repeats=', '"',value_annot_repeats(),'"'))
    
    
    #Generate command ---
    commandToExport(
      paste(
        "Rscript",
        file.path(dirScript,'DNAm_Pct_report','DNAm_Pct.R'),
        cond1, cond2, cond3,
        cond4, cond5, cond6,
        cond7, cond8, cond9,
        cond10,
        cond11,
        cond12,
        cond13,
        sep = " "
      )
    )
    
    
    #Paths missing? Ask user ---    
    confirmReportGeneration <- TRUE
    if(cond1 == "" | cond3 == "" | cond4 == "" | cond5 == ""){
      sendSweetAlert(
        session = session,
        title = "Warning!",
        text = "Some paths are missing! This could return errors during report generation.",
        type = "warning"
      )
      ask_confirmation(
        inputId = "confirmPathsMissing",
        title = "Want to generate report even if some paths are missing?"
      )
    } else {
      confirmReportGeneration(TRUE)
    }
    
  })
  
  observeEvent(input$confirmPathsMissing, {
    confirmReportGeneration(input$confirmPathsMissing)
  }, ignoreNULL = TRUE)
  
  observeEvent(confirmReportGeneration(), {
    if(confirmReportGeneration()){
      w = Waiter$new(
        html = tagList(
          spin_circle(),
          h1("Generating report..."),
          # h2("Please do not shutdown this page before the end of the task."),
          h2("This window will now automatically close."),
          h4("Progression can be followed directly on the console.")
        )
      )
      
      w$show()
      
      Sys.sleep(5)
      
      stopApp(  commandToExport()  )
      
      
      
      w$hide() 
    }
  }, ignoreNULL = TRUE)
  
  
  # ----
}
# ----

# Compile, launch app and retrieve command to be launched ----
#compile app
app <- shinyApp(ui = ui, server = server, 
                options = list(launch.browser = TRUE))
#run app / retrieve command
commandToLaunch <- runApp(app)
# window.close()
#print command
print(commandToLaunch)
#launch command
system(commandToLaunch)
