# --------------------------------------------
#
# ui_idat_process.R
# Compiles the different parts of the ui Processing IDAT files tab.
# Version 1.0
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Generate tab
#
# --------------------------------------------

#Dependencies----
# shiny
# shinyFiles
# shinyWidgets
# rmarkdown
# waiter

# Generate row - Input: idat files ----
rows_idat_files <-
  fluidRow(
    column(6,
           
           h5("Choose Directory containing the idat files to process"),
           shinyDirButton("idat_files_dir_path", 
                          "Choose Directory containing the idat files to process", 
                          "Upload", 
                          class = "pathButt", 
                          icon = icon("folder")),
           tagAppendAttributes(textOutput("idat_files_dir_path"), 
                               class = 'fileLoaded'),
           
           
           h5("Truncate sample names?"),
           switchInput(inputId = "truncate_samples_names",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger"),
           
           
    ),
    column(6,
           p("Select the directory containing the idat files to process."),
           br(),
           p("Check this box to truncate the names of the samples: 
           every character after the first \"_\" will be removed.")
    )
  )


# Generate row - Input: Controls ----
row_control_population <-
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
  )








# Generate row - Normalization method ----
row_norm_method <-
  fluidRow(
    column(6,
           
           h5("Choose Normalization method to use"),
           awesomeRadio(
             inputId = "norm_method",
             label = "Select method...", 
             choices = c("quantile", "funnorm"),
             selected = "quantile",
             status = "warning"
           )
           
    ),
    column(6,
           p("Select the normalisation method be performed if multiple samples are provided."),
           p("quantile corresponds to stratified quantile.")
    )
  )






# Generate rows - filter options ----
row_filter_options <-
  fluidRow(
    column(6,
           
           h5("Remove probes with high detection p-value?"),
           switchInput(inputId = "remove_probes_highpval",  
                       value = FALSE, 
                       onStatus = "success",
                       offStatus = "danger"),
           br(),
           h5("Remove samples with too much probes with high detection p-value?"),
           switchInput(inputId = "remove_samples_highpval",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger"),
           br(),
           h5("Remove samples with an insufficient Meth/Unmeth signal?"),
           switchInput(inputId = "removeSamplesWithBadMethUnmeth",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger"),
           br(),
           h5("Insufficient Meth/Unmeth signal limit value to remove samples"),
           sliderInput(inputId = "badMethUnmethSampleCutoff",
                       label="",
                              min = 0, max = 20, 
                              value = 10.5, 
                              step = 0.5),
           
           br(),
           h5("Remove probes associated to snps?"),
           switchInput(inputId = "remove_snip",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger")
           
    ),
    column(6,
           p("Info 1."),
           p("Info 2."),
           p("Info 3."),
           p("Info 4."),
           p("Info 5.")
    )
  )







# Generate rows - Output options ----
row_output_options <-
  fluidRow(
    column(6,
           
           h5("Export M-values?"),
           switchInput(inputId = "export_mval",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger"),
           br(),
           h5("Generate QC reports?"),
           switchInput(inputId = "generate_qc_reports",  
                       value = TRUE, 
                       onStatus = "success",
                       offStatus = "danger")
           
    ),
    column(6,
           p("Check this box if you want the M-values to be also exported in a csv file."),
           br(),
           p("Check this box if you want the QC reports to be generated.")
    )
  )






# Generate row - Launch processing ----
row_launch_processing <- 
  fluidRow(
    column(3, ),
    column(3, 
           shinyDirButton("outputDir_path", "Change output directory", "Upload", 
                          class = "pathButt", 
                          icon = icon("folder")),
           h4("Output directory:"),
           verbatimTextOutput("outputDir_path", placeholder = TRUE),
           br(),
           textInput(inputId = "output_basename", 
                     label = "Output basename", value = "output"),
           br(),
           actionButton("launchProcessing", label = "Launch processing", 
                        buttonType = "default", class = "launchProcessingButt", 
                        icon = icon("file-download")),
           tags$head(tags$style(".launchProcessingButt{background-color:#00529B;} 
                                  .launchProcessingButt{color: #DFDFDF;} 
                                  .launchProcessingButt{padding: 40px;} 
                                  .launchProcessingButt{font-size: 20px;}"))
    ),
    column(3, )
  )




# Generate tab ----
tab_idat_process <- 
  tabPanel("Processing IDAT files...",
           
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
           h1("IDAT processing and Quality Check"),
           p("This tool generate QC reports (html format) describing the 
                quality of idat input files provided and process these files using the selected filters."),
           p("Click on the 'Launch processing' button at the bottom of the page 
               once you filled the previous options."),
           
           tags$hr(), # Horizontal line
           
           # Input: idat files ----
           h3("IDAT files"),
           rows_idat_files,
           tags$hr(), # Horizontal line
           
           
           # Input: Controls ----
           h3("Control population"),
           row_control_population,
           tags$hr(), # Horizontal line
           
           
           
           # Normalization method ----
           h3("Normalization method"),
           row_norm_method,
           tags$hr(), # Horizontal line
           
           
           # Filter options ----
           h3("Filter options"),
           row_filter_options,
           tags$hr(), # Horizontal line
           
           
           # Output options ----
           h3("Output options"),
           row_output_options,
           tags$hr(), # Horizontal line
           
           # Launch processing ----
           row_launch_processing,
           tags$hr(), # Horizontal line
           
  )




