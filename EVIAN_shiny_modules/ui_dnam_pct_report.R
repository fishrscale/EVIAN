# --------------------------------------------
#
# ui_dnam_pct_report.R
# Compiles the different parts of the ui DNAm Pct report tab.
# Version 1.0
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Generate row(s)
#   # Samples
#   # Controls
#   # Regions
#   # RDS input paths
#   # Export files option
#   # Generate report (+ output location)
# # Generate tab (+ html styles)
#
# --------------------------------------------

#Dependencies----
# shiny
# shinyFiles
# shinyWidgets
# rmarkdown
# waiter

# Generate rows - Input: Samples ----
rowgroup_samples <-
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

# Generate rows - Input: Regions ----
rowgroup_regions <-
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
  )

# Generate rows - RDS input paths ----
rowgroup_rds_input_files <-
  fluidRow(
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
  )


# Generate row - Input: Export Files ----
row_export_files <-
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
  )

# Generate row - Generate report ----
row_generate_report <- 
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
  )

# Generate tab ----
tab_dnam_pct_report <- 
  tabPanel("DNAm Pct report...",
           
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
           h1("DNA methylation Percentage distribution report"),
           p("This tool generate a report (html format) describing the 
                patterns and enrichment of DNA methylation in regions selected by the user."),
           p("The sample/control/region file paths are mandatory arguments and must be filled by the user:
                the path of the samples, the control, the CpG positions and the regions files."),
           p("Click on the 'Generate Report' button at the bottom of the page 
               once you filled the previous options."),
           
           tags$hr(), # Horizontal line
           
           # Input: Samples ----
           h3("Samples"),
           rowgroup_samples,
           tags$hr(), # Horizontal line
           
           # Input: Controls ----
           h3("Control population"),
           row_control_population,
           tags$hr(), # Horizontal line
           
           # Input: Regions ----
           h3("Regions to analyze"),
           rowgroup_regions,
           tags$hr(), # Horizontal line
           
           # RDS input paths ----
           rowgroup_rds_input_files,
           br(), # Line break
           tags$hr(), # Horizontal line
           
           # Input: Export Files ----
           h3("Graphs/Tables export"),
           row_export_files,
           tags$hr(), # Horizontal line
           
           # Generate report ----
           row_generate_report,
           tags$hr(), # Horizontal line
  )



