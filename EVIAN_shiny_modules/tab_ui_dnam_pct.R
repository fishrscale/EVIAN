# --------------------------------------------
#
# tabDnamPctUI.R
# Contains the tabDnamPctUI function (requires the tabDnamPctServer function).
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Text intro
# # Samples input
# # Control population Input
# # Regions Input
# # RDS files Input
# # Export Graphs and Tables
# # Output directory
# # Generate report
# --------------------------------------------

tabDnamPctUI <- function(id, 
                         tabName,
                         texts_full_list){
  tabItem(tabName = tabName,
          
          ##############################################
          # Text intro
          ##############################################
          fluidRow(
            box(
              width = 12,
              h1("DNAm Pct report"), 
              p(texts_full_list[["DNAm Pct report"]])
            )
          ),
          ##############################################
          
          
          ##############################################
          # Samples input
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Samples"),
                   getPathFiltColsInputUI(
                     NS(id,"mod_dnampctrep_samples"), 
                     file_button_title = 
                       "Choose CSV File For Patients Data To Analyze (Beta-value)",
                     file_button_text = "Select file...",
                     title_choose_panel = 
                       "Choose CSV File For Patients Data To Analyze (Beta-value):",
                     title_picker_input = "Which samples should be visualized?"
                   ) 
                   
            ),
            column(6, p(texts_full_list[["DNAm Pct samples"]]) )
            
          ) ),
          ##############################################
          
          
          ##############################################
          # Control population Input
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Control population"),
                   getPathInputUI(
                     NS(id,"mod_dnampctrep_controlpop"), 
                     file_button_title = 
                       "Choose CSV File For Control Population Data (SumStat)",
                     file_button_text = "Select file...",
                     title_choose_panel = 
                       "Choose CSV File For Control Population Data (SumStat):"
                   ) 
                   
            ),
            column(6, p(texts_full_list[["controlpop"]]) )
            
          ) ),
          ##############################################
          
          
          ##############################################
          # Regions Input
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Regions to analyze"),
                   getPathFiltRegionsInputUI(
                     NS(id,"mod_dnampctrep_regions"), 
                     file_button_title = 
                       "Choose TAB File For Regions Data to be analyzed",
                     file_button_text = "Select file...",
                     title_choose_panel = "Choose TAB File For Regions Data to be analyzed:",
                     title_group_input = "Which group of regions should be visualized? (example: Syndrome1,Syndrome2,ControlRegions1,ControlRegions2)",
                     title_status_input = "What status labels for regions can be used for computation? (example: HighConfidence,MediumConfidence,RandomLabel1)",
                     title_control_input = "What group of regions can be used as control? (example: ControlRegions1,ControlRegions2)"
                   )
                   
            ),
            column(6, p(texts_full_list[["DNAm Pct regions"]]) )
            
          ) ),
          ##############################################
          
          
          ##############################################
          # RDS files Input
          ##############################################
          div(id = "rds_input_paths", class = "collapse out", 
              fluidRow( box( 
                width = 12,
                column(6,
                       # Input: CpG positions ----
                       h3("CpG positions"),
                       getPathInputUI(
                         NS(id,"mod_dnampctrep_rds_cpgpos"), 
                         file_button_title = 
                           "Choose RDS File For CpG Position Data",
                         file_button_text = "Select file...",
                         title_choose_panel = 
                           "Choose RDS File For CpG Position Data:"
                       )
                       
                ),
                column(6, p(texts_full_list[["DNAm Pct rds cpgpos"]]) )
                
              ),
              box( 
                width = 12,
                column(6,
                       # Input: Annot CGI ----
                       h3("Annot CGI"),
                       getPathInputUI(
                         NS(id,"mod_dnampctrep_rds_cgi"), 
                         file_button_title = 
                           "Choose RDS File For Annot CGI Data",
                         file_button_text = "Select file...",
                         title_choose_panel = 
                           "Choose RDS File For Annot CGI Data:"
                       )
                ),
                column(6, p(texts_full_list[["DNAm Pct rds cgi"]]) )
                
              ),
              box( 
                width = 12,
                column(6,
                       # Input: Annot Gene ----
                       h3("Annot Gene"),
                       getPathInputUI(
                         NS(id,"mod_dnampctrep_rds_gene"), 
                         file_button_title = 
                           "Choose RDS File For Annot Gene Data",
                         file_button_text = "Select file...",
                         title_choose_panel = 
                           "Choose RDS File For Annot Gene Data:"
                       )
                       
                ),
                column(6, p(texts_full_list[["DNAm Pct rds gene"]]) )
                
              ),
              box( 
                width = 12,
                column(6,
                       # Input: Annot Repeats ----
                       h3("Annot Repeats"),
                       getPathInputUI(
                         NS(id,"mod_dnampctrep_rds_repeats"), 
                         file_button_title = 
                           "Choose RDS File For Annot Repeats Data",
                         file_button_text = "Select file...",
                         title_choose_panel = 
                           "Choose RDS File For Annot Repeats Data:"
                       )
                       
                ),
                column(6, p(texts_full_list[["DNAm Pct rds repeats"]]) )
                
              ) )
              
          ),
          HTML("<button type='button' class='btn' data-toggle='collapse' 
       style='float:left' data-target='#rds_input_paths'> 
       <span class='glyphicon glyphicon-collapse-down'></span> 
       Click here to modify the annotation RDS default inputs.</button>"),
          ##############################################
          
          
          ##############################################
          # Export Graphs and Tables
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Graphs/Tables export"),
                   switchTrueFalseUI(id = NS(id,"mod_dnampct_exportgt"), 
                                     switch_title = "Export Graphs and Tables?", 
                                     default_value = FALSE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["DNAm Pct exportgt"]]) )
          ) ),
          ##############################################
          
          
          ##############################################
          # Output directory
          ##############################################
          fluidRow( box( 
            width = 12, align="center", 
            getDirPathUI(NS(id,"mod_dnampctrep_outdir"),
                         dir_button_title = "Output directory:",
                         dir_button_text = "Change output directory",
                         title_choose_panel = "Select output directory")
          ) ),
          ##############################################
          
          
          ##############################################
          # Generate report
          ##############################################
          fluidRow( box( 
            width = 12, align="center", 
            generateCommandUI(NS(id,"mod_dnampctrep_generate"),
                              button_text = "Generate report")
          ) )
          ##############################################
          
  )
  
}
