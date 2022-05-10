# --------------------------------------------
#
# tabIdatProcessUI.R
# Contains the tabIdatProcessUI function (requires the tabIdatProcessServer function).
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Text intro
# # IDAT directory
# # Control population Input
# # truncate_samples_names
# # qc_report
# # remove_probes_highpval
# # remove_samples_highpval
# # removeSamplesWithBadMethUnmeth
# # badMethUnmethSampleCutoff
# # norm_method
# # remove_snip
# # get_mval
# # Output directory
# # Output name
# # Generate report
# 
# --------------------------------------------

tabIdatProcessUI <- function(id, 
                         tabName,
                         texts_full_list){
  tabItem(tabName = tabName,
          
          ##############################################
          # Text intro
          ##############################################
          fluidRow(
            box(
              width = 12,
              h1("IDAT processing and QC"), 
              p(texts_full_list[["IDAT process"]])
            )
          ),
          ##############################################
          
          
          ##############################################
          # IDAT directory
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("IDAT files directory"),
                   getDirPathUI(NS(id,"mod_idat_process_idat_dir"),
                                dir_button_title = "Idat directory:",
                                dir_button_text = "Change input directory",
                                title_choose_panel = "Select input directory")
            ),
            column(6, p(texts_full_list[["IDAT process idat_dir"]]) )
            
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
                     NS(id,"mod_idat_process_controlpop"), 
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
          # truncate_samples_names
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Truncate sample names"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_trunc"), 
                                     switch_title = "Should the sample names be truncated?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process trunc"]]) )
          ) ),
          ##############################################
          
          
          
          ##############################################
          # qc_report
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("QC report"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_qc_report"), 
                                     switch_title = "Generate QC report(s)?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process qc_report"]]) )
          ) ),
          ##############################################
          
          
          
          ##############################################
          # remove_probes_highpval
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Remove Probes Highpval"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_rph"), 
                                     switch_title = "Remove probes with high detection p-value?", 
                                     default_value = FALSE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process rph"]]) )
          ) ),
          ##############################################
          
          
          ##############################################
          # remove_samples_highpval
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Remove Samples Highpval"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_rsh"), 
                                     switch_title = "Remove samples with too much probes with high detection p-value?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process rsh"]]) )
          ) ),
          ##############################################
          
          
          ##############################################
          # removeSamplesWithBadMethUnmeth
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Samples With Bad Meth Unmeth Signal"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_bad_mu"), 
                                     switch_title = "Remove samples with an insufficient Meth/Unmeth signal?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process bad_mu"]]) )
          ) ),
          ##############################################
          
          
          
          ##############################################
          # badMethUnmethSampleCutoff
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   
                   h3("Cutoff Meth/Unmeth signal"),
                   sliderInput(inputId = NS(id,"mod_idat_process_bad_mu_limit"), 
                               label = h3("Select which cutoff to apply:"), 
                               min = 7, max = 15, step = 0.5,
                               value = 10.5)
                   
            ),
            column(6, p(texts_full_list[["IDAT process bad_mu_limit"]]) )
          ) ),
          ##############################################
          
          
          ##############################################
          # norm_method
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("Normalization methods"),
                   awesomeRadio(
                     inputId = NS(id,"mod_idat_process_norm"),
                     label = "Select which method to use:", 
                     choices = c("quantile", "funnorm"),
                     selected = "quantile",
                     status = "info"
                   )
                   
            ),
            column(6, p(texts_full_list[["IDAT process norm"]]) )
          ) ),
          ##############################################
          
          
          
          
          ##############################################
          # remove_snip
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("SNPs"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_remove_snip"), 
                                     switch_title = "Remove probes associated to snps?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process remove_snip"]]) )
          ) ),
          ##############################################
          
          
          
          ##############################################
          # get_mval
          ##############################################
          fluidRow( box( 
            width = 12,
            column(6,
                   h3("M-values"),
                   switchTrueFalseUI(id = NS(id,"mod_idat_process_get_mval"), 
                                     switch_title = "Retrieve also M-value files?", 
                                     default_value = TRUE, 
                                     customOnBackColor="#9FDAA1",
                                     customOnColor="#FFFFFF", 
                                     customOffBackColor="#E3666A",
                                     customOffColor="#FFFFFF")
            ),
            column(6, p(texts_full_list[["IDAT process get_mval"]]) )
          ) ),
          ##############################################
          
          
          
          
          ##############################################
          # Output directory
          ##############################################
          fluidRow( box( 
            width = 12, align="center", 
            getDirPathUI(NS(id,"mod_idat_process_outdir"),
                         dir_button_title = "Output directory:",
                         dir_button_text = "Change output directory",
                         title_choose_panel = "Select output directory")
          ) ),
          ##############################################
          
          
          
          ##############################################
          # Output name
          ##############################################
          fluidRow( box( 
            width = 12, align="center", 
            
            textInput(NS(id,"mod_idat_process_outbasename"), 
                      label = h3("Output basename:"), 
                      value = "output")
            
          ) ),
          ##############################################
          
          
          
          ##############################################
          # Generate report
          ##############################################
          fluidRow( box( 
            width = 12, align="center", 
            generateCommandUI(NS(id,"mod_idat_process_generate"),
                              button_text = "Generate report")
          ) )
          ##############################################
          
  )
  
}

 