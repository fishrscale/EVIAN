# --------------------------------------------
#
# tabIdatProcessServer.R
# Contains the tabIdatProcessServer function that control the tabIdatProcessUI.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Retrieve volumes and directories
# # Server modules to be linked with corresponding ui modules
# # Define params for the command to generate
# # Module to generate the command
#
# --------------------------------------------

tabIdatProcessServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ##############################################
    # Retrieve volumes and directories
    ##############################################
    volumes = getVolumes()
    roots_s_first <- reactive(c(evian_directory = dirScript,
                                user_current_directory = userCurrentDir,
                                home = '~',
                                all_spaces_available = volumes()))
    roots_u_first <- reactive(c(user_current_directory = userCurrentDir,
                                evian_directory = dirScript,
                                home = '~',
                                all_spaces_available = volumes()))
    roots_u_novol <- reactive(c(evian_directory = dirScript,
                                user_current_directory = userCurrentDir,
                                home = '~'))
    ##############################################
    
    
    
    ##############################################
    # Server modules to be linked with corresponding ui modules
    ##############################################
    idat_dir <- getDirPathServer("mod_idat_process_idat_dir", 
                                 roots = roots_u_novol, 
                                 default_directory = NULL,
                                 filetypes = c('', 'idat'))
    control_path <- getPathInputServer("mod_idat_process_controlpop", 
                                       roots = roots_u_first)
    truncate_samples_names <- switchTrueFalseServer("mod_idat_process_trunc")
    qc_report <- switchTrueFalseServer("mod_idat_process_qc_report")
    remove_probes_highpval <- switchTrueFalseServer("mod_idat_process_rph")
    remove_samples_highpval <- switchTrueFalseServer("mod_idat_process_rsh")
    removeSamplesWithBadMethUnmeth <- 
      switchTrueFalseServer("mod_idat_process_bad_mu")
    
    badMethUnmethSampleCutoff <- reactive(input$mod_idat_process_bad_mu_limit)
    norm_method <- reactive(input$mod_idat_process_norm)
    
    remove_snip <- switchTrueFalseServer("mod_idat_process_remove_snip")
    get_mval <- switchTrueFalseServer("mod_idat_process_get_mval")
    out_folder <- getDirPathServer("mod_idat_process_outdir", 
                                   roots = roots_u_novol, 
                                   default_directory = userCurrentDir)
    
    out_basename <- reactive(input$mod_idat_process_outbasename)
    ##############################################
    
    
    
    ##############################################
    # Define params for the command to generate
    ##############################################
    param_named_vector <- reactive(c(
      "idat_dir" = idat_dir(),
      "control_path" = control_path(),
      "truncate_samples_names" = truncate_samples_names(),
      "qc_report" = qc_report(),
      "remove_probes_highpval" = remove_probes_highpval(),
      "remove_samples_highpval" = remove_samples_highpval(),
      "removeSamplesWithBadMethUnmeth" = removeSamplesWithBadMethUnmeth(),
      "badMethUnmethSampleCutoff" = badMethUnmethSampleCutoff(),
      "norm_method" = norm_method(),
      "remove_snip" = remove_snip(),
      "get_mval" = get_mval(),
      "out_folder" = out_folder(),
      "output_basename" = out_basename()))
    ##############################################
    
    
    ##############################################
    # Module to generate the command
    ##############################################
    generateCommandServer(
      "mod_idat_process_generate",
      script_launch_command = "Rscript",
      script_path = file.path(dirScript, 
                              'IDAT_processing_qc_report', 
                              'IDAT_process_and_QC.R'),
      param_named_vector = param_named_vector,
      params_to_check = c("idat_dir", "out_folder", "output_basename"),
      # block_if_missing = FALSE,
      # text_warning_check = paste0("Some paths are missing! ",
      #                             "This could return errors ",
      #                             "during files processing or QC."),
      # text_warning_bypass = "Launch analysis anyway?"
      block_if_missing = TRUE,
      text_warning_check = "",
      text_warning_bypass = ""
    )
    ##############################################
    
  })
}
#######################

