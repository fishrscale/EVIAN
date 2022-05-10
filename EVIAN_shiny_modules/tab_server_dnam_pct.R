# --------------------------------------------
#
# tabDnamPctServer.R
# Contains the tabDnamPctServer function that control the tabDnamPctUI.
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

tabDnamPctServer <- function(id) {
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
    samples_input <- getPathFiltColsInputServer("mod_dnampctrep_samples", 
                                                roots = roots_u_first)
    controlpop_path <- getPathInputServer("mod_dnampctrep_controlpop", 
                                          roots = roots_u_first)
    regions_input <- getPathFiltRegionsInputServer("mod_dnampctrep_regions", 
                                                   roots = roots_u_first)
    rds_cpgpos_path <- getPathInputServer("mod_dnampctrep_rds_cpgpos", 
                                          roots = roots_s_first)
    rds_cgi_path <- getPathInputServer("mod_dnampctrep_rds_cgi", 
                                       roots = roots_s_first)
    rds_gene_path <- getPathInputServer("mod_dnampctrep_rds_gene", 
                                        roots = roots_s_first)
    rds_repeats_path <- getPathInputServer("mod_dnampctrep_rds_repeats", 
                                           roots = roots_s_first)
    export_gt <- switchTrueFalseServer("mod_dnampct_exportgt")
    outdir_path <- getDirPathServer("mod_dnampctrep_outdir", 
                                    roots = roots_u_novol, 
                                    default_directory = userCurrentDir)
    ##############################################
    
    
    ##############################################
    # Define params for the command to generate
    ##############################################
    param_named_vector <- reactive(c(
      "samples_path" = samples_input$data_path(),
      "samples_id_to_check" =
        paste0(samples_input$columns_to_keep(), collapse = ","),
      "control_path" = controlpop_path(),
      "regions_path" = regions_input$data_path(),
      "regions_group_to_check" =
        paste0(regions_input$regions_group_to_check(), collapse = ","),
      "regions_status" =
        paste0(regions_input$regions_status(), collapse = ","),
      "group_as_ctrls" =
        paste0(regions_input$group_as_ctrls(), collapse = ","),
      "cpg_positions" = rds_cpgpos_path(),
      "annot_cgi" = rds_cgi_path(),
      "annot_gene" = rds_gene_path(),
      "annot_repeats" = rds_repeats_path(),
      "export_tables_graphs" = export_gt(),
      "outputDir_path" = outdir_path()))
    ##############################################
    
    
    ##############################################
    # Module to generate the command
    ##############################################
    generateCommandServer(
      "mod_dnampctrep_generate",
      script_launch_command = "Rscript",
      script_path = file.path(dirScript, 'DNAm_Pct_report', 'DNAm_Pct.R'),
      param_named_vector = param_named_vector,
      params_to_check = c("samples_path", "control_path", 
                          "regions_path", "outputDir_path"),
      block_if_missing = FALSE,
      text_warning_check = paste0("Some paths are missing! ",
                                  "This could return errors ",
                                  "during report generation."),
      text_warning_bypass = "Generate report anyway?"
    )
    ##############################################
    
  })
}
#######################

