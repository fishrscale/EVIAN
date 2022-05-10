# --------------------------------------------
#
# texts_full_list.R
# Contains the full list of texts to be used in the interface.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # multiple tabs
# # DNAm Pct tab
# # Idat Process tab
#
# --------------------------------------------

#######################
# list of texts to be printed
#######################
texts_full_list <- list(
  
  #######################
  # multiple tabs
  #######################
  "controlpop" = HTML(paste(
    "Select the control population file to be compared with the samples.",
    "This file (usually named 'SumStat') contains the summary of 
    DNA methylation distribution per CpG in the control population.",
    "Columns: stats (mean, sd, etc...) / Rows: one CpG per row",
    sep = "<br/>"
  )),
  #######################
  
  
  #######################
  # DNAm Pct tab
  #######################
  "DNAm Pct report" = HTML(paste(
    "This tool generate a report (html format) describing the patterns and 
    enrichment of DNA methylation in regions selected by the user.",
    "The file paths are mandatory arguments and must be filled by the user: the 
    path of the samples, the control, the CpG positions and the regions files.",
    "Click on the 'Generate Report' button at the bottom of the page once you 
    filled the previous options.",
    sep = "<br/>"
  )),
  "DNAm Pct samples" = HTML(paste(
    "Select the file(s) containing patient data to be analyzed.",
    "Columns: one sample per column / Rows: one CpG per row",
    "",
    "Only 10 samples can be analyzed at the same time.",
    "Use the text box to indicate which samples id to keep for the analysis.
    If the box remains empty, all samples from the file will be analyzed.",
    sep = "<br/>"
  )),
  "DNAm Pct regions" = HTML(paste(
    "Provide the regions to be used for the analysis. 
    Regions positions must use the same genome version as the 
    CpG positions genome version.",
    "Columns: chr,start,end,strand,name,group,status / Rows: one region per row",
    "",
    "Fill the 1st text box to select which group of regions to analyze. 
    Leave it empty to analyze all regions (will take more time for the report 
    to be generated).",
    "",
    "Fill the 2nd text box to select which status of regions to analyze 
    (e.g. regions with high confidence, potential DMRs, etc).
    Removed regions could still appear in profiles if they overlap 
    some regions that are still kept.
    Leave it empty to not filter out regions based on status.",
    "",
    "Fill the 3rd text box to select which group of regions 
    to be considered as control regions. 
    Leave it empty to not display any region as control region.",
    sep = "<br/>"
  )),
  "DNAm Pct rds cpgpos" = HTML(paste(
    "Provide the positions of CpGs with the correct genome version.",
    "Columns: ,Chr,Start,Strand / Rows: one region per row",
    ".rds data.",
    sep = "<br/>"
  )),
  "DNAm Pct rds cgi" = HTML(paste(
    "Provide the positions of CpG Islands with the correct genome version.",
    ".rds data.",
    sep = "<br/>"
  )),
  "DNAm Pct rds gene" = HTML(paste(
    "Provide the positions of Genes with the correct genome version.",
    ".rds data.",
    sep = "<br/>"
  )),
  "DNAm Pct rds repeats" = HTML(paste(
    "Provide the positions of Repeats with the correct genome version.",
    ".rds data.",
    sep = "<br/>"
  )),
  "DNAm Pct exportgt" = HTML(paste(
    "Check this box if you want the tables and plots to be exported 
    as pdf files in a small directory (next to the html report).",
    sep = "<br/>"
  )),
  #######################
  
  
  #######################
  # Idat Process tab
  #######################
  "IDAT process" = HTML(paste(
    "This tool process the idat files provided in the input directory and 
    return QC reports describing the quality of each sample provided.",
    "The idat directory path is a mandatory argument and must be filled 
    by the user. The idat files provided must have 1 'red' file and 
    1 'green' file for each dataset.",
    "Click on the 'Launch analysis' button at the bottom of the page once you 
    filled the previous options.",
    sep = "<br/>"
  )),
  
  
  "IDAT process idat_dir" = "",
  "IDAT process trunc" = "",
  "IDAT process qc_report" = "",
  "IDAT process rph" = "",
  "IDAT process rsh" = "",
  "IDAT process bad_mu" = "",
  "IDAT process bad_mu_limit" = "",
  "IDAT process norm" = "",
  "IDAT process remove_snip" = "",
  "IDAT process get_mval" = ""
  
  
  #######################
  
  
)


