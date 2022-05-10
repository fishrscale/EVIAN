# --------------------------------------------
#
# DNAm_Pct.R
# Convert command-line options as R arguments, retrieve absolute paths 
#   and render the DNAm_Pct_report.Rmd file.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Retrieve the script dir
# # Loading arguments
# # Print arguments
# # Get samples names to define html filename
# # Retrieve absolute paths
# # Render Rmd
#
# --------------------------------------------
# Options:
#   -s CHARACTER, --samples_path=CHARACTER
#   sample file(s) path(s). Paths must be separated by a ','.
#   
#   -i CHARACTER, --samples_id_to_check=CHARACTER
#   sample IDs to keep. IDs must be separated by a ','. If NULL, all samples are kept. [default= NULL]
#   
#   -c CHARACTER, --control_path=CHARACTER
#   control population summary file path.
#   
#   -r CHARACTER, --regions_path=CHARACTER
#   region file(s) path(s). Paths must be separated by a ','.
#   
#   -g CHARACTER, --regions_group_to_check=CHARACTER
#   group of regions to be analyzed. Groups must be separated by a ','. If NULL, all regions are analyzed. [default= NULL]
#   
#   -t CHARACTER, --regions_status=CHARACTER
#   status of regions to be analyzed. Status must be separated by a ','. If NULL, all regions are analyzed. [default= NULL]
#   
#   -a CHARACTER, --group_as_ctrls=CHARACTER
#   group of regions to be used as control regions.Groups must be separated by a ','. Only groups of regions that can be analyzed can be used as control regions (cf regions_group_to_check option). If NULL, no regions are used as control regions. [default= NULL]
#   
#   -e CHARACTER, --export_tables_graphs=CHARACTER
#   If TRUE, graphs and tables are also exported outside the html report. [default= FALSE]
#   
#   -d CHARACTER, --outputDir_path=CHARACTER
#   output directory path [default= .]
#   
#   -m CHARACTER, --cpg_positions=CHARACTER
#   CpG positions (.rds format) file path [default= ./DNAm_Pct_report/annot_rds_files/positions-probesEPIC-hg38.rds]
#   
#   -n CHARACTER, --annot_cgi=CHARACTER
#   Annotation CpG Island (.rds format) file path [default= ./DNAm_Pct_report/annot_rds_files/annotCGI-CpG-Islands-cpgIslandExt.rds]
#   
#   -o CHARACTER, --annot_gene=CHARACTER
#   Annotation genes (.rds format) file path [default= ./DNAm_Pct_report/annot_rds_files/annotGene_NCBI-RefSeq_refGene.rds]
#   
#   -p CHARACTER, --annot_repeats=CHARACTER
#   Annotation repeats (.rds format) file path [default= ./DNAm_Pct_report/annot_rds_files/annotRepeats-RepeatMasker-rmsk.rds]
#   
#   -h, --help
#   Show this help message and exit
# --------------------------------------------

# Retrieve the script dir ----
args <- commandArgs()
dirScript <- dirname(gsub(args[ grep(args, pattern = "--file") ],
                          pattern = "--file=", replacement = ""))
if(length(dirScript)==0){ 
  if(rstudioapi::isAvailable()){
    dirScript = normalizePath(
      file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
    )
  } else {
    warning("Could not determine script directory. 
            Setting current directory as script directory.
            This could create issues.")
    dirScript="." 
  }
}

# Loading arguments----
library(optparse)
option_list = list(
  make_option(c("-s", "--samples_path"), type="character",
              help="sample file(s) path(s). Paths must be separated by a ','.", 
              metavar="character"),
  make_option(c("-i", "--samples_id_to_check"), type="character", default=NULL,
              help=
                paste0("sample IDs to keep. IDs must be separated by a ','.", 
                       " If NULL, all samples are kept. [default= %default]"), 
              metavar="character"),
  make_option(c("-c", "--control_path"), type="character", 
              help="control population summary file path.", metavar="character"),
  make_option(c("-r", "--regions_path"), type="character", 
              help="region file(s) path(s). Paths must be separated by a ','.", 
              metavar="character"),
  make_option(c("-g", "--regions_group_to_check"), type="character", default=NULL,
              help=
                paste0("group of regions to be analyzed. Groups must be separated by a ','.", 
                       " If NULL, all regions are analyzed. [default= %default]"), 
              metavar="character"),
  make_option(c("-t", "--regions_status"), type="character", default=NULL,
              help=
                paste0("status of regions to be analyzed. Status must be separated by a ','.", 
                       " If NULL, all regions are analyzed. [default= %default]"), 
              metavar="character"),
  make_option(c("-a", "--group_as_ctrls"), type="character", default=NULL,
              help=
                paste0("group of regions to be used as control regions.", 
                       "Groups must be separated by a ','.", 
                       " Only groups of regions that can be analyzed",
                       " can be used as control regions (cf regions_group_to_check option).",
                       " If NULL, no regions are used as control regions. [default= %default]"), 
              metavar="character"),
  make_option(c("-e", "--export_tables_graphs"), type="character", default="FALSE",
              help=paste0("If TRUE, graphs and tables are also exported",
                          " outside the html report. [default= %default]"), 
              metavar="character"),
  make_option(c("-d", "--outputDir_path"), type="character", default=".",
              help="output directory path [default= %default]", metavar="character"),
  make_option(c("-m", "--cpg_positions"), type="character", 
              default=file.path(dirScript, "annot_rds_files", 
                                "positions-probesEPIC-hg38.rds"),
              help="CpG positions (.rds format) file path [default= %default]", 
              metavar="character"),
  make_option(c("-n", "--annot_cgi"), type="character", 
              default=file.path(dirScript, "annot_rds_files", 
                                "annotCGI-CpG-Islands-cpgIslandExt.rds"),
              help="Annotation CpG Island (.rds format) file path [default= %default]", 
              metavar="character"),
  make_option(c("-o", "--annot_gene"), type="character", 
              default=file.path(dirScript, "annot_rds_files", 
                                "annotGene_NCBI-RefSeq_refGene.rds"),
              help="Annotation genes (.rds format) file path [default= %default]", 
              metavar="character"),
  make_option(c("-p", "--annot_repeats"), type="character", 
              default=file.path(dirScript, "annot_rds_files", 
                                "annotRepeats-RepeatMasker-rmsk.rds"),
              help="Annotation repeats (.rds format) file path [default= %default]", 
              metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
parameters = parse_args(opt_parser)

# Print arguments----
print(paste("Launching Report generation with following args:",
             "-sp", parameters$samples_path,
             "-sitc", parameters$samples_id_to_check,
             "-cpath", parameters$control_path,
             "-cpos", parameters$cpg_positions,
             "-rp", parameters$regions_path,
             "-rgtc ", parameters$regions_group_to_check,
             "-rs", parameters$regions_status,
             "-gac", parameters$group_as_ctrls,
             "-etg", parameters$export_tables_graphs,
            "-dir" = parameters$outputDir_path,
            sep=" "
             ))

# Get samples names to define html filename --------------------------------
samplesPath <- unlist(strsplit(parameters$samples_path, split = ","))
if(length(samplesPath) > 1){
  samples = read.csv(samplesPath[1], row.names = 1, header=TRUE, nrows = 1)
  for (x in samplesPath[-1]) {
    samplesTmp = read.csv(x, row.names = 1, header=TRUE, nrows = 1)
    samples <- cbind(samples, samplesTmp[match(rownames(samples), rownames(samplesTmp)),])
  }
} else {
  samples = read.csv(samplesPath, row.names = 1, header=TRUE, nrows = 1)
}

if(!is.null(parameters$samples_id_to_check)){
  samples = samples[, colnames(samples) %in% 
                      unlist(strsplit(parameters$samples_id_to_check, ",")), drop=FALSE]
}

if(ncol(samples) > 10){ stop("Error: too many samples have been provided/selected! For efficiency purposes, only a maximum of 10 samples is authorized.") }

if(any(duplicated(colnames(samples)))){ stop("Error: Some samples are duplicated in the data provided. Each sample id must be unique.") }

samplesFileBasename <- paste0(colnames(samples), collapse = "__") 
htmlOutputName <- paste0(samplesFileBasename, "_MethPctAnalysis.html")

# Retrieve absolute paths --------------------------------
outputDir_path_abs <- normalizePath(parameters$outputDir_path)

samples_path_abs <- unlist(strsplit(parameters$samples_path, split = ","))
samples_path_abs <- paste0(sapply(samples_path_abs, normalizePath), collapse = ",")

control_path_abs <- normalizePath(parameters$control_path)

regions_path_abs <- unlist(strsplit(parameters$regions_path, split = ","))
regions_path_abs <- paste0(sapply(regions_path_abs, normalizePath), collapse = ",")

cpg_positions_abs <- normalizePath(parameters$cpg_positions)
annot_cgi_abs <- normalizePath(parameters$annot_cgi)
annot_gene_abs <- normalizePath(parameters$annot_gene)
annot_repeats_abs <- normalizePath(parameters$annot_repeats)

# Render Rmd --------------------------------
library(rmarkdown)
render(input = file.path(dirScript,"DNAm_Pct_report.Rmd"),
       params = list(samples_path = samples_path_abs,
                     samples_id_to_check = parameters$samples_id_to_check,
                     control_path = control_path_abs,
                     regions_path = regions_path_abs,
                     regions_group_to_check = parameters$regions_group_to_check,
                     regions_status = parameters$regions_status,
                     group_as_ctrls = parameters$group_as_ctrls,
                     export_tables_graphs = parameters$export_tables_graphs,
                     outputDir_path = outputDir_path_abs,
                     cpg_positions = cpg_positions_abs,
                     annot_cgi = annot_cgi_abs,
                     annot_gene = annot_gene_abs,
                     annot_repeats = annot_repeats_abs),
       output_file = file.path(outputDir_path_abs, htmlOutputName)
       )

