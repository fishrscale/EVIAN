# --------------------------------------------
#
# Generate_RDS_files.R
# Retrieve/download input files and save them as RDS files.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Steps:
# # Loading arguments
# # Load Library
# # Generate RDS positions
# # Load UCSC session to retrieve data
# # Generate RDS genes
# # Generate RDS CGI
# # Generate RDS Repeats
#
# --------------------------------------------
# Options:
#   -c CHARACTER, --rds_cpg_pos=CHARACTER
#   If TRUE, generate the rds file for cpg position annotation. The path of cpg_positions csv file must be provided. [default= TRUE]
#   
#   -p CHARACTER, --cpg_positions_path=CHARACTER
#   CpG positions (.csv format) file path. [default= NULL]
#   
#   -g CHARACTER, --rds_genes=CHARACTER
#   If TRUE, generate the rds file for genes annotation. [default= TRUE]
#   
#   -i CHARACTER, --rds_cgi=CHARACTER
#   If TRUE, generate the rds file for CpG Islands annotation. [default= TRUE]
#   
#   -r CHARACTER, --rds_repeats=CHARACTER
#   If TRUE, generate the rds file for repeats annotation. [default= TRUE]
#   
#   -v CHARACTER, --genome_version=CHARACTER
#   Genome version to import for genes/cgi/repeats rds files generation [default= hg38]
#   
#   -o CHARACTER, --output_dir=CHARACTER
#   Output directory for the different rds files generated. [default= .]
#   
#   -h, --help
#   Show this help message and exit
# --------------------------------------------

###########################################################################
#Loading arguments
###########################################################################
library(optparse)
option_list = list(
  make_option(c("-c", "--rds_cpg_pos"), type="character", default="TRUE",
              help=paste0("If TRUE, generate the rds file for cpg position annotation.",
                          " The path of cpg_positions csv file must be provided. [default= %default]"), 
              metavar="character"),
  make_option(c("-p", "--cpg_positions_path"), type="character", default=NULL,
              help="CpG positions (.csv format) file path. [default= %default]", 
              metavar="character"),
  make_option(c("-g", "--rds_genes"), type="character", default="TRUE",
              help=paste0("If TRUE, generate the rds file for genes annotation.",
                          " [default= %default]"), 
              metavar="character"),
  make_option(c("-i", "--rds_cgi"), type="character", default="TRUE",
              help=paste0("If TRUE, generate the rds file for CpG Islands annotation.",
                          " [default= %default]"), 
              metavar="character"),
  make_option(c("-r", "--rds_repeats"), type="character", default="TRUE",
              help=paste0("If TRUE, generate the rds file for repeats annotation.",
                          " [default= %default]"), 
              metavar="character"),
  make_option(c("-v", "--genome_version"), type="character", default="hg38",
              help=paste0("Genome version to import for genes/cgi/repeats rds files generation",
                          " [default= %default]"), 
              metavar="character"),
  make_option(c("-o", "--output_dir"), type="character", default=".",
              help=paste0("Output directory for the different rds files generated.",
                          " [default= %default]"), 
              metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
parameters = parse_args(opt_parser)


###########################################################################
#Load Library
###########################################################################
library(rtracklayer)


###########################################################################
#Generate RDS positions
###########################################################################
if(parameters$rds_cpg_pos == TRUE){
  positions <- read.csv(parameters$cpg_positions_path, row.names = 1, header = TRUE)
  colnames(positions) <- c("chr", "start", "strand")
  saveRDS(positions, file = file.path(parameters$output_dir, 
                                      "test_positions-probesEPIC-hg38.rds"))
}

###########################################################################
#Load UCSC session to retrieve data (requires internet connexion)
###########################################################################
session <- browserSession()
genome(session) <- parameters$genome_version


###########################################################################
#Generate RDS genes
###########################################################################
if(parameters$rds_genes == TRUE){
  query <- ucscTableQuery(session, track="NCBI RefSeq",
                          table="refGene")
  annot_gene_list <- getTable(query)
  saveRDS(annot_gene_list, file = file.path(parameters$output_dir, 
                                            "test_annotGene_NCBI-RefSeq_refGene.rds"))
}

###########################################################################
#Generate RDS CGI
###########################################################################
if(parameters$rds_cgi == TRUE){
  query <- ucscTableQuery(session, track="CpG Islands",
                          table="cpgIslandExt")
  annot_CGI_list <- getTable(query)
  saveRDS(annot_CGI_list, file = file.path(parameters$output_dir, 
                                           "test_annotCGI-CpG-Islands-cpgIslandExt.rds"))
  
}

###########################################################################
#Generate RDS Repeats
###########################################################################
if(parameters$rds_repeats == TRUE){
  #Too many requests if directly using the following commands:
  # query <- ucscTableQuery(session, track="RepeatMasker",
  #                         table="rmsk")
  # annot_repeats_list <- getTable(query)
  # saveRDS(annot_CGI_list, file = file.path(parameters$output_dir, 
  #                                          "test_annotRepeats-RepeatMasker-rmsk.rds"))

  #Alternative:
  #Download rmsk.txt.gz
  system(paste0("wget -O rmsk.txt.gz https://hgdownload.soe.ucsc.edu/goldenPath/",
                parameters$genome_version,
                "/database/rmsk.txt.gz"))
  
  # Open rmsk.txt.gz file and save annot_repeats RDS -------------------------------
  annot_CGI_list <- read.table("rmsk.txt.gz", row.names = NULL, 
                               header = FALSE, sep = "\t")
  colnames(annot_CGI_list) <- c("bin", "swScore", "milliDiv", "milliDel", "milliIns", 
                                "genoName", "genoStart", "genoEnd", "genoLeft", "strand",
                                "repName", "repClass", "repFamily", 
                                "repStart", "repEnd", "repLeft","id")
  
  saveRDS(annot_CGI_list, file = file.path(parameters$output_dir, 
                                           "test_annotRepeats-RepeatMasker-rmsk.rds"))
  system("rm rmsk.txt.gz")
  
}
