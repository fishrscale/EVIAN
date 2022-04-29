# --------------------------------------------
#
# IDAT_process_and_QC.R
# Generate IDAT QC reports and process IDAT files  contained in a folder 
#   to retrieve a beta-value file (and a m-values file if required).
#    Based on minfi-pipeline: http://bioconductor.org/help/course-materials/2015/BioC2015/methylation450k.html#reading-data
# Version 1.0
# Date: 29 April 2022
# Robin Grolaux, Alexis Hardy
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
# The available filters:
# 1- remove probes with high detection p-value (default: FALSE)
# 2- remove samples with too much probes with high detection p-value (default: TRUE)
# 3- remove samples with an insufficient Meth/Unmeth signal (default: TRUE)
# 4- remove probes associated to snps (default: TRUE)
# 
# 2 normalization methods are available here:
# 1- stratified quantile (= "quantile") (default) 
# 2- funnorm
# No normalization is performed if only one sample.
# --------------------------------------------
# Options:
#   -i CHARACTER, --idat_dir=CHARACTER
#   idat directory path.
#   
#   -j CHARACTER, --control_path=CHARACTER
#   control population summary file path (only required for the first bval density if running the qc report).  [default= NULL]
#   
#   -t CHARACTER, --truncate_samples_names=CHARACTER
#   should the sample names be truncated? [default= TRUE]
#   
#   -q CHARACTER, --qc_report=CHARACTER
#   generate qc report before processing idat files? [default= TRUE]
#   
#   -p CHARACTER, --remove_probes_highpval=CHARACTER
#   remove probes with high detection p-value? [default= FALSE]
#   
#   -v CHARACTER, --remove_samples_highpval=CHARACTER
#   remove samples with too much probes with high detection p-value? [default= TRUE]
#   
#   -r CHARACTER, --removeSamplesWithBadMethUnmeth=CHARACTER
#   remove samples with an insufficient Meth/Unmeth signal? [default= TRUE]
#   
#   -c CHARACTER, --badMethUnmethSampleCutoff=CHARACTER
#   insufficient Meth/Unmeth signal limit value to remove samples [default= 10.5]
#   
#   -n CHARACTER, --norm_method=CHARACTER
#   normalization methods between samples provided. Only quantile and funnorm methods are available. No normalization is performed if only one sample is provided. [default= quantile]
#   
#   -s CHARACTER, --remove_snip=CHARACTER
#   remove probes associated to snps? [default= TRUE]
#   
#   -m CHARACTER, --get_mval=CHARACTER
#   also retrieve M-val files? [default= TRUE]
#   
#   -o CHARACTER, --out_folder=CHARACTER
#   output directory path [default= .]
#   
#   -b CHARACTER, --output_basename=CHARACTER
#   output base name [default= output]
#   
#   -h, --help
#   Show this help message and exit
# --------------------------------------------

# Retrieve the script dir ----
args <- commandArgs()
dirScript <- dirname(gsub(args[ grep(args, pattern = "--file") ],
                          pattern = "--file=", replacement = ""))
if(length(dirScript)==0){ dirScript="." }

##################################################################
# Parameters description
##################################################################
# Loading arguments----
library(optparse)
option_list = list(
  make_option(c("-i", "--idat_dir"), type="character",
              help="idat directory path.", 
              metavar="character"),
  make_option(c("-j", "--control_path"), type="character", default=NULL,
              help=paste0("control population summary file path ",
                          "(only required for the first bval density ",
                          "if running the qc report).  [default= %default]"), 
              metavar="character"),
  make_option(c("-t", "--truncate_samples_names"), type="character", default="TRUE",
              help="should the sample names be truncated? [default= %default]", 
              metavar="character"),
  make_option(c("-q", "--qc_report"), type="character", default="TRUE",
              help="generate qc report before processing idat files? [default= %default]", 
              metavar="character"),
  make_option(c("-p", "--remove_probes_highpval"), type="character", default="FALSE",
              help="remove probes with high detection p-value? [default= %default]", 
              metavar="character"),
  make_option(c("-v", "--remove_samples_highpval"), type="character", default="TRUE",
              help=paste0("remove samples with too much probes with high detection",
                          " p-value? [default= %default]"), 
              metavar="character"),
  make_option(c("-r", "--removeSamplesWithBadMethUnmeth"), type="character", default="TRUE",
              help=paste0("remove samples with an insufficient Meth/Unmeth signal?",
                          " [default= %default]"), 
              metavar="character"),
  make_option(c("-c", "--badMethUnmethSampleCutoff"), type="character", default="10.5",
              help=paste0("insufficient Meth/Unmeth signal limit value to remove",
                          " samples [default= %default]"), 
              metavar="character"),
  make_option(c("-n", "--norm_method"), type="character", default="quantile",
              help=paste0("normalization methods between samples provided.",
                          " Only quantile and funnorm methods are available.",
                          " No normalization is performed if only one sample",
                          " is provided. [default= %default]"), 
              metavar="character"),
  make_option(c("-s", "--remove_snip"), type="character", default="TRUE",
              help="remove probes associated to snps? [default= %default]", 
              metavar="character"),
  make_option(c("-m", "--get_mval"), type="character", default="TRUE",
              help="also retrieve M-val files? [default= %default]", 
              metavar="character"),
  make_option(c("-o", "--out_folder"), type="character", default=".",
              help="output directory path [default= %default]", metavar="character"),
  make_option(c("-b", "--output_basename"), type="character", default="output",
              help="output base name [default= %default]", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
parameters = parse_args(opt_parser)


##################################################################
# Load libraries
##################################################################
# IDAT preprocessing
library(minfi)
library(IlluminaHumanMethylationEPICmanifest)
library(IlluminaHumanMethylation450kmanifest)

# QC report
library(rmarkdown)


##################################################################
# Launch QC report generation
##################################################################
if (parameters$qc_report) {

  #Retrieve absolute paths
  idat_dir_path_abs <- normalizePath(parameters$idat_dir)
  if(!is.null(parameters$control_path)){
    control_path_abs <- normalizePath(parameters$control_path)
  } else {
    control_path_abs <- NULL
  }
  outputDir_path_abs <- normalizePath(parameters$out_folder)
  
  #Generate QC report
  render(input = file.path(dirScript, "IDAT_qc_report.Rmd"),
         params = list(idat_dir = idat_dir_path_abs,
                       control_path = control_path_abs,
                       truncate_samples_names=parameters$truncate_samples_names,
                       remove_probes_highpval=parameters$remove_probes_highpval,
                       remove_samples_highpval=parameters$remove_samples_highpval,
                       removeSamplesWithBadMethUnmeth=parameters$removeSamplesWithBadMethUnmeth,
                       badMethUnmethSampleCutoff=parameters$badMethUnmethSampleCutoff,
                       remove_snip=parameters$remove_snip),
         output_file = file.path(outputDir_path_abs,
                                 paste0(parameters$output,
                                        "_qc_report.html"))
  )

}




##################################################################
##################################################################
# IDAT processing
##################################################################
##################################################################


##################################################################
# Load IDAT files
##################################################################
#load .IDAT files into RG_channel_set object
RGSet <- read.metharray.exp(parameters$idat_dir)

#Trim sample names
if(parameters$truncate_samples_names == TRUE){#@@@check if FALSE
  sampleNames(RGSet) <- lapply(sampleNames(RGSet), {function(x) sub('_.*', '', x)})
}

RGSet_old <- RGSet




##################################################################
# Load additional annotation library
##################################################################
#Check annotation required and install
annotation_required <- paste0(RGSet@annotation[1], "anno.", RGSet@annotation[2])
if(annotation_required %in% installed.packages()[,1]){
  library(annotation_required, character.only = TRUE)
} else {
  print(paste0("Trying to install... ", annotation_required))
  BiocManager::install(annotation_required, character.only = TRUE, update = FALSE)
  library(annotation_required, character.only = TRUE)
}



##################################################################
# Filter Detection p-values
##################################################################

#Filter using detection P-values from minfi
highPval <- detectionP(RGSet) > 0.01

probes_high_d_pval <- c()
if(parameters$remove_probes_highpval){
  probes_high_d_pval <- rownames(highPval)[rowMeans(highPval) != 0]
}

samples_high_d_pval <- names(which(colMeans(highPval) > 0.01))
if(parameters$remove_samples_highpval){
  if (length(samples_high_d_pval) == length(sampleNames(RGSet))) {
    stop("All samples found to have excess of bad probes (>1%).")
  }
  
  #Drop samples that had a high amount of probes with a bad detection p-value
  RGSet <- RGSet[, !colnames(RGSet) %in% samples_high_d_pval]
}

print(
  paste0(
    length(which(colMeans(highPval) > 0.01)),
    " sample(s) ", 
    ifelse(parameters$remove_samples_highpval, "was/were removed", "should be removed"),
    " because of a high amount of bad probes (>1percent)"
  )
)




##################################################################
# Filter Meth/Unmeth signal
##################################################################

#Compute QC stat
qc <- getQC(preprocessRaw(RGSet))
meds <- (qc$uMed + qc$mMed)/2
samples_bad_methunmeth <- 
  which(!meds > as.numeric(parameters$badMethUnmethSampleCutoff))

#Remove samples with bad Meth/Unmeth signal
if(parameters$removeSamplesWithBadMethUnmeth){
  if (length(samples_bad_methunmeth) == length(sampleNames(RGSet))) {
    stop("All remaining samples found to have insufficient Meth or Unmeth medians.")
  }
  
  #Drop samples that have a bad bad Meth/Unmeth signal
  RGSet <- RGSet[, !colnames(RGSet) %in% samples_bad_methunmeth]
}

print(
  paste0(
    length(samples_bad_methunmeth),
    " sample(s) ", 
    ifelse(parameters$removeSamplesWithBadMethUnmeth, "was/were removed", "should be removed"),
    " because of an insufficient Meth or Unmeth medians."
  )
)





##################################################################
# Normalization
##################################################################

# Normalization
if( length(sampleNames(RGSet)) > 1 ) {
  if (parameters$norm_method == 'quantile'){
    GRset <- preprocessQuantile(RGSet, fixOutliers = TRUE,
                                #removeBadSamples = TRUE, badSampleCutoff = 10.5, @@@todo remove ? not necessary anymore?
                                quantileNormalize = TRUE, stratified = TRUE, 
                                mergeManifest = FALSE, sex = NULL)
  } else {
    GRset <- preprocessFunnorm(RGSet)
  }
} else {
  Mset <- preprocessRaw(RGSet)
  GMset <- mapToGenome(Mset)
  GRset <- ratioConvert(GMset)
}





##################################################################
# Filter Probes associated to SNPs
##################################################################

#Add SNP info to GenomicRanges object
GRset <- addSnpInfo(GRset)

# Drop probes that contain a SNP at the CpG interrogation or at the 
#   single nucleotide extension for any minor allele frequency
if (parameters$remove_snip == TRUE){
  
  probe_count_old <-  length(GRset)
  GRset <- dropLociWithSnps(GRset, snps=c("SBE","CpG"), maf=0)
  probe_count_new <-  length(GRset)
  
  print(
    paste0(probe_count_old - probe_count_new, 
           " probes have been removed because", 
           " they contain either a SNP at the CpG interrogation or",
           " at the single nucleotide extension.")
  )
}






##################################################################
# Filter Probes with bad detection p-value before normalization
##################################################################
if(parameters$remove_probes_highpval){
  #Drop probes that had a bad detection p-value before normalization
  GRset <- GRset[!rownames(GRset) %in% probes_high_d_pval, ]
  
  print(
    paste0(sum(rownames(GRset) %in% probes_high_d_pval), 
           " probes have been removed because", 
           " they have a high detection p-value in at least one",
           " of the samples.")
  )
}




##################################################################
# Retrieve output
##################################################################

#Get Beta_values
beta <- getBeta(GRset)

# Beta-values density comparison before/after filters and normalization
if (parameters$qc_report) {
  
  #Generate QC report
  render(input = file.path(dirScript, "Bval_comparison_qc_report.Rmd"),
         params = list(bval1 = getBeta(RGSet_old),
                       name_bval1="Before normalization and filters",
                       bval2 = beta,
                       name_bval2="After normalization and filters"),
         output_file = file.path(outputDir_path_abs,
                                 paste0(parameters$output,
                                        "_bval_comparison_qc_report.html"))
  )
  
}

#Save
write.csv(beta, 
          file.path(parameters$out_folder, 
                    paste0(parameters$output_basename,'_Beta_val.csv')
                    )
          )

if(parameters$get_mval == TRUE){
  #Get M-values  
  M <- getM(GRset)
  
  #Save
  write.csv(M, 
            file.path(parameters$out_folder, 
                      paste0(parameters$output_basename,'_M_val.csv')
                      )
            )
}

##################################################################

print("IDAT processing finished!")
