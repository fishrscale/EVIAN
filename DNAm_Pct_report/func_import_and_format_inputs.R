# --------------------------------------------
#
# func_import_and_format_inputs.R
# Contains the functions (from the DNAm_Pct_report.Rmd) used to 
#   import and convert the input files.
# Version 1.0
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Functions:
# # General
#   import_and_concatenate
# # Sample files
#   import_meth_samples
# # Control population file
#   import_meth_control
# # Regions file
#   split_regions_to_long_format
#   import_all_regions
#   combine_groups_by_region
#   prepare_regions_to_analyze
#   get_probes_per_region
#   get_regions_with_probes
#
# --------------------------------------------


#######################################
# General
#######################################

# ----
# Import and concatenate files.
#   If more than 1 sample file is provided,
#   a loop is used to bind all samples into 1 dataframe. ----
# Input:
#   paths: character vector: at least one path, one path per element.
#   separator: 	the field separator character. 
#     By default: ",".
#   header: TRUE if first line corresponds to column names. (If FALSE, the 
#     combine_files option will not work with "same_cols" value).
#     By default: TRUE.
#   rownames_column: index of column corresponding to row names. 
#     NULL to not assign any row names (do not work if using the combine_files 
#     option with "same_rows").
#     By default: 1.
#   combine_files: either "same_rows" or "same_cols" to respectively 
#     concactenate rows (cbind) or columns (rbind) while taking in account 
#     respectively the rownames or the header/columns. (the rownames or 
#     colnames must be the same between the different files to be combined).
#     By default: "same_rows".
import_and_concatenate <- function(paths, separator = ",",
                                   header = TRUE, rownames_column = 1,
                                   combine_files = "same_rows"){
  data <- read.table(paths[1], row.names = rownames_column, 
                     header = header, sep = separator)
  if (length(paths) > 1) {
    for (x in paths[-1]) {
      data_tmp <- read.table(x, row.names = rownames_column, 
                             header = header, sep = separator)
      if (combine_files == "same_rows") {
        data <- cbind(data, data_tmp[match(rownames(data), rownames(data_tmp)), ])
      } else if (combine_files == "same_cols") {
        data <- rbind(data, data_tmp[, match(colnames(data), colnames(data_tmp))])
      }
      rm(data_tmp)
    }
  }
  return(data)
}

#######################################
# Sample files
#######################################

# ----
# Import, concatenate, filter and check sample files.
#   If more than 1 sample file is provided,
#   a loop is used to bind all samples into 1 dataframe. 
#   A maximum of 10 samples is authorized and all sample id should be unique. ----
# Input:
#   paths: character vector: at least one path, one path per element.
#   filter_sample_id: a character vector of samples id to be kept. 
#     If NULL, all samples are kept.
#     By default: NULL.
#   convert_to_pct: If TRUE, convert proportions into percentages.
# Additional inputs (should only be used to adjust import to the 
# sample files format):
#   separator: 	the field separator character. 
#     By default: ",".
#   header: TRUE if first line corresponds to column names. (If FALSE, the 
#     combine_files option will not work with "same_cols" value).
#     By default: TRUE.
#   rownames_column: index of column corresponding to row names. 
#     NULL to not assign any row names (do not work if using the combine_files 
#     option with "same_rows").
#     By default: 1.
#   combine_files: either "same_rows" or "same_cols" to respectively 
#     concactenate rows (cbind) or columns (rbind) while taking in account 
#     respectively the rownames or the header/columns. (the rownames or 
#     colnames must be the same between the different files to be combined).
#     By default: "same_rows".
import_meth_samples <- function(paths, 
                                filter_sample_id = NULL,
                                convert_to_pct = TRUE,
                                separator = ",", 
                                header = TRUE, rownames_column = 1, 
                                combine_files = "same_rows"){
  
  # Import and concatenate sample files ----
  sample_meth <- import_and_concatenate(paths = paths, 
                                        separator = separator, 
                                        header = header, 
                                        rownames_column = rownames_column, 
                                        combine_files = combine_files)
  
  # If filter_sample_id parameter is used, filter sample id to keep. ----
  if (length(filter_sample_id) > 0) {
    sample_meth <- sample_meth[, colnames(sample_meth) %in% 
                                 filter_sample_id, 
                               drop = FALSE]
  }
  
  # ----
  # If more than 10 samples are provided, this will return an error to avoid
  #   graphs/tables to be over-compressed and to avoid potential memory
  #   issues. ----
  if (ncol(sample_meth) > 10) {
    stop(
      paste0(
        "Error: too many samples have been provided/selected! ",
        "For efficiency purposes, only a maximum of 10 samples is authorized."
      )
    )
  }
  
  # ----
  # If a same sample id can be found in 2 files, this will return an error to
  #   avoid a sample to be erased by the other one. ----
  if (any(duplicated(colnames(sample_meth)))) {
    stop(paste0("Error: Some samples are duplicated in the data provided.",
                "Each sample id must be unique."))
  }
  
  # If convert_to_pct is TRUE, convert proportions into percentages ----
  if (convert_to_pct){
    sample_meth <- sample_meth * 100
  }
  
  return(sample_meth)
}

  


#######################################
# Control population file
#######################################



# Import and convert control population stat summary file
# Input:
#   path: character vector: file path.
#   parameters_to_keep: a character vector of samples id to be kept. 
#     If NULL, all parameters are kept.
#     By default: c("mean", "max", "min", 
#                   "X1", "X5", "X25", 
#                   "X75", "X95", "X99").
#   convert_to_pct: If TRUE, convert proportions into percentages.
#   header: TRUE if first line corresponds to column names.
#     By default: TRUE.
#   rownames_column: index of column corresponding to row names. 
#     By default: 1.
import_meth_control <- function(path, convert_to_pct = TRUE,
                                parameters_to_keep = c("mean", "max", "min", 
                                                       "X1", "X5", "X25", 
                                                       "X75", "X95", "X99"), 
                                header = TRUE, rownames_column = 1) {
  
  # Import control population file and remove entropy and standard deviation. ----
  control_meth <- read.csv(path, row.names = rownames_column, header = header)
  if (!is.null(parameters_to_keep)) {
    control_meth <- control_meth[, parameters_to_keep]
  }
  
  # If convert_to_pct is TRUE, convert proportions into percentages ----
  if (convert_to_pct){
    control_meth <- control_meth * 100
  }
  
  return(control_meth)
}

  
  


#######################################
# Regions file
#######################################

# ----
# Function to split region's groups: if the dataset provided has some region
#   with multiple groups per line, these lines will be separated into multiple
#   lines: ----
# from
# Region1 groupA        status1
# Region2 groupB,groupC status1,status2
# to
# Region1 groupA        status1
# Region2 groupB        status1
# Region2 groupC        status2
# Function to retrieve the cpg id for the regions to be analyzed.
# Input:
#   regions_all - dataset with the region name, positions, groups and status.
split_regions_to_long_format <- function(regions_all) {
  # ----
  # Regions with multiple groups are retrieved to be duplicated in order to
  #   have one group per line. ----
  index_multiple_groups <- grep(pattern = ",|;", x = regions_all$group)
  if (length(index_multiple_groups) > 0) {
    regions_cleaned <- regions_all[-1 * index_multiple_groups, ]
    regions_to_split <- regions_all[index_multiple_groups, ]
    
    # ----
    # Check that all regions with multiple regions have the same number of
    #   regions than status. ----
    row_vec_group <- rep(
      seq_len(nrow(regions_to_split)),
      nchar(gsub("[^,|;]", "", regions_to_split$group)) + 1
    )
    
    row_vec_status <- rep(
      seq_len(nrow(regions_to_split)),
      nchar(gsub("[^,|;]", "", regions_to_split$status)) + 1
    )
    
    if (length(row_vec_group) != length(row_vec_status) |
        any(row_vec_group != row_vec_status)) {
      stop(
        paste0(
          "Error: some regions do not have the same number of groups and ",
          "status. This could be due to a wrong separator (',' and ';' are ",
          "the only separators authorized) or a missing field. The error can ",
          "be found in each of the following regions: ",
          paste0(regions_to_split[
            which(table(row_vec_group) != table(row_vec_status)), "name"],
            collapse = ",")
        )
      )
    }
    
    # ----
    # Split groups and status, duplicate regions for each additional group then
    #   combine with the regions that had already only one group. ----
    split_group <- gsub(
      pattern = "^ *| *$",
      replacement = "",
      unlist(strsplit(regions_to_split$group, split = ",|;"))
    )
    split_status <- gsub(
      pattern = "^ *| *$",
      replacement = "",
      unlist(strsplit(regions_to_split$status, split = ",|;"))
    )
    
    
    regions_to_split <- regions_to_split[row_vec_group, ]
    regions_to_split$group <- split_group
    regions_to_split$status <- split_status
    
    regions_all <- rbind(
      regions_cleaned,
      regions_to_split
    )
  }
  
  # ----
  # Remove duplicate regions (e.g. if one region had 2 times the same
  #   group). ----
  regions_all <- unique(regions_all)
  
  return(regions_all)
}


# ----
# Import, format and check regions files.
#   If more than 1 region file is provided,
#   a loop is used to bind all samples into 1 dataframe.  ----
#
# Region files must be a tab/tsv file with the 7 following columns:
# chr start end strand  name  group status
#
# Input:
#   paths: character vector: at least one path, one path per element.
# Additional inputs (should only be used to adjust import to the 
# region files format):
#   separator: 	the field separator character. 
#     By default: "\t".
#   header: TRUE if first line corresponds to column names. (If FALSE, the 
#     combine_files option will not work with "same_cols" value).
#     By default: TRUE.
#   rownames_column: index of column corresponding to row names. 
#     NULL to not assign any row names (do not work if using the combine_files 
#     option with "same_rows").
#     By default: NULL.
#   combine_files: either "same_rows" or "same_cols" to respectively 
#     concactenate rows (cbind) or columns (rbind) while taking in account 
#     respectively the rownames or the header/columns. (the rownames or 
#     colnames must be the same between the different files to be combined).
#     By default: "same_rows".
import_all_regions <- function(paths, 
                               separator = "\t", 
                               header = TRUE, 
                               rownames_column = NULL, 
                               combine_files = "same_cols") {
  
  # ----
  # Import region files. If more than 1 region file is provided,
  #   a loop is used to bind all regions into 1 dataframe. ----
  regions_all <- import_and_concatenate(paths = paths, 
                                        separator = "\t", 
                                        header = TRUE, 
                                        rownames_column = NULL, 
                                        combine_files = "same_cols")
  
  # Duplicates are removed. ----
  regions_all <- unique(regions_all)
  
  # Split region's groups: one group/status per line ----
  regions_all <- split_regions_to_long_format(regions_all)
  
  return(regions_all)
}


# ----
# Function to remove duplicate regions and re-combine the different
#   groups/status into a same line: ----
# from
# Region1 groupA        status1
# Region2 groupB        status1
# Region2 groupC        status2
# to
# Region1 groupA        status1
# Region2 groupB,groupC status1,status2
# Input:
#   regions_to_analyze - dataset with the region name, positions, groups and
#     status. One line per group/status.
combine_groups_by_region <- function(regions_to_analyze) {
  # order regions ----
  regions_to_analyze <- regions_to_analyze[order(regions_to_analyze$name,
                                                 regions_to_analyze$group,
                                                 regions_to_analyze$status), ]
  
  # check if there are duplicates ----
  regions_to_analyze <- unique(regions_to_analyze)
  which_are_dup <- which(duplicated(regions_to_analyze[, 1:5]))
  
  if (length(which_are_dup) > 0) {
    # re-order duplicates ----
    dup_regions <- regions_to_analyze[c(which_are_dup, which_are_dup - 1), ]
    dup_regions <- dup_regions[order(dup_regions$name,
                                     dup_regions$group,
                                     dup_regions$status), ]
    dup_regions <- unique(dup_regions)
    
    # ----
    # re-combine lines for regions with multiple groups and status in different
    #   lines ----
    dup_regions <-
      lapply(unique(dup_regions$name), function(x) {
        y <- dup_regions[dup_regions$name == x, ][1, ]
        y$group <-
          paste0(dup_regions[dup_regions$name == x, "group"], collapse = "; ")
        y$status <-
          paste0(dup_regions[dup_regions$name == x, "status"], collapse = "; ")
        return(y)
      })
    dup_regions <- do.call("rbind", dup_regions)
    
    # modify the dataset of regions to analyse ----
    regions_to_analyze <- rbind(
      regions_to_analyze[-1 * c(which_are_dup, which_are_dup - 1), ],
      dup_regions
    )
  }
  return(regions_to_analyze)
}


# ----
# Filter groups and status to be used for the analysis (if the corresponding
#   parameters are used) and combine the regions' groups and status to have
#   one region per line. ----
# Input:
#   regions_all - dataset with the region name, positions (chr, start, end, 
#     strand), groups and status. One line per group/status.
#   regions_status - character vector giving the status of regions to be kept 
#     for the analysis.
#   regions_group_to_check - character vector giving the groups of regions to 
#     be analyzed.
prepare_regions_to_analyze <- function(regions_all, 
                                       regions_status,
                                       regions_group_to_check){
  regions_to_analyze <- regions_all
  
  #filter regions based on status
  if (!is.null(params$regions_status)) {
    regions_to_analyze <- 
      regions_to_analyze[regions_to_analyze$status %in%
                           unlist(strsplit(params$regions_status, ",")), ]
  }
  
  #filter regions based on groups
  if (!is.null(params$regions_group_to_check)) {
    regions_to_analyze <- 
      regions_to_analyze[regions_to_analyze$group %in%
                           unlist(strsplit(params$regions_group_to_check, ",")), ]
  }
  
  # ----
  # For the regions to analyze: remove duplicate regions and re-combine the
  #   different groups/status into one line:
  regions_to_analyze <- combine_groups_by_region(regions_to_analyze)
  
  return(regions_to_analyze)
}

# Function to retrieve the cpg id for the regions to be analyzed. ----
# Dependencies: GenomicRanges
# Input:
#   regions_to_analyze - dataset with the region name, positions, groups and
#     status. One line per region / no duplicates.
#   positions - dataset with positions of each cpg. The rownames must
#     correspond to the cpg ids.
get_probes_per_region <- function(regions_to_analyze, positions) {
  # ----
  # Filter the cpg positions based on regions to reduce the memory requirement
  #   of GPos transformation ----
  positions_tmp <-
    positions[positions$chr %in% unique(regions_to_analyze$chr), ]
  positions_tmp <- positions_tmp[
    positions_tmp$start >= min(regions_to_analyze$start) &
      positions_tmp$start <= max(regions_to_analyze$end),
  ]
  
  # ----
  # Convert filtered cpg positions and regions to analyze as GPos and GRanges
  #   objects ----
  positions_tmp <- GPos(
    seqnames = positions_tmp$chr,
    pos = positions_tmp$start,
    strand = positions_tmp$strand,
    name = rownames(positions_tmp)
  )
  
  regions_to_analyze_tmp <- GRanges(
    seqnames = regions_to_analyze$chr,
    ranges = IRanges(
      start = regions_to_analyze$start,
      end = regions_to_analyze$end
    ),
    strand = regions_to_analyze$strand,
    name = regions_to_analyze$name,
    group = regions_to_analyze$group,
    status = regions_to_analyze$status
  )
  
  # ----
  # Find overlaps between cpg positions and regions to analyze and retrieve the
  #   id of probes overlapping each region ----
  hits <- findOverlaps(regions_to_analyze_tmp,
                       positions_tmp,
                       ignore.strand = TRUE)
  hits <- as.data.frame(hits)
  hits <- hits[order(hits$queryHits, decreasing = FALSE), ]
  probes <-
    sapply(1:nrow(regions_to_analyze), function(x) {
      paste0(
        positions_tmp$name[
          hits$subjectHits[hits$queryHits == x]
        ],
        collapse = ","
      )
    })
  
  return(probes)
}


# Function to retrieve and check the cpg id for the regions to be analyzed. ----
# Dependencies: GenomicRanges
# Input:
#   regions_to_analyze - dataset with the region name, positions, groups and
#     status. One line per region / no duplicates.
#   positions - dataset with positions of each cpg. The rownames must
#     correspond to the cpg ids.
get_regions_with_probes <- function(regions_to_analyze, positions){
  
  # Retrieve the cpg id per region. ----
  regions_to_analyze$probes_epic <-
    get_probes_per_region(regions_to_analyze, positions)
  
  # ----
  # Check that at least some regions have overlapping probes. Regions to analyze
  #   without any overlapping probe are removed. ----
  if (all(regions_to_analyze$probes_epic == "")) {
    stop("All regions selected have no probes to be analyzed!")
  } else if (any(regions_to_analyze$probes_epic == "")) {
    warning(
      paste0(
        "The following regions selected have no probes to be analyzed thus were ",
        "removed from the analysis:\n",
        paste0(regions_to_analyze$name[regions_to_analyze$probes_epic == ""],
               collapse = ",")
      )
    )
    regions_to_analyze <-
      regions_to_analyze[regions_to_analyze$probes_epic != "", ]
  }
  
  return(regions_to_analyze)
}


