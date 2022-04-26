# --------------------------------------------
#
# func_analysis_global.R
# Contains the functions (from the DNAm_Pct_report.Rmd) used to 
#   generate the information table and the manhattan plot data.
# Version 1.0
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Functions:
# # Infos Tables
#   generate_info_dataframe
# # Manhattan plot
#   get_list_for_manhanttan_plot
#
# --------------------------------------------

#######################################
# Infos Tables
#######################################

# ----
# Generate the dataframe with the general information about the analysis.
#   Regions must have a column with probes id. ----
generate_info_dataframe <- function(sample_meth, regions_to_analyze,
                                    report_version,
                                    control_path,
                                    group_as_ctrls) {
  
  # create the base dataframe and names vector ----
  info_dataframe <- data.frame(
    Sys.time(),
    as.character(report_version)
  )
  
  info_names <- c("Date of report", "Software version")
  
  # ----
  # add info about sample and control files to the base dataframe and
  #   names vector ----
  info_dataframe <- cbind(
    info_dataframe,
    ncol(sample_meth),
    paste0(colnames(sample_meth), collapse = " "),
    basename(control_path)
  )
  
  info_names <- c(
    info_names,
    "Nb of tested samples",
    "Names of tested samples", "Control population filename"
  )
  
  # get info about regions to analyze ----
  list_probes <- strsplit(
    regions_to_analyze$probes_epic,
    ","
  )
  nb_tested_probes <- length(unlist(list_probes))
  
  regions_width <- regions_to_analyze$end - regions_to_analyze$start
  
  mean_width_regions <- mean(regions_width)
  range_width_regions <- paste0(range(regions_width), collapse = "-")
  
  mean_nb_cpg_per_region <- mean(sapply(list_probes, length))
  range_nb_cpg_per_region <-
    paste0(range(sapply(list_probes, length)), collapse = "-")
  
  # ----
  # add info about regions to analyze to the base dataframe and names
  #   vector ----
  info_dataframe <- cbind(
    info_dataframe,
    nb_tested_probes,
    length(unique(regions_to_analyze$name)),
    mean_width_regions,
    range_width_regions,
    mean_nb_cpg_per_region,
    range_nb_cpg_per_region
  )
  
  info_names <- c(
    info_names,
    "Nb of tested cpg probes", "Nb of tested regions",
    "Mean size of tested regions",
    "Min|Max size of tested regions",
    "Mean probe count per tested region",
    "Min|Max probe count per tested region"
  )
  
  
  # ----
  # add info about regions to analyze that are also control regions (if the
  #   group_as_ctrls parameter is used) to the base dataframe and names
  #   vector ----
  if (!is.null(group_as_ctrls)) {
    # retrieve regions to analyze that are also control regions
    index_control_regions <- which(
      sapply(regions_to_analyze$group, function(x) {
        any(
          unlist(strsplit(x, "; ")) %in%
            unlist(strsplit(group_as_ctrls, ","))
        )
      })
    )
    name_control_regions <- regions_to_analyze$name[index_control_regions]
    
    # get info about regions to analyze that are also control regions
    list_probes <- strsplit(
      regions_to_analyze$probes_epic[
        regions_to_analyze$name %in% name_control_regions
      ],
      ","
    )
    
    nb_control_probes <- length(unlist(list_probes))
    
    regions_width <-
      regions_to_analyze[regions_to_analyze$name %in% name_control_regions, ]
    regions_width <- regions_width$end - regions_width$start
    
    mean_width_regions <- mean(regions_width)
    range_width_regions <- paste0(range(regions_width), collapse = "-")
    
    mean_nb_cpg_per_region <- mean(sapply(list_probes, length))
    range_nb_cpg_per_region <-
      paste0(range(sapply(list_probes, length)), collapse = "-")
    
    # ----
    # add info about regions to analyze that are also control regions to the
    #   base dataframe and names vector ----
    info_dataframe <- cbind(
      info_dataframe,
      nb_control_probes,
      length(name_control_regions),
      mean_width_regions,
      range_width_regions,
      mean_nb_cpg_per_region,
      range_nb_cpg_per_region
    )
    
    info_names <- c(
      info_names,
      "Nb of tested control cpg probes", "Nb of control regions",
      "Mean size of control regions",
      "Min|Max size of control regions",
      "Mean probe count per control region",
      "Min|Max probe count per control region"
    )
  }
  
  # Building dataframe----
  info_dataframe <- t(info_dataframe)
  info_dataframe <- cbind(info_dataframe, info_dataframe)
  info_dataframe[, 1] <- info_names
  rownames(info_dataframe) <- NULL
  
  return(info_dataframe)
}


#######################################
# Manhattan plot
#######################################

# ----
# Function to retrieve the cpg id for the regions to be analyzed. Return a
#   list with: 1- the difference between each sample and the control population
#   mean for each cpg; 2- and the positions of each chr/label on the manhattan
#   plot. ----
# Input:
#   sample_meth - methylation % per sample. Rownames: cpg ids / colnames: name
#     of samples.
#   control_meth - methylation % distribution for the control population.
#     Rownames: cpg ids / colnames: name of distribution parameter (mean, min,
#     max, X1, X5, X25, X75, X95, X99 required).
#   positions - dataset with positions of each cpg. The rownames must
#     correspond to the cpg ids.
get_list_for_manhanttan_plot <- function(sample_meth, control_meth, positions) {
  
  # Order probes between sample, control and cpg position datasets. ----
  nb_samples <- ncol(sample_meth)
  sample_meth_subset <-
    sample_meth[rownames(sample_meth) %in% rownames(control_meth), ]
  sample_meth_subset <-
    sample_meth_subset[order(rownames(sample_meth_subset)), ]
  
  control_meth_subset <-
    control_meth[rownames(control_meth) %in% rownames(sample_meth_subset), ]
  control_meth_subset <-
    control_meth_subset[order(rownames(control_meth_subset)), ]
  
  positions2 <-
    positions[rownames(positions) %in% rownames(sample_meth_subset), ]
  positions2 <- positions2[order(rownames(positions2)), ]
  
  # ----
  # Check that probes were correctly ordered between sample, control and cpg
  #   position datasets. If the order is correct, prepare the samples dataframe
  #   for the manhattan plot. ----
  are_all_probes_id_aligned <-
    all(rownames(control_meth_subset) == rownames(sample_meth_subset)) &
    all(rownames(positions2) == rownames(sample_meth_subset))
  if (are_all_probes_id_aligned) {
    
    # ----
    # Substract samples methylation % with the mean control population
    #   methylation % ----
    sample_methdiff_subset <- sample_meth_subset - control_meth_subset$mean
    
    # Combine samples methylation % with the probes positions ----
    sample_methdiff_subset <- cbind(sample_methdiff_subset, positions2)
    
    # Reorder chromosomes from chr1 to chr22 then the rest ----
    order_chr <- paste0("chr", 1:22)
    order_chr <- c(
      order_chr,
      unique(sample_methdiff_subset$chr)[
        !unique(sample_methdiff_subset$chr) %in% order_chr]
    )
    sample_methdiff_subset$chr <-
      factor(sample_methdiff_subset$chr, levels = order_chr)
    
    # Reorder dataset ----
    sample_methdiff_subset <- sample_methdiff_subset[
      order(sample_methdiff_subset$chr, sample_methdiff_subset$start), ]
    
    # Prepare the color vector and assign it to chromosomes ----
    color_vector <- rep(c("black", "grey"),
                        length(levels(sample_methdiff_subset$chr)))
    color_vector <- color_vector[1:length(levels(sample_methdiff_subset$chr))]
    sample_methdiff_subset$color_chr <- sample_methdiff_subset$chr
    levels(sample_methdiff_subset$color_chr) <- color_vector
    rm(color_vector)
    
    # ----
    # Compute the positions of the chromosomes on the x-axis and associate the
    #   label/chr names ----
    x <- cumsum(table(sample_methdiff_subset$chr))
    labels_pos <- c(0, x[-length(x)]) + ((x - c(0, x[-length(x)])) / 2)
    names(labels_pos) <- names(x)
    
    # Prepare list to return ----
    list_to_return <- list(
      "sample_methdiff_subset" = sample_methdiff_subset,
      "labels_pos" = labels_pos
    )
  } else {
    print(paste0("Error while generating Manhattan plot: probes could not be",
                 "aligned between cases and control population."))
    list_to_return <- NULL
  }
  
  return(list_to_return)
}




