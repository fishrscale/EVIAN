# --------------------------------------------
#
# func_analysis_per_region.R
# Contains the functions (from the DNAm_Pct_report.Rmd) used to 
#   generate the boxplot and table about statistics per region.
# Version 1.0
# Date: 29 April 2022
# Alexis Hardy
# ULB 2022
#
# --------------------------------------------
#
# Functions:
# # Boxplot per region
#   combine_datasets_for_boxplot
#   boxplot_meth_distr_per_region
# # Summary table per region
#   get_region_median_diff_dataset
#   print_methdiff_formatted_table
#
# --------------------------------------------


#######################################
# Boxplot per region
#######################################

# ----
# Function to subset and combine sample, control and regions datasets using
#   probes from all regions to analyze, shift from wide to long format and add
#   a column to check which regions are control regions. Regions must have a
#   column with probes id. ----
# Dependencies: reshape2
# Input:
#   regions_to_analyze - dataset with the region name, positions, groups,
#     status and cpg ids associated. One line per region / no duplicates.
#   sample_meth - methylation % per sample. Rownames: cpg ids / colnames: name
#     of samples.
#   control_meth - methylation % distribution for the control population.
#     Rownames: cpg ids / colnames: name of distribution parameter (mean, min,
#     max, X1, X5, X25, X75, X95, X99 required).
#   group_as_ctrls - vector of groups to be considered as controls: determine
#     which regions are to be considered as controls.
combine_datasets_for_boxplot <- function(regions_to_analyze, sample_meth,
                                         control_meth, group_as_ctrls) {
  # ----
  # Subset sample and control datasets using probes from all regions to analyze,
  #   shift from wide to long format and rename columns/rows. ----
  cgs_vec <- unlist(strsplit(
    regions_to_analyze$probes_epic,
    ","
  ))
  
  sample_meth_subset <-
    sample_meth[rownames(sample_meth) %in% cgs_vec, , drop = FALSE]
  sample_meth_subset$cpg_id <- rownames(sample_meth_subset)
  sample_meth_subset_long <- melt(sample_meth_subset, id.vars = "cpg_id")
  name_samples <- unique(sample_meth_subset_long$variable)
  colnames(sample_meth_subset_long) <- c("cpg_id", "group_samples", "meth_pct")
  
  control_meth_subset_long <-
    control_meth[rownames(control_meth) %in% cgs_vec, "mean", drop = FALSE]
  colnames(control_meth_subset_long) <- "meth_pct"
  control_meth_subset_long$group_samples <- "ctrl"
  control_meth_subset_long$cpg_id <- rownames(control_meth_subset_long)
  rownames(control_meth_subset_long) <- NULL
  control_meth_subset_long <- control_meth_subset_long[, c(3, 2, 1)]
  
  # Combine sample and control datasets. ----
  comb_meth_subset_long <-
    rbind(sample_meth_subset_long, control_meth_subset_long)
  
  # Subset and rename the region dataset. ----
  regions_to_analyze_tmp <-
    regions_to_analyze[, c("group", "name", "probes_epic")]
  regions_to_analyze_tmp$probes_epic <-
    strsplit(regions_to_analyze_tmp$probes_epic, ",")
  regions_to_analyze_tmp <- data.frame(
    group = rep(regions_to_analyze_tmp[, 1],
                sapply(regions_to_analyze_tmp$probes_epic, length)),
    name = rep(regions_to_analyze_tmp[, 2],
               sapply(regions_to_analyze_tmp$probes_epic, length)),
    cpg_id = unlist(regions_to_analyze_tmp$probes_epic)
  )
  
  # Merge sample/control dataset with the modified region dataset. ----
  meth_distr_per_region <-
    merge(regions_to_analyze_tmp, comb_meth_subset_long, by = "cpg_id")
  
  
  # Check if control regions are used to adjust the order of boxplots. ----
  if (!is.null(group_as_ctrls)) {
    meth_distr_per_region$as_ctrl_region <-
      sapply(meth_distr_per_region$group, function(x) {
        any(
          unlist(strsplit(x, "; ")) %in%
            unlist(strsplit(group_as_ctrls, ","))
        )
      })
  } else {
    meth_distr_per_region$as_ctrl_region <- FALSE
  }
  
  return(meth_distr_per_region)
}

# ----
# Function to generate the methylation percentage boxplot for each
#   region/group. ----
# The dataset to provide must have the following format:
# cpg_id  group           name      group_samples  meth_pct  as_ctrl_region
# probe1  groupA; groupB  dmrName1  sample1        40.50000  TRUE
# probe2  groupA; groupC  dmrName1  ctrl           52.05712  TRUE
# Dependencies: RColorBrewer
# Input:
#   meth_distr_per_region - dataset with the cpg id, group, region name,
#     sample/ctrl associated, methylation %, as_ctrl_region and new_order.
#     as_ctrl_region: if region is a control region (logical vector).
#     new_order: combined vector that will be used to determine  the order
#     of boxes to plot (character vector).
boxplot_meth_distr_per_region <- function(meth_distr_per_region) {
  
  # Define sub-functions to be used to compute positions for the boxplot. ----
  # Sub-function to compute positions of each region.
  # Return a vector containing the positions of each region on the x-axis
  compute_region_positions <- function(nb_categories, nb_regions) {
    pos_region <- seq(1,
                      0.5 * (nb_categories - 1) * nb_regions +
                        1 * (nb_regions - 1) +
                        1 - 0.5 * (nb_categories - 1),
                      length.out = nb_regions
    )
    return(pos_region)
  }
  
  # Sub-function to compute positions of boxes for each category and each
  #   region.
  # nb_categories: number of categories per region
  # nb_regions: number of regions
  # pos_regions: vector containing the positions of each region on the x-axis
  compute_box_positions <- function(nb_categories, nb_regions, pos_regions) {
    pos_categories <- seq(-1 * (nb_categories - 1) / 4,
                          (nb_categories - 1) / 4,
                          length.out = nb_categories)
    box_positions <- sort(
      c(sapply(pos_categories, function(x) {
        pos_regions + x
      }))
    )
    return(box_positions)
  }
  
  # Sub-function to retrieve the coordinates of the vertical lines between
  #   regions
  get_vertical_lines_coordinates <- function(pos_regions) {
    width_region <- (pos_regions[2] - pos_regions[1])
    first_line_pos <- pos_regions[1] - width_region / 2
    last_line_pos <-
      pos_regions[length(unique(meth_distr_per_region$name))] + width_region / 2
    return(seq(first_line_pos, last_line_pos, by = width_region))
  }
  
  # ----
  # Sub-function to plot the main panel of the boxplot ----
  boxplot_meth_main_panel <- function(meth_distr_per_region, color_vector,
                                      box_positions, nb_categories, pos_regions,
                                      nb_total_regions, nb_control_regions) {
    
    # ----
    # Boxplot function. Coordinates are retrieved to adjust the names of each
    #   group ----
    myplot <- boxplot(meth_pct ~ new_order,
                      data = meth_distr_per_region,
                      boxwex = 0.4,
                      ylab = "Methylation Percentage (%)", xlab = "",
                      ylim = c(0, 100),
                      col = c(color_vector, "slateblue1"),
                      xaxt = "n", at = box_positions
    )
    
    # Retrieve label names and add them to the x axis ----
    label_names <- sapply(strsplit(myplot$names, "\\,"), function(x) x[[2]])
    label_names <- label_names[
      seq(
        from = 1, to = 1 + length(label_names) - nb_categories,
        by = nb_categories
      )
    ]
    
    axis(1,
         las = 2,
         at = pos_regions,
         labels = label_names,
         tick = FALSE, cex.axis = 0.5
    )
    mtext("Regions", side = 1, line = 7)
    
    # Display a grey polygon above regions to be considered as controls. ----
    if (nb_control_regions > 0) {
      # show control regions
      polygon(
        x = c(
          pos_regions[1 + nb_total_regions - nb_control_regions] -
            (nb_categories + 1) * 0.5 / 2,
          pos_regions[nb_total_regions] + (nb_categories + 1) * 0.5 / 2,
          pos_regions[nb_total_regions] + (nb_categories + 1) * 0.5 / 2,
          pos_regions[1 + nb_total_regions - nb_control_regions] -
            (nb_categories + 1) * 0.5 / 2
        ),
        y = c(150, 150, -50, -50), col = rgb(0.5, 0.5, 0.5, 0.2),
        border = F
      )
    }
    
    # Add the grey vertical lines between regions ----
    lines_pos <- get_vertical_lines_coordinates(pos_regions)
    for (i in lines_pos) {
      abline(v = i, lty = 1, col = "grey")
    }
  }
  
  # ----
  
  # Order data based on sample names levels. ----
  meth_distr_per_region <-
    meth_distr_per_region[order(meth_distr_per_region$group_samples), ]
  
  # Get the number of categories and samples to plot per region. ----
  nb_categories <- length(levels(meth_distr_per_region$group_samples))
  nb_samples <- nb_categories - 1
  
  # Get the number of regions plotted and the number of control regions. ----
  nb_total_regions <- length(unique(meth_distr_per_region$name))
  nb_control_regions <- length(unique(
    meth_distr_per_region$name[meth_distr_per_region$as_ctrl_region]
  ))
  
  # Get the boxes coordinates. ----
  pos_regions <- compute_region_positions(
    nb_categories = nb_categories,
    nb_regions = nb_total_regions
  )
  box_positions <- compute_box_positions(
    nb_categories = nb_categories,
    nb_regions = nb_total_regions,
    pos_regions = pos_regions
  )
  
  # ----
  # Get the color vector for each sample to be plotted along the control
  #   population. ----
  color_vector <-
    brewer.pal(n = max(3, nb_samples), name = "Set2")[1:nb_samples]
  names(color_vector) <-
    levels(meth_distr_per_region$group_samples)[1:nb_samples]
  
  # ----
  # Order boxes by control regions then region names then by sample name
  #   levels. ----
  meth_distr_per_region$new_order <-
    paste0(meth_distr_per_region$as_ctrl_region, ",",
           meth_distr_per_region$name, ",",
           as.numeric(meth_distr_per_region$group_samples))
  
  
  # Adjust layout and plot parameters. ----
  old_mar <- par()$mar
  layout(matrix(1:2, ncol = 2), widths = c(0.8, 0.2))
  par(mar = c(8, 4, 1, 1))
  
  # Plot the main panel of the boxplot ----
  boxplot_meth_main_panel(
    meth_distr_per_region, color_vector, box_positions, nb_categories,
    pos_regions, nb_total_regions, nb_control_regions
  )
  
  # Adjust plot parameters. ----
  par(mar = c(0, 0, 0, 0))
  
  # ----
  # Add a legend using an empty plot. If control regions are provided, the
  #   legend will show an additional section for control regions. ----
  plot.new()
  if (nb_control_regions > 0) {
    legend("top",
           legend = c(names(color_vector),
                      "control \npopulation \nmean",
                      "control \nregions"),
           fill = c(color_vector, "slateblue1", rgb(0.5, 0.5, 0.5, 0.3)),
           bty = "n"
    )
  } else {
    legend("top",
           legend = c(names(color_vector),
                      "control \npopulation \nmean"),
           fill = c(color_vector, "slateblue1"),
           bty = "n"
    )
  }
  
  # Re-initialize plot parameters. ----
  layout(matrix(1))
  par(mar = old_mar)
}




#######################################
# Summary table per region
#######################################

# ----
# Function to generate a dataframe with median methylation % difference per
#   region between control population and each case. ----
# Input:
#   regions_to_analyze - dataset with the region name, positions, groups,
#     status and cpg ids associated. One line per region / no duplicates.
#   regions_all - dataset with the region name, positions, groups and status.
#     One line per group/status. Will be only used to retrieve all the groups
#     (even groups and status removed from the first steps).
#   sample_meth - methylation % per sample. Rownames: cpg ids / colnames: name
#     of samples.
#   control_meth - methylation % distribution for the control population.
#     Rownames: cpg ids / colnames: name of distribution parameter (mean, min,
#     max, X1, X5, X25, X75, X95, X99 required).
get_region_median_diff_dataset <- function(regions_to_analyze, regions_all,
                                         sample_meth, control_meth) {
  
  # Retrieve all groups ----
  regions_to_analyze_tmp <- regions_to_analyze[, c("group", "name", "chr",
                                                   "start", "end", "probes_epic")]
  regions_to_analyze_tmp <-
    regions_to_analyze_tmp[order(regions_to_analyze_tmp$name,
                                 regions_to_analyze_tmp$group), ]
  
  regions_all_tmp <- regions_all[, c("group", "name", "chr", "start", "end")]
  regions_all_tmp <-
    regions_all_tmp[regions_all_tmp$name %in% regions_to_analyze_tmp$name, ]
  regions_all_tmp <-
    regions_all_tmp[order(regions_all_tmp$name, regions_all_tmp$group), ]
  
  regions_to_analyze_tmp$group <-
    sapply(unique(regions_all_tmp$name), function(x) {
      y <- regions_all_tmp[regions_all_tmp$name == x, ][1, ]
      y$group <- paste0(regions_all_tmp[regions_all_tmp$name == x, "group"],
                        collapse = "; ")
      return(y$group)
    })
  
  # Subset table ----
  regions_to_analyze_tmp$position_hg38 <- paste0(
    regions_to_analyze_tmp$chr, ":",
    regions_to_analyze_tmp$start, "-",
    regions_to_analyze_tmp$end
  )
  regions_to_analyze_tmp <-
    regions_to_analyze_tmp[, c("group", "name", "position_hg38", "probes_epic")]
  
  # Compute median methylation % for the control population per region ----
  regions_to_analyze_tmp$median_meth_ctrlpop <-
    sapply(regions_to_analyze_tmp$probes_epic, function(x) {
      median(control_meth[unlist(strsplit(x, ",")), "mean"], na.rm = TRUE)
    })
  
  # Compute median methylation % per case per region ----
  median_meth_per_case <- do.call(
    "rbind",
    lapply(regions_to_analyze_tmp$probes_epic, function(x) {
      apply(sample_meth[unlist(strsplit(x, ",")), , drop = FALSE], 
            2,
            function(x){
              median(x, na.rm = TRUE)
              })
    })
  )
  
  # ----
  # Compute median methylation % difference per region between control and
  #   each case ----
  median_diff_per_case <-
    median_meth_per_case - regions_to_analyze_tmp$median_meth_ctrlpop
  regions_to_analyze_tmp <- cbind(regions_to_analyze_tmp,
                                  median_meth_per_case, median_diff_per_case)
  
  # Clean dataframe and rename columns ----
  regions_to_analyze_tmp$probes_epic <- NULL
  rownames(regions_to_analyze_tmp) <- NULL
  colnames(regions_to_analyze_tmp) <- c(
    "group", "region_name", "position_hg38",
    "MethPct_median_ctrlPop",
    paste0("MethPct_median_", colnames(sample_meth)),
    paste0("MethPct_difference_", colnames(sample_meth))
  )
  
  return(regions_to_analyze_tmp)
}

# ----
# Function to print the regions with the median methylation % difference in a
#   formatted table ----
# Dependencies: kableExtra
# Input:
#   regions_with_methpct - dataset with the group, region name, chr:start-end
#     positions, median Ctrl Pop Meth % and Meth % (median diff) for each sample.
#   name_samples - vector of sample names in the same order than the dataset
#     provided.
#   regions_group_to_check - vector of groups to be highlighted.
#   index_control_regions - index vector of regions to be considered as
#     controls: must have the same order than the regions of dataset provided.
print_methdiff_formatted_table <- function(regions_with_methpct,
                                           name_samples,
                                           regions_group_to_check,
                                           index_control_regions) {
  # ----
  # Sub-function to prepare the color vector based on methylation %
  #   difference ----
  get_diff_color_per_region <- function(regions_with_methpct) {
    color_vector <- colorRampPalette(c(
      rgb(0.50, 0.50, 0.90, 1),
      rgb(0.65, 0.65, 0.95, 1),
      rgb(1.00, 1.00, 1.00, 1),
      rgb(0.95, 0.65, 0.65, 1),
      rgb(0.90, 0.50, 0.50, 1)
    ))(201)
    
    nb_samples <- (ncol(regions_with_methpct) - 4) / 2
    scale_values <- round(
      regions_with_methpct[, (5 + nb_samples):ncol(regions_with_methpct),
                           drop = FALSE
      ] + 101
    )
    
    color_vector <- apply(scale_values, 2, function(x) {
      color_vector[x]
    })
    
    return(color_vector)
  }
  
  # ----
  # Sub-function to check which groups must be colored then insert tags to
  #   color the group text ----
  get_colored_group_text <- function(regions_with_methpct,
                                     regions_group_to_check) {
    
    # ----
    # retrieve list of groups to be colored and create a color vector for
    #   the groups ----
    groups_to_be_colored <- unlist(strsplit(regions_group_to_check, ","))
    group_color_palette <- colorRampPalette(c("#6464AF", "#AF64AF", "#AF6464",
                                              "#AFAF64", "#64AF64", "#64AFAF"))
    color_group_vector <- group_color_palette(length(groups_to_be_colored))
    names(color_group_vector) <- groups_to_be_colored
    
    # ----
    # retrieve list of groups from the dataset that are not to be colored and
    #   create a "white" color vector for these groups ----
    text_groups <- unique(unlist(strsplit(regions_with_methpct$group, "; ")))
    text_groups <- text_groups[!text_groups %in% groups_to_be_colored]
    white_group <- rep("#FFFFFF", length(text_groups))
    names(white_group) <- text_groups
    
    # combine color vectors ----
    color_group_vector <- c(color_group_vector, white_group)
    
    # ----
    # modify the text to remove the ";" and color the background and text of
    #   each group ----
    text_group_vec <- sapply(regions_with_methpct$group, function(x) {
      y <- color_group_vector[match(unlist(strsplit(x, "; ")),
                                    names(color_group_vector))]
      
      paste0(text_spec(unlist(strsplit(x, "; ")),
                       color = ifelse(y == "#FFFFFF", "black", "white"),
                       background = y
      ),
      collapse = "  "
      )
    })
    
    return(text_group_vec)
  }
  
  # ----
  
  # Prepare table to be printed ----
  table_to_be_printed <-
    regions_with_methpct[, -1 * (5:(4 + length(name_samples)))]
  colnames(table_to_be_printed) <- c(
    "group", "region_name", "position_hg38",
    "ctrlPop", name_samples
  )
  
  # Prepare meth diff colors ----
  color_vector <- get_diff_color_per_region(regions_with_methpct)
  
  # Return colored groups ----
  table_to_be_printed$group <-
    get_colored_group_text(regions_with_methpct, regions_group_to_check)
  
  
  # Prepare the base format of the table ----
  tbl <- table_to_be_printed %>%
    kable("html", digits = 3, escape = F) %>%
    kable_styling() %>%
    add_header_above(
      c(" " = 4, "Difference against ctrlPop" = length(name_samples))
    ) %>%
    add_header_above(
      c(" " = 3, "Median methylation Percentage (%)" = 1 + length(name_samples))
    )
  
  # Color background of control regions ----
  if (length(index_control_regions) > 0) {
    tbl <- row_spec(tbl, index_control_regions,
                    background = rgb(0.925, 0.925, 0.925))
  }
  
  # Color case per sample ----
  for (i in 1:length(name_samples)) {
    tbl <- column_spec(tbl, 4 + i, background = color_vector[, i])
  }
  
  
  # Print table ----
  return(tbl)
}



