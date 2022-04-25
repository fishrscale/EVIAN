


# ----
# Subset a dataset (with chr/start/end columns) based on the region defined by
#   the chr_vec/start_vec/end_vec parameters. ----
# Input:
#   dataset - dataset with the chr/start/end columns.
#   chr_vec, start_vec, end_vec - Respectively the chr, start and end
#     positions to be used for filtering the dataset.
subset_dataset_based_on_region <- function(dataset, chr_vec,
                                           start_vec, end_vec) {
  return(dataset[dataset$chr == chr_vec &
                   !(dataset$end < start_vec | dataset$start > end_vec), ])
}

# ----
# Subset each dataset based on the region defined by the
#   chr_vec/start_vec/end_vec parameters. Sample and control datasets are also
#   merged with cpg positions. ----
# Input:
#   sample_meth - methylation % per sample. Rownames: cpg ids / colnames: name
#     of samples.
#   control_meth - methylation % distribution for the control population.
#     Rownames: cpg ids / colnames: name of distribution parameter (mean, min,
#     max, X1, X5, X25, X75, X95, X99 required).
#   positions - dataset with positions of each cpg. The rownames must
#     correspond to the cpg ids.
#   annot_gene_list - used to plot the gene panels. Dataset with the chr,
#     start, end, strand columns. Must also have a column gene_name. Columns
#     exonStarts and exonEnds are used to plot exons.
#   annot_cgi_list - used to plot the cpg island panels. Dataset with the chr,
#     start, end.
#   annot_repeats_list - used to plot the repeats panels. Dataset with the chr,
#     start, end.
#   regions_all - used to plot the regions panels. Dataset with the region name,
#     positions, groups and status. One line per group/status.
#   chr_vec, start_vec, end_vec - Respectively the chr, start and end positions
#     to be used to delimitate the region to plot.
get_datalist_based_on_region <- function(sample_meth, control_meth, positions,
                                         annot_gene_list, annot_cgi_list,
                                         annot_repeats_list, regions_all,
                                         chr_vec, start_vec, end_vec) {
  
  # Subset cpg position dataset ----
  positions2 <- positions[positions$chr == chr_vec &
                            !(positions$start < start_vec - 1 | positions$start > end_vec + 1), ]
  positions2$strand <- NULL
  
  # Retrieve cpg ids ----
  cgs_vec <- rownames(positions2)
  
  # Subset sample and control datasets ----
  sample_meth_subset <-
    sample_meth[rownames(sample_meth) %in% cgs_vec, , drop = FALSE]
  control_meth_subset <-
    control_meth[rownames(control_meth) %in% cgs_vec, , drop = FALSE]
  
  # Merge sample and control datasets with cpg positions ----
  sample_meth_subset_with_pos <-
    merge(positions2, sample_meth_subset, by = "row.names")
  control_meth_subset_with_pos <-
    merge(positions2, control_meth_subset, by = "row.names")
  
  # Order sample and control datasets ----
  sample_meth_subset_with_pos <-
    sample_meth_subset_with_pos[order(sample_meth_subset_with_pos$start), ]
  control_meth_subset_with_pos <-
    control_meth_subset_with_pos[order(control_meth_subset_with_pos$start), ]
  
  # Subset gene annotation dataset ----
  annot_gene <- subset_dataset_based_on_region(
    annot_gene_list,
    chr_vec, start_vec, end_vec
  )
  
  # Subset cgi annotation dataset ----
  annot_cgi <- subset_dataset_based_on_region(
    annot_cgi_list,
    chr_vec, start_vec, end_vec
  )
  
  # Subset repeats annotation dataset ----
  annot_repeats <- subset_dataset_based_on_region(
    annot_repeats_list,
    chr_vec, start_vec, end_vec
  )
  
  # Subset regions to plot dataset ----
  regions_all_subset <- subset_dataset_based_on_region(
    regions_all,
    chr_vec, start_vec, end_vec
  )
  
  return(list(
    "sample" = sample_meth_subset_with_pos,
    "control" = control_meth_subset_with_pos,
    "gene" = annot_gene,
    "cgi" = annot_cgi,
    "repeats" = annot_repeats,
    "regions_to_plot" = regions_all_subset
  ))
}


# ----
# Macro function to plot a profile of methylation % for a few samples along the
#   distribution of the control population, the CGI density, the repeats
#   density, and the annotation from the regions and the genes provided. This
#   function plots 5 panels (corresponding to 5 tracks) + 5 side-panels
#   (containing corresponding legend). ----
# Dependencies: RColorBrewer
# Input:
#   sample_meth_subset_with_pos - methylation % per sample. Wide format: must
#     have at least the start column and one column per sample. Should be
#     filtered for the right chromosome to plot.
#   control_meth_subset_with_pos - methylation % distribution for the control
#     population. Columns: must have at least the start column then the
#     following distribution parameters: mean, min, max, X1, X5, X25, X75, X95,
#     X99. Should be filtered for the right chromosome to plot.
#   chr_vec, start_vec, end_vec - Respectively the chr, start and end positions
#     to be used to delimitate the region to plot.
#   annot_gene - used to plot the gene panels. Dataset with the chr, start, end,
#     strand columns. Must also have a column gene_name. Columns exonStarts and
#     exonEnds are used to plot exons.
#   annot_cgi - used to plot the cpg island panels. Dataset with the chr,
#     start, end.
#   annot_repeats - used to plot the repeats panels. Dataset with the chr,
#     start, end.
#   regions_all_subset - used to plot the regions panels. Dataset with the
#     region name, positions, groups and status. One line per group/status.
plot_region_profile <- function(sample_meth_subset_with_pos,
                                control_meth_subset_with_pos,
                                chr_vec, start_vec, end_vec,
                                annot_gene, annot_cgi, annot_repeats,
                                regions_all_subset) {
  
  # ----
  # Define sub-functions to be used to plot each panel. ----
  # Functions to plot the methylation profile panel and its corresponding
  #   legend.
  panel_profile_base <- function(sample_meth_subset_with_pos,
                                 control_meth_subset_with_pos,
                                 start_vec, end_vec, color_vector) {
    # Prepare a base empty plot. ----
    plot(
      x = control_meth_subset_with_pos$start,
      y = control_meth_subset_with_pos$mean,
      xlim = c(start_vec, end_vec), ylim = c(0, 100),
      xlab = "", xaxt = "n",
      ylab = "Methylation Percentage (%)", type = "p",
      pch = NA
    )
    grid()
    
    
    # ----
    # Add polygons associated to control population paramaters (except the
    #   mean). ----
    polygon(
      x = c(control_meth_subset_with_pos$start,
            rev(control_meth_subset_with_pos$start)),
      y = c(control_meth_subset_with_pos$X99,
            rev(control_meth_subset_with_pos$X1)),
      col = rgb(0.3, 0.3, 0.3, 0.1),
      border = rgb(0.3, 0.3, 0.3, 0.25)
    )
    polygon(
      x = c(control_meth_subset_with_pos$start,
            rev(control_meth_subset_with_pos$start)),
      y = c(control_meth_subset_with_pos$X95,
            rev(control_meth_subset_with_pos$X5)),
      col = rgb(0.3, 0.3, 0.3, 0.1),
      border = rgb(0.3, 0.3, 0.3, 0.25)
    )
    polygon(
      x = c(control_meth_subset_with_pos$start,
            rev(control_meth_subset_with_pos$start)),
      y = c(control_meth_subset_with_pos$X75,
            rev(control_meth_subset_with_pos$X25)),
      col = rgb(0.3, 0.3, 0.3, 0.1),
      border = rgb(0.3, 0.3, 0.3, 0.25)
    )
    lines(
      x = control_meth_subset_with_pos$start,
      y = control_meth_subset_with_pos$max,
      col = "grey80", lty = "dashed"
    )
    lines(
      x = control_meth_subset_with_pos$start,
      y = control_meth_subset_with_pos$min,
      col = "grey80", lty = "dashed"
    )
    
    
    
    # Add lines and points for each sample provided. ----
    name_samples <-
      colnames(sample_meth_subset_with_pos)[4:ncol(sample_meth_subset_with_pos)]
    for (name_sample in name_samples) {
      lines(
        x = sample_meth_subset_with_pos$start,
        y = sample_meth_subset_with_pos[, name_sample],
        col = color_vector[name_sample]
      )
      points(
        x = sample_meth_subset_with_pos$start,
        y = sample_meth_subset_with_pos[, name_sample],
        pch = 21, bg = color_vector[name_sample], col = "black"
      )
    }
    
    # Add lines and points for the mean of the control population. ----
    lines(
      x = control_meth_subset_with_pos$start,
      y = control_meth_subset_with_pos$mean,
      col = "grey50"
    )
    points(
      x = control_meth_subset_with_pos$start,
      y = control_meth_subset_with_pos$mean,
      pch = 21, bg = "grey60", col = "black"
    )
  }
  panel_profile_legend <- function(sample_meth_subset_with_pos, color_vector) {
    # Empty plot for legend display. ----
    plot.new()
    
    name_samples <-
      colnames(sample_meth_subset_with_pos)[4:ncol(sample_meth_subset_with_pos)]
    
    # ----
    # Display one legend per type of representation (lines then boxes) to
    #   overlap them. ----
    legend("top",
           c(
             name_samples,
             "Controls mean",
             NA, NA, NA
           ),
           text.col = "transparent",
           lty = c(rep(x = 1, 1 + length(name_samples)), NA, NA, NA),
           col = c(color_vector[name_samples], "grey60", NA, NA, NA),
           pch = c(rep(x = 20, 1 + length(name_samples)), NA, NA, NA),
           cex = 1, xpd = TRUE,
           box.lwd = 0, box.col = "transparent", bg = "transparent"
    )
    legend("top",
           c(
             name_samples,
             "Controls mean",
             "25-75 percentiles",
             "5-95 percentiles",
             "1-99 percentiles"
           ),
           border = c(rep(x = NA, 1 + length(name_samples)),
                      "black", "black", "black"),
           fill = c(
             rep(x = NA, 1 + length(name_samples)),
             rgb(0.3, 0.3, 0.3, 0.4),
             rgb(0.3, 0.3, 0.3, 0.2),
             rgb(0.3, 0.3, 0.3, 0.1)
           ),
           cex = 1, xpd = TRUE, bty = "n"
    )
  }
  
  # Functions to plot the CGI annotation panel and its corresponding legend.
  panel_cgi_base <- function(annot_cgi, start_vec, end_vec) {
    
    # Base CGI display and adjust the box around the plot ----
    plot(
      x = c(start_vec, end_vec),
      y = c(0, 1),
      xaxt = "n", xlab = "",
      yaxt = "n", ylab = "", pch = "", bty = "n"
    )
    
    if (nrow(annot_cgi) > 0) {
      segments(
        x0 = annot_cgi$start,
        x1 = annot_cgi$end,
        y0 = 0.5,
        y1 = 0.5,
        lwd = 3, col = "green"
      )
      
      box(bty = "o", col = "black")
      box(bty = "]", col = "white")
      box(bty = "7", col = "black")
    }
  }
  panel_cgi_legend <- function() {
    plot.new()
    
    legend("left",
           "CpG islands",
           cex = 0.75, xpd = TRUE,
           box.lwd = 0, box.col = "transparent", bg = "transparent"
    )
  }
  
  # Functions to plot the repeats annotation panel and its corresponding legend.
  panel_repeats_base <- function(annot_repeats, start_vec, end_vec) {
    plot(
      x = c(start_vec, end_vec),
      y = c(0, 1),
      xaxt = "n", xlab = "",
      yaxt = "n", ylab = "", pch = "", bty = "u"
    )
    
    if (nrow(annot_repeats) > 0) {
      segments(
        x0 = annot_repeats$start,
        x1 = annot_repeats$end,
        y0 = 0.5,
        y1 = 0.5,
        lwd = 5, col = "black"
      )
    }
  }
  panel_repeats_legend <- function() {
    plot.new()
    
    legend("left",
           "Repeats",
           cex = 0.75, xpd = TRUE,
           box.lwd = 0, box.col = "transparent", bg = "transparent"
    )
  }
  
  # Functions to plot the regions annotation panel and its corresponding legend.
  panel_regions_base <- function(regions_all_subset, start_vec, end_vec) {
    plot(
      x = c(start_vec, end_vec),
      y = c(0, nrow(regions_all_subset) + 1),
      xaxt = "n", xlab = "",
      yaxt = "n", ylab = "Regions", pch = ""
    )
    
    segments(
      x0 = regions_all_subset$start,
      x1 = regions_all_subset$end,
      y0 = 1:nrow(regions_all_subset),
      y1 = 1:nrow(regions_all_subset),
      lwd = 3, col = "red"
    )
  }
  panel_regions_legend <- function(regions_all_subset) {
    plot(
      x = c(-0.01, 1),
      y = c(0, nrow(regions_all_subset) + 1),
      xaxt = "n", xlab = "",
      yaxt = "n", ylab = "", pch = NA, bty = "n"
    )
    
    text(
      x = rep(0, nrow(regions_all_subset)),
      y = 1:nrow(regions_all_subset),
      adj = c(0, 0.5),
      labels = regions_all_subset$name, cex = 0.75,
      col = "black"
    )
    
    text(
      x = rep(1, nrow(regions_all_subset)),
      y = 1:nrow(regions_all_subset),
      adj = c(1, 0.5),
      labels = regions_all_subset$group, cex = 0.75,
      col = "black"
    )
  }
  
  # Functions to plot the genes annotation panel and its corresponding legend.
  panel_genes_base <- function(annot_gene, chr_vec, start_vec, end_vec) {
    if (nrow(annot_gene) == 0) {
      plot(
        x = c(start_vec, end_vec),
        y = c(0, 1),
        xlab = chr_vec,
        yaxt = "n", ylab = "Genes", pch = ""
      )
    } else {
      plot(
        x = c(start_vec, end_vec),
        y = c(0, nrow(annot_gene) + 1),
        xlab = chr_vec,
        yaxt = "n", ylab = "Genes", pch = ""
      )
      
      annot_gene <- annot_gene[order(annot_gene$start, annot_gene$end), ]
      rownames(annot_gene) <- NULL
      
      new_end <- annot_gene$start[annot_gene$strand == "-"]
      new_start <- annot_gene$end[annot_gene$strand == "-"]
      annot_gene$start[annot_gene$strand == "-"] <- new_start
      annot_gene$end[annot_gene$strand == "-"] <- new_end
      
      arrows(
        x0 = annot_gene$start,
        x1 = annot_gene$end,
        y0 = as.numeric(rownames(annot_gene)),
        y1 = as.numeric(rownames(annot_gene)),
        lwd = 3, angle = 70, length = 0.04,
        col = "darkblue"
      )
      
      annot_gene$start[annot_gene$strand == "-"] <- new_end
      annot_gene$end[annot_gene$strand == "-"] <- new_start
    }
  }
  panel_genes_legend <- function(annot_gene) {
    if (nrow(annot_gene) == 0) {
      plot.new()
    } else {
      plot(
        x = c(-0.01, 1),
        y = c(0, nrow(annot_gene) + 1),
        xaxt = "n", xlab = "",
        yaxt = "n", ylab = "", pch = NA, bty = "n"
      )
      
      annot_gene <- annot_gene[order(annot_gene$start, annot_gene$end), ]
      rownames(annot_gene) <- NULL
      
      text(
        x = rep(0, nrow(annot_gene)),
        y = as.numeric(rownames(annot_gene)),
        adj = c(0, 0.5),
        labels = annot_gene$gene_name, cex = 0.75,
        col = "black"
      )
    }
  }
  
  
  
  # Save old plot parameters. ----
  old_mar <- par()$mar
  
  
  # Prepare the named color vector associated to the samples provided. ----
  color_vector <- brewer.pal(n = max(3, ncol(sample_meth_subset_with_pos) - 3),
                             name = "Set2")[1:(ncol(sample_meth_subset_with_pos) - 3)]
  names(color_vector) <-
    colnames(sample_meth_subset_with_pos)[4:ncol(sample_meth_subset_with_pos)]
  
  
  # Create a layout for 10 panels (5 plots + 5 empty plots for legends) ----
  layout(matrix(1:10, ncol = 2, nrow = 5, byrow = TRUE),
         widths = c(0.8, 0.2), heights = c(0.55, 0.025, 0.025, 0.1, 0.25)
  )
  
  
  par(mar = c(0, 4.1, 0.6, 0.6)) # Adjust margins.
  # Plot 1st track and legend ----
  panel_profile_base(sample_meth_subset_with_pos,
                     control_meth_subset_with_pos,
                     start_vec, end_vec, color_vector)
  par(mar = c(0, 0, 0, 0)) # Adjust margins.
  panel_profile_legend(sample_meth_subset_with_pos, color_vector)
  
  
  par(mar = c(0, 4.1, 0, 0.6)) # Adjust margins.
  # Plot 2nd track and legend ----
  panel_cgi_base(annot_cgi, start_vec, end_vec)
  par(mar = c(0, 0, 0, 0)) # Adjust margins.
  panel_cgi_legend()
  
  
  par(mar = c(0, 4.1, 0, 0.6)) # Adjust margins.
  # Plot 3rd track and legend ----
  panel_repeats_base(annot_repeats, start_vec, end_vec)
  par(mar = c(0, 0, 0, 0)) # Adjust margins.
  panel_repeats_legend()
  
  
  par(mar = c(0, 4.1, 0, 0.6)) # Adjust margins.
  # Plot 4th track and legend ----
  panel_regions_base(regions_all_subset, start_vec, end_vec)
  par(mar = c(0, 0, 0, 0)) # Adjust margins.
  panel_regions_legend(regions_all_subset)
  
  
  par(mar = c(4, 4.1, 0, 0.6)) # Adjust margins.
  # Plot 5th track ----
  panel_genes_base(annot_gene, chr_vec, start_vec, end_vec)
  par(mar = c(4, 0, 0, 0)) # Adjust margins.
  panel_genes_legend(annot_gene)
  
  
  # Re-initialize layout and plot parameters. ----
  layout(matrix(1))
  par(mar = old_mar)
}

