



############################################
# Detection P-value - QC
############################################

#Compute and format qc data - detection p-value
get_qc_data_detectpval <- function(RGSet){
  d_pval <- detectionP(RGSet)
  d_pval_above_limit <- d_pval > 0.01
  frac_failed_pos_per_sample <- data.frame(sampleNames=colnames(d_pval_above_limit),
                                           FailedPositionsPct=100*colMeans(d_pval_above_limit), # Fraction of failed positions per sample
                                           row.names = NULL)
  
  mean_pval_per_failed_pos <- rowMeans(d_pval)[rowSums(d_pval_above_limit) > 0]
  mean_pval_per_failed_pos <- data.frame(MeanDetectionPvalue = mean_pval_per_failed_pos, # Mean p-value for each failed position
                                         NumberOfSamplesWithFailure = rowSums(d_pval_above_limit)[rowSums(d_pval_above_limit) > 0]) # Number of samples by failed (>1 fail) positions
  #Generating table 1
  tbl_detectPVal1 <-
    frac_failed_pos_per_sample %>%
    kable("html", ) %>%
    kable_styling(font_size = 11, full_width = FALSE, position = "float_right")
  
  #color per sample
  colorV <- ifelse(frac_failed_pos_per_sample$FailedPositionsPct > 1, "#FFA5A5", "white")
  for (i in 1:nrow(frac_failed_pos_per_sample)) {
    tbl_detectPVal1 <- row_spec(tbl_detectPVal1, i, background = colorV[i])
  }

  #Generating table 2  
  tbl_detectPVal2 <-
    mean_pval_per_failed_pos %>%
    kable("html", ) %>%
    kable_styling(font_size = 11, full_width = TRUE, position = "left") %>%
    scroll_box(width = "100%", height = "300px")
  
  return(list("tbl_detectPVal1"=tbl_detectPVal1, 
              "tbl_detectPVal2"=tbl_detectPVal2,
              "frac_failed_pos_per_sample"=frac_failed_pos_per_sample, 
              "mean_pval_per_failed_pos"=mean_pval_per_failed_pos, 
              "d_pval"=d_pval))
}



#Detection P-value - get qc result
get_qc_result_detectpval <- function(frac_failed_pos_per_sample, 
                                     mean_pval_per_failed_pos, 
                                     d_pval, 
                                     iconGOOD, iconWARNING, iconBAD,
                                     remove_probes_highpval,
                                     remove_samples_highpval){
  
  #QC result
  if( all(frac_failed_pos_per_sample$FailedPositionsPct <= 1) ) { 
    if( 100*nrow(mean_pval_per_failed_pos)/nrow(d_pval) < 1 ) { 
      #green
      DetectPvalQCresult <- 
        list(iconGOOD,
             "Good: Most probes have a good detection p-value.",
             paste0(round(100*nrow(mean_pval_per_failed_pos)/nrow(d_pval), 2), 
                    " % of low-quality probes ",
                    ifelse(remove_probes_highpval, "were", "should be"),
                    " removed.") )
    } else {
      #orange
      DetectPvalQCresult <- 
        list(iconWARNING,
             "Warning: A lot of probes with a bad detection p-value had to be removed. You should re-check that samples provided have not too much bad probes (limit: 1% of bad probes).",
             paste0(round(100*nrow(mean_pval_per_failed_pos)/nrow(d_pval), 2), 
                    " % of low-quality probes ",
                    ifelse(remove_probes_highpval, "were", "should be"),
                    " removed.") )
    }
  } else {
    #red
    DetectPvalQCresult <- 
      list(iconBAD,
           paste0("Bad: at least one sample (displayed in red in the tab 'for each ",
                  "sample') ",
                  ifelse(remove_samples_highpval, "had to be", "should be"),
           " removed because of a high amount of probes with a bad detection p-value.",
           " \n\n Removing all bad probes in this/these sample(s) might not be enough",
           " to analyze it/them without any error."),
           paste0(sum(frac_failed_pos_per_sample$FailedPositionsPct > 1), " sample(s) ",
                  ifelse(remove_samples_highpval, "was/were", "should be"),
                  " removed",
                  " and ", 
                  round(100*nrow(mean_pval_per_failed_pos)/nrow(d_pval), 2), 
                  " % of low-quality probes ",
                  ifelse(remove_probes_highpval, "were", "should be"),
                  " removed.") )
    
  }
  
  return(DetectPvalQCresult)
}



############################################
# MethUnmeth median intensity - QC
############################################

#Compute and format qc data - MethUnmeth median intensity
get_qc_data_methunmeth <- function(RGSet, badSampleCutoff){
  badSampleCutoff <- as.numeric(params$badMethUnmethSampleCutoff)
  
  #retrieve meth/unmeth dataframe
  methUnmeth_table <- getQC(preprocessRaw(RGSet))
  methUnmeth_table <- as.data.frame(methUnmeth_table)
  
  #retrieve bad samples vector
  whichIsBad <- 
    ifelse((methUnmeth_table$mMed + methUnmeth_table$uMed)/2 < badSampleCutoff, 
           "bad", "good")
  
  #Generating table
  tblPlot_methUnmeth <- methUnmeth_table
  tblPlot_methUnmeth$whichIsBad <- whichIsBad
  
  colnames(methUnmeth_table) <- c("Meth", "Unmeth")
  tbl_methUnmeth <-
    methUnmeth_table %>%
    kable("html" ) %>%
    kable_styling(font_size = 11, full_width = FALSE, 
                  position = "float_right") %>%
    add_header_above(c(" ", "Median intensity (log2)" = 2)) 
  
  #color per sample
  colorV <- ifelse(whichIsBad == "bad", "#FFA5A5", "white")
  for (i in 1:nrow(methUnmeth_table)) {
    tbl_methUnmeth <- row_spec(tbl_methUnmeth, i, background = colorV[i])
  }
  
  return(list(
    "tbl_methUnmeth" = tbl_methUnmeth,
    "tblPlot_methUnmeth" = tblPlot_methUnmeth
  ))
}

#MethUnmeth median intensity - get qc result
get_qc_result_methunmeth <- function(tblPlot_methUnmeth,
                                     iconGOOD, iconBAD,
                                     removeSamplesWithBadMethUnmeth){
  
  #QC result
  if( all(tblPlot_methUnmeth$whichIsBad == "good") ) { 
    #green
    MethUnmethQCresult <- list(iconGOOD,
                               "Good: Each sample has a good Meth/Unmeth median intensity ratio.",
                               "No sample(s) were removed here." )
  } else {
    #red
    MethUnmethQCresult <- list(iconBAD,
                               paste0("Bad: at least one sample (displayed in red in the graph and table) ", ifelse(removeSamplesWithBadMethUnmeth, "had to be", "should be"), " removed because of a low median intensity of Meth or Unmeth signals. Bad sample(s) here should not be used: a problem could have occured with the data generation."),
                               paste0(sum(tblPlot_methUnmeth$whichIsBad == "bad"), " sample(s) of low-quality ",
                                      ifelse(removeSamplesWithBadMethUnmeth, "was/were", "should be"), 
                                      " removed.") )
  }
  
  return(MethUnmethQCresult) 
}





############################################
# Control probes - QC
############################################

# Compute and format qc data - Control probes
#   The following Code is an adapted version of qcReport function 
#   from minfi package.
get_qc_data_ctrlprobes <- 
  function(RGSet, intensityThreshold = c(5, 17),
           controls = c("BISULFITE CONVERSION I", "BISULFITE CONVERSION II", 
                        "EXTENSION", "HYBRIDIZATION", "NON-POLYMORPHIC", 
                        "SPECIFICITY I", "SPECIFICITY II", "TARGET REMOVAL")){
    
    #Initialize lists
    controlsList <- vector(mode = "list", length(controls))
    names(controlsList) <- controls
    wrongProbeslist <- vector(mode = "list", length(controls))
    names(wrongProbeslist) <- controls
    
    #Retrieve red/green signals
    r <- getRed(RGSet)
    g <- getGreen(RGSet)
    
    
    #Loop: for each type of control probes, get and condense the values in a list
    for (controlType in controls) {
      
      #Retrieve data per control type
      ctrlAddress <- getControlAddress(RGSet, controlType = controlType)
      ctlG <- melt(g[ctrlAddress, , drop = FALSE], varnames = c("address", "sample"))
      ctlR <- melt(r[ctrlAddress, , drop = FALSE], varnames = c("address", "sample"))
      
      #Format data
      ctl <- rbind(cbind(channel = "green", ctlG), cbind(channel = "red", ctlR))
      ctl$value <- log2(ctl$value)
      ctl <- ctl[order(ctl$sample),]
      
      #Which probes are out of bounds?
      probesOutofLimits <- 
        (ctl$value < intensityThreshold[1]) | 
        (ctl$value > intensityThreshold[2])
      
      #Fill wrongProbeslist list: Number of failed probes + samples associated
      wrongProbeslist[[controlType]] <- 
        list(sum(probesOutofLimits), unique(ctl[probesOutofLimits, "sample"]))
      
      #Fill controlsList list: control probes red/green signals
      controlsList[[controlType]] <- ctl
    }
    
    #compute number of bad probes by control type
    outlier_ctprb_nb_per_type <- as.matrix(unlist(sapply(wrongProbeslist, `[`, 1)))
    outlier_ctprb_nb_per_type[
      is.na(outlier_ctprb_nb_per_type)] <- 0 #number of bad probes by control
    colnames(outlier_ctprb_nb_per_type) <- 
      "Number of \"out-of-range\" control probes"
    
    #Generating table 1
    tbl_ctrlProbes1 <-
      outlier_ctprb_nb_per_type %>%
      kable("html") %>%
      kable_styling(font_size = 11, full_width = FALSE, position = "float_left")
    
    #retrieve data for all samples and control types
    data_all_ctprb <- do.call("rbind", controlsList)
    data_all_ctprb$probesOutofLimits <- 
      (data_all_ctprb$value < intensityThreshold[1]) | 
      (data_all_ctprb$value > intensityThreshold[2])
    
    #compute number of bad probes by sample
    if(any(data_all_ctprb$probesOutofLimits)){
      outlier_ctprb_nb_per_sample <- 
        data.frame(table(data_all_ctprb[,c(3,5)])[, 2, drop=FALSE])[,-2]
    } else {
      outlier_ctprb_nb_per_sample <- 
        data.frame(table(data_all_ctprb[,c(3,5)]))[,-2]
      outlier_ctprb_nb_per_sample[,2] <- 0
    }
    colnames(outlier_ctprb_nb_per_sample) <- 
      c("sample", "Number of \"out-of-range\" control probes")
    
    #Generating table 2
    tbl_ctrlProbes2 <-
      outlier_ctprb_nb_per_sample %>%
      kable("html") %>%
      kable_styling(font_size = 11, full_width = FALSE, position = "right")
    
    #color per sample
    colorV <- ifelse(outlier_ctprb_nb_per_sample[,2] > 0, "#FFA5A5", "white")
    for (i in 1:nrow(outlier_ctprb_nb_per_sample)) {
      tbl_ctrlProbes2 <- row_spec(tbl_ctrlProbes2, i, background = colorV[i])
    }
    
    return(list(
      "controlsList"=controlsList,
      "tbl_ctrlProbes1"=tbl_ctrlProbes1,
      "tbl_ctrlProbes2"=tbl_ctrlProbes2,
      "nb_bad_probes_per_sample" = setNames(outlier_ctprb_nb_per_sample[,2],
                                            outlier_ctprb_nb_per_sample[,1])
    ))
  }

#Control probes - get qc result
get_qc_result_ctrlprobes <- function(nb_bad_probes_per_sample,
                                     iconGOOD, iconWARNING){
  #QC result
  if( all(nb_bad_probes_per_sample == 0) ) { 
    #green
    ctrlProbesQCresult <- list(iconGOOD,
                               "Good: Control probes from each sample are within expected range (between 5 and 17).",
                               "No sample(s) were removed here." )
  } else {
    #orange
    ctrlProbesQCresult <- list(iconWARNING,
                               "Warning: at least one sample (displayed in red in the table) has a control probe out of the expected range (between 5 and 17). Bad sample(s) here should not be used: a problem could have occured with the data generation.",
                               paste0(sum(nb_bad_probes_per_sample > 0), " sample(s) of low-quality has not been removed but should be avoided for the rest of the analysis.") )
  }
  return(ctrlProbesQCresult) 
}


# Stripplot for Control probes.
#   The following Code is an adapted version of qcReport 
#   function from minfi package.
controlProbesGreenRedPlot <- function(ctl, controlType){
  ctl <- ctl[order(ctl$sample),]
  ctl$sample <- as.factor(ctl$sample)
  side_margin_length <- 
    max(nchar(as.character(levels(ctl$sample))))*0.425
  par(oma = c(3, 2, 2, 2))
  layout(matrix(1:2, ncol=2))
  par(mar=c(1.1,side_margin_length,1.1,0))
  plot(x=ctl$value[ctl$channel=="green"], 
       y=ctl$sample[ctl$channel=="green"],
       pch=20, col="#64B400", xlab = "", ylab = "", yaxt='n')
  title(main = "Green", col.main="#64B400",
        outer = FALSE, line = 0.4)
  axis(side = 2, at = 1:length(unique(ctl$sample)), 
       labels = rep("",length(unique(ctl$sample))) )
  text(x = par("usr")[1] - (par("usr")[2] - par("usr")[1])*0.1,
       y = 1:length(unique(ctl$sample)),
       labels = unique(ctl$sample),
       xpd = NA, adj = c(1,0.5),
       ## Rotate the labels by 25 degrees.
       srt = 25,
       cex = 0.75)
  
  par(mar=c(1.1,0,1.1,side_margin_length))
  plot(x=ctl$value[ctl$channel=="red"], 
       y=ctl$sample[ctl$channel=="red"],
       pch=20, col="#C84600", xlab = "", ylab = "", yaxt='n')
  title(main = "Red", col.main="#C84600",
        outer = FALSE, line = 0.4)
  title(main = paste0("Control: ", controlType), 
        xlab = "Log2 Intensity",
        ylab = "sample",
        outer = TRUE, line = 1.2)
  layout(matrix(1))
  par(mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
}









############################################
# Bval density - QC
############################################

# Compute and format qc data - Beta-value density  -------------------------------------
get_qc_data_bvaldensity <- function(RGSet, control_path, bval_threshold){
  #Get reference distribution to compare it to the sample and test it
  refDistribution <- read.csv(control_path, 
                              header=TRUE, row.names = 1)
  refDistribution <- density(refDistribution$mean)
  
  #Get Beta-values density from sample(s)
  betaDat <- getBeta(RGSet)
  densityList <- lapply( 1:ncol(betaDat), function(x){
    density(betaDat[, x], na.rm=TRUE)
  } )
  
  
  #testing sum of absolute difference between density curves
  aValue <- sapply(densityList, function(x){ sum(abs(x$y-refDistribution$y)) } )
  
  #Prepare table
  bvalDensityDiff_table <- data.frame(
    "sample" = colnames(betaDat),
    "betaValueAbsoluteDifference" = aValue
  )
  
  #color sample names ----
  colorV <- colorRampPalette(c(rgb(0,0,0.5),
                               rgb(0,0.5,0.5)))(length(densityList))
  bvalDensityDiff_table$sample <- 
    sapply(1:nrow(bvalDensityDiff_table), function(x){
      return(
        text_spec(bvalDensityDiff_table$sample[x], 
                  background = colorV[x], color="white")
      )
    })
  
  #Generate table to print
  tbl_bvalDensityDiff <-
    bvalDensityDiff_table %>%
    kable("html", escape = F) %>%
    kable_styling(font_size = 11, full_width = FALSE, position = "float_right")
  
  #color per sample
  colorV <- ifelse(bvalDensityDiff_table[,2] > bval_threshold, 
                   "#FFB900", "white")
  for (i in 1:nrow(bvalDensityDiff_table)) {
    tbl_bvalDensityDiff <- row_spec(tbl_bvalDensityDiff, i, 
                                    background = colorV[i])
  }
  
  return(list(
    "density_samples"=densityList,
    "density_ref"=refDistribution,
    "bvalDensityDiff_table"=bvalDensityDiff_table,
    "tbl_bvalDensityDiff"=tbl_bvalDensityDiff,
    "bval_abs_diff_per_sample" = setNames(bvalDensityDiff_table[,2],
                                          colnames(betaDat))
  ))
}


# Beta-value density - get qc result -------------------------------------
get_qc_result_bvaldensity <- function(bval_abs_diff_per_sample, 
                                      bval_threshold, 
                                      iconGOOD, iconWARNING){
  
  #QC result
  if( all(bval_abs_diff_per_sample < bval_threshold) ) { 
    #green
    bvalDensityDiffQCresult <- 
      list(iconGOOD,
           "Good: Each sample seems to follow the distribution of the control population provided.",
           "No sample(s) were removed here." )
  } else {
    #orange
    bvalDensityDiffQCresult <- 
      list(iconWARNING,
           "Warning: at least one sample (displayed in orange in the table) has a beta-value distribution that seems different from the control population provided. This can result in a lot of differentially methylated CpGs or regions for this/these sample(s). You should check manually that the beta-value ditribution from these samples is not abberant compared to the control population.",
           paste0(sum(bval_abs_diff_per_sample > bval_threshold), " sample(s) have a different global distribution of beta-values and should be checked to see if it/they can be compared to this control population.") )
  }
  
  return(bvalDensityDiffQCresult)
}


#Beta value density plot function
bvalDensityPlot <- function(densityList, refDistribution){
  maxYlim <- max( sapply(densityList, function(x){ max(x$y) }),
                  refDistribution$y)* 1.05
  
  colorV <- colorRampPalette(c(rgb(0,0,1),
                               rgb(0,1,1)))(length(densityList))
  
  plot(x = 1, y = 1, xlim = c(0,1), ylim = c(0, maxYlim), 
       pch=NA, xlab = "Beta-value", ylab = "Density")
  abline(h=0, col = "grey60")
  for (d in 1:length(densityList)) {
    lines(densityList[[d]], col=colorV[d])
  }
  lines(x = refDistribution$x,
        y = refDistribution$y, 
        col="red", lty=2, lwd=2)
  
}







