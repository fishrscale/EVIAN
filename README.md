# DNAm_analysis toolbox

## Introduction

DNAm_analysis is a suite of tools to perform a differential analysis of the distribution of CpG methylation (via Illumina Methyl Array technology).  
This toolbox currently include:  
* **DNAm_report**: a script to generate a HTML report about the distribution of CpG methylation in a few samples against a control population. This analysis is mainly focused on a set of known regions provided by the user.    
* **DNAm_analysis_shiny.R**: and a shiny interface to launch the previous scripts without using directly command-lines.  

Each script can be directly launched in command-lines: look at the *ReadMe.md* from the corresponding directories for more information.  

## Installation

### Installation via conda

* Install conda via Miniconda (https://docs.conda.io/en/latest/miniconda.html) or Anaconda.

* Open your terminal window (Mac, Linux) or anaconda prompt (Windows) and launch the following command to install a new conda environment for the DNAm_analysis toolbox:

```
conda create -n DNAm_analysis -c conda-forge -c bioconda r r-essentials r-rmarkdown pandoc=2.7.3 r-kableextra bioconductor-rtracklayer r-rcolorbrewer r-reshape2 r-shiny r-shinyfiles r-shinywidgets r-waiter
```

* Once the installation is complete, activate the environment previously generated to load the required dependencies before using any script from the toolbox:

```
conda activate DNAm_analysis
```

### Installation via R/RStudio

@@@To complete

## Usage

The usage of each tool is detailed in the ReadMe.md of each corresponding directory.

Here is detailed the steps to use the shiny interface *DNAm_analysis_shiny.R*.

* Once you loaded the required dependencies to use the scripts, launch the *DNAm_analysis_shiny.R* script to open the interface.

```
Rscript ./path/to/DNAm_analysis_shiny.R
```

* Each tab on the left corresponds to one of the available tool: select the one that you want to use.

@@@To complete with a screenshot modified

* Follow the required instructions, select the files and conditions you want to use for the analysis.

@@@To complete with a screenshot modified

~~* Click on the Bookmark button if you want to save all your current inputs for a later analysis.~~

@@@To complete with a screenshot modified

* Once you finished completing the conditions you wanted to use, click on the "Generate report" or "Launch analysis" button at the bottom of the page.

@@@To complete with a screenshot modified

* A command-line will be generated and launched: the progress can be followed on the terminal window.


## Citation/Work/Contact

@@@To complete

## Licence

@@@To complete

