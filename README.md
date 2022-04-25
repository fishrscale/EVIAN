# EVIAN (EpiVariant and Imprinting ANalysis) toolbox

## Introduction

EVIAN is a suite of tools to perform a differential analysis of the distribution of CpG methylation (via Illumina Methyl Array technology).  
This toolbox currently include:  
* **DNAm_Pct_report**: a Rmd script to generate a HTML report about the distribution of CpG methylation in a few samples against a control population. This analysis is mainly focused on a set of known regions provided by the user.    
* **EVIAN_shiny.R**: and a shiny interface to launch the previous scripts without using directly command-lines.  

Each script can be directly launched in command-lines: look at the *ReadMe.md* from the corresponding directories for more information.  

## Installation

### Installation via conda

* Install conda via Miniconda (https://docs.conda.io/en/latest/miniconda.html) or Anaconda.

* Open your terminal window (Mac, Linux) or anaconda prompt (Windows) and launch the following command to install a new conda environment for the EVIAN toolbox:

```
conda create -n EVIAN_env -c conda-forge -c bioconda r r-essentials r-rmarkdown pandoc r-kableextra bioconductor-rtracklayer r-rcolorbrewer r-reshape2 r-shiny r-shinyfiles r-shinywidgets r-waiter r-optparse
```

* Once the installation is complete, activate the environment previously generated to load the required dependencies before using any script from the toolbox:

```
conda activate EVIAN_env
```

### Installation via R/RStudio

@@@To complete

## Usage

The usage of each tool is detailed in the ReadMe.md of each corresponding directory.

Here is detailed the steps to use the shiny interface *EVIAN_shiny.R*.

* Once you loaded the required dependencies to use the scripts, launch the *EVIAN_shiny.R* script to open the interface.

```
Rscript ./path/to/EVIAN_shiny.R
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

## Regions dataset description

Regions to be imported and analyzed must be saved in a tsv (tabulation-separated values) file.  
This file must have the 7 following columns (using this following order): 
| column name | description | example | 
| --- | --- |  --- | 
| chr | chromosome | chr1 |
| start | start position | 9200 |
| end | end position | 10200 |
| strand | strand | +, - or * |
| name | name of the region | DmrA-geneA, DmrB-1234, etc... |
| group | group(s) associated to this region | SyndromeA, ImprintedRegionGroup1, etc... |
| status | status associated to the corresponding group in this region | "High_Confidence", "Hypothetical", etc... |


It is possible to import different region files at once.  
It is also possible to have multiple groups associated to one region:
* 2 or more times the same region with one group per line with its associated status (ex: region 3)
* 2 or more groups (separated by a comma) in the "group" column of the same line (with associated status for each of the groups in the same order and separated by commas) (ex: region 2);
* or a combination of both previous possibilities (ex: region 3).

#### Regions dataset example
| chr | start | end  | strand | name | group | status |
| --- | --- | --- | --- | --- | --- | --- |
| chr1 | 1000 | 1500 | * | region1 | groupA | HighConf |
| chr3 | 500 | 750 | + | region2 | groupA,groupB | HighConf,HighConf |
| chr4 | 5000 | 10000 | * | region3 | groupB | LowConf |
| chr4 | 5000 | 10000 | * | region3 | groupC,groupD | HighConf,LowConf |


## Citation/Work/Contact

@@@To complete

## Licence

@@@To complete

