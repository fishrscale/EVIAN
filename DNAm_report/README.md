# DNAm_analysis report

## Introduction

DNAm_analysis report generates a HTML report about the distribution of CpG methylation (via Illumina Methyl Array technology) in a few samples against a control population. This analysis is mainly focused on a set of known regions provided by the user.

## Installation

Follow the instructions on the base *ReadMe.md* to install the DNAm_analysis toolbox and use this script.

## Usage

To test this script, example files are provided in the *DATA* and *Annotation_files* directories.

Once you loaded the required dependencies to use this tool, you can either:
* launch the *DNAm_report_script.R* script that will generate the report from the Rmd file:

```
cd ./path/to/DNAm_analysis
Rscript ./DNAm_report/DNAm_report_script.R -sp "../DATA/Samples_small.csv.gz" -sitc "ZH1609094r1,ZH1712576ms2"  -cpath "../DATA/Control_population.csv.gz" -cpos "../Annotation_files/CpGs_position_v2.csv" -rp "../Annotation_files/regionsList_v4.tab,../Annotation_files/regionsList_v5.tab" -rgtc "PHP,BWS,SRS" -rs "High" -gac "SRS" -etg "TRUE" -dir "../ExampleOfReport/"
```

* or launch the *DNAm_visualization_report.Rmd* via the `rmarkdown::render()` command via R/RStudio:

```
setwd("./path/to/DNAm_analysis")
library(rmarkdown)
render(input = "./DNAm_report/DNAm_visualization_report.Rmd",
       params = list(
           samples_path = "../DATA/Samples_small.csv.gz",
           samples_id_to_check = "ZH1609094r1,ZH1712576ms2",
           control_path = "../DATA/Control_population.csv.gz",
           cpg_positions = "../Annotation_files/CpGs_position_v2.csv",
           regions_path = "../Annotation_files/regionsList_v4.tab,../Annotation_files/regionsList_v5.tab",
           regions_group_to_check = "PHP,BWS,SRS",
           regions_status = "High",
           group_as_ctrls = "SRS",
           export_tables_graphs = "TRUE",
           outputDir_path = "../ExampleOfReport/"
           ),
       output_file = "../ExampleOfReport/nameOfReport.html")
       )
```

**Warning**: the *annot_rds_files* directory must remain in the same directory than the *DNAm_visualization_report.Rmd* script.

**Warning**: the *regionsList_v5.tab* file is an incorrect file with false group connections: its only purpose is to show what combinations of groups looks like on the report.

## Arguments description

| Shell_____ | R | Description  |
| ------------- |-------------| -----|
| `-sp` | `samples_path` | Path(s) of the (beta-value) sample files to be analyzed: one row per probe / one column per sample. |
| `-sitc` | `samples_id_to_check` | Vector of samples to be kept from the sample files. If empty, no sample will be removed. |
| `-cpath` | `control_path` | Path of the stats summary (beta-value) control population file to serve as control: one row per probe. The following columns are required: mean, min, max, X1, X5, X25, X75, X95, X99 (X1: percentile 1). |
| `-cpos` | `cpg_positions` | Path of the cpg annotation file giving the coordinates of each probe: one row per probe / rownames: probeID / columns: chr, start, strand. |
| `-rp` | `regions_path` | Path(s) to a tab file giving the regions (with its characteristics) to be plotted or analyzed. The following columns are required: chr, start, end, strand, name (region name), group (e.g. syndrom1), status (e.g. highConfidence). |
| `-rgtc` | `regions_group_to_check` | Vector of group of regions to be analyzed from the region files. If empty, all regions will be analyzed. |
| `-rs` | `regions_status` | Vector of status of regions to be kept for the region files. If empty, no region is removed based on its status. |
| `-gac` | `group_as_ctrls` | Vector of group of regions to be considered as control regions. If empty, no region will be considered as control regions. |
| `-etg` | `export_tables_graphs` | Should the graphs and tables be also exported as pdf and csv files? Possible values: `TRUE` or `FALSE`. |
| `-dir` | `outputDir_path` | Path of the directory where to output the html report and potential additional files. |

The name of report is automatic and based on samples provided if not launching the Rmd via render.

A maximum of 10 samples can be analyzed with this report and each sample id must be unique.

## Citation/Work/Contact

@@@To complete

## Licence

@@@To complete

