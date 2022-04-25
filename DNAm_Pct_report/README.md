# DNA methylation Percentage (DNAm_Pct) distribution report

## Introduction

DNAm_Pct report generates a HTML report about the distribution of CpG methylation (via Illumina Methyl Array technology) in a few samples against a control population. This analysis is mainly focused on a set of known regions provided by the user.

## Installation

Follow the instructions on the base *ReadMe.md* to install the EVIAN toolbox and use this script.

## Usage

To test this script, example files are provided in the *test_data* directory.

Once you loaded the required dependencies to use this tool, you can either:
* launch the *DNAm_Pct.R* script that will generate the report from the Rmd file:

```
cd ./path/to/EVIAN
Rscript ./DNAm_Pct_report/DNAm_Pct.R -sp "./test_data/Samples_small.csv.gz" -sitc "ZH1609094r1,ZH1712576ms2"  -cpath "./test_data/Control_population.csv.gz" -rp "./test_data/regionsList_v1-2022_03_25.tsv,./test_data/fakeRegionsListForTests.tsv" -rgtc "PHP,BWS,SRS" -rs "High,Low,l" -gac "SRS" -etg "TRUE" -dir "./ExampleOfReport/"
```

* or launch the *DNAm_Pct_report.Rmd* via the `rmarkdown::render()` command via R/RStudio:

```
setwd("./path/to/EVIAN/DNAm_Pct_report")
library(rmarkdown)
#input paths must not be relative if the current directory is not the DNAm_Pct_report directory.
render(input = "DNAm_Pct_report.Rmd",
       params = list(
           samples_path = "../test_data/Samples_small.csv.gz",
           samples_id_to_check = "ZH1609094r1,ZH1712576ms2",
           control_path = "../test_data/Control_population.csv.gz",
           regions_path = "../test_data/regionsList_v1-2022_03_25.tsv,../test_data/fakeRegionsListForTests.tsv",
           regions_group_to_check = "PHP,BWS,SRS",
           regions_status = "High,Low,l",
           group_as_ctrls = "SRS",
           export_tables_graphs = "TRUE",
           outputDir_path = "../ExampleOfReport/"
           ),
       output_file = "../ExampleOfReport/nameOfReport.html"
       )
       
```

**Warning**: the *annot_rds_files* directory and the func_*.R files must remain in the same directory than the *DNAm_Pct_report.Rmd* script.

**Warning**: the *fakeRegionsListForTests.tsv* file is an incorrect file with false group connections: its only purpose is to show what combinations of groups looks like on the report.

## Arguments description

| arg small | arg long | Description  |
| ------------- |-------------| -----|
| `-s` | `--samples_path` | Path(s) of the (beta-value) sample files to be analyzed: one row per probe / one column per sample. |
| `-i` | `--samples_id_to_check` | Vector of samples to be kept from the sample files. If empty, no sample will be removed. |
| `-c` | `--control_path` | Path of the stats summary (beta-value) control population file to serve as control: one row per probe. The following columns are required: mean, min, max, X1, X5, X25, X75, X95, X99 (X1: percentile 1). |
| `-r` | `--regions_path` | Path(s) to a tab file giving the regions (with its characteristics) to be plotted or analyzed. The following columns are required: chr, start, end, strand, name (region name), group (e.g. syndrom1), status (e.g. highConfidence). |
| `-g` | `--regions_group_to_check` | Vector of group of regions to be analyzed from the region files. If empty, all regions will be analyzed. |
| `-t` | `--regions_status` | Vector of status of regions to be kept for the region files. If empty, no region is removed based on its status. |
| `-a` | `--group_as_ctrls` | Vector of group of regions to be considered as control regions. If empty, no region will be considered as control regions. |
| `-e` | `--export_tables_graphs` | Should the graphs and tables be also exported as pdf and csv files? Possible values: `TRUE` or `FALSE`. |
| `-d` | `--outputDir_path` | Path of the directory where to output the html report and potential additional files. |
| `-m` | `--cpg_positions` | Path of the cpg annotation RDS file giving the coordinates of each probe: one row per probe / rownames: probeID / columns: chr, start, strand. |
| `-n` | `--annot_cgi` | Path of the cpg island annotation RDS file giving the coordinates of each cpg island. |
| `-o` | `--annot_gene` | Path of the gene annotation RDS file giving the coordinates of each gene. |
| `-p` | `--annot_repeats` | Path of the repeats annotation file giving the coordinates of each repeat. |

The name of report is automatically based on the samples provided unless the report is generated directly via render with the Rmd file.

A maximum of 10 samples can be analyzed with this report and each sample id must be unique.

## Citation/Work/Contact

@@@To complete

## Licence

@@@To complete

