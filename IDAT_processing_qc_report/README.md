# IDAT processing and Quality Check (QC) report

## Introduction

IDAT_process_and_QC.R generates a HTML QC report about the quality of .idat files provided through the input directory (Illumina Methyl Array technology) and process these .idat files to retrieve the Beta-values (≈ Methylation proportion) and M-values files. It also generates a second small HTML report to check that the beta-value density is improved after filtering/normalization.

## Installation

Follow the instructions on the base *ReadMe.md* to install the EVIAN toolbox and use this script.

## Usage

To test this script, example files are provided in the *test_data/idat_files* directory.

Once you loaded the required dependencies to use this tool, you can either:
* launch the *IDAT_process_and_QC.R* script (-h or --help to see each option available) that will process the input and generate the reports from the Rmd files:

```
cd ./path/to/EVIAN
Rscript ./IDAT_processing_qc_report/IDAT_process_and_QC.R --idat_dir=./test_data/idat_files/ --out_folder=./ExampleOfReport/ --output_basename=test_qc_outputs --norm_method=quantile --qc_report=TRUE --control_path=./test_data/Control_population.csv.gz"
```

**Warning**: the func_*.R and Rmd files must remain in the same directory than the *IDAT_process_and_QC.R* script.

## Arguments description

```
Usage: IDAT_processing_qc_report/IDAT_process_and_QC.R [options]


Options:
	-i CHARACTER, --idat_dir=CHARACTER
		idat directory path.

	-j CHARACTER, --control_path=CHARACTER
		control population summary file path (only required for the first bval density if running the qc report).  [default= NULL]

	-t CHARACTER, --truncate_samples_names=CHARACTER
		should the sample names be truncated? [default= TRUE]

	-q CHARACTER, --qc_report=CHARACTER
		generate qc report before processing idat files? [default= TRUE]

	-p CHARACTER, --remove_probes_highpval=CHARACTER
		remove probes with high detection p-value? [default= FALSE]

	-v CHARACTER, --remove_samples_highpval=CHARACTER
		remove samples with too much probes with high detection p-value? [default= TRUE]

	-r CHARACTER, --removeSamplesWithBadMethUnmeth=CHARACTER
		remove samples with an insufficient Meth/Unmeth signal? [default= TRUE]

	-c CHARACTER, --badMethUnmethSampleCutoff=CHARACTER
		insufficient Meth/Unmeth signal limit value to remove samples [default= 10.5]

	-n CHARACTER, --norm_method=CHARACTER
		normalization methods between samples provided. Only quantile and funnorm methods are available. No normalization is performed if only one sample is provided. [default= quantile]

	-s CHARACTER, --remove_snip=CHARACTER
		remove probes associated to snps? [default= TRUE]

	-m CHARACTER, --get_mval=CHARACTER
		also retrieve M-val files? [default= TRUE]

	-o CHARACTER, --out_folder=CHARACTER
		output directory path [default= .]

	-b CHARACTER, --output_basename=CHARACTER
		output base name [default= output]

	-h, --help
		Show this help message and exit
```

## Citation/Work/Contact

@@@To complete

## Licence

@@@To complete

