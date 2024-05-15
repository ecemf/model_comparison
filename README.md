# ECEMF working package 1 model comparison reports

This repository includes code necessary to create the model comparison reports developed in the working package 1 of the ECEMF project.
The list of supported reports are:
- `individualCharts`: a html report containing charts for the main variables reported.  
- `historicalCharts`: a html report containing a subset of `individualCharts`, containing charts for every reported variable with available historical data.
- `variablesSummations`: a series of excel sheets and pdf reports describing summation checks for the reported results.
- `modelChanges`: a html report for each model results evolution in time.
- `policyBrief`: a html report with charts created for the 2040 policy brief.
- `T1p3 scenarios`: a html report with charts in ECEMF format.

## Requirements

### Access to the ECEMF-internal Scenario Explorer

The ECEMF project uses the IIASA Scenario Explorer infrastructure to compile and
validate scenario data from the participating modeling teams.
The [ECEMF-internal Scenario Explorer](https://data.ece.iiasa.ac.at/ecemf-internal)
is accessible only for consortium members. Please reach out to the
[IIASA Scenario Services team](https://software.ece.iiasa.ac.at) to gain access.

## Installation - Option 1

Option 1)
You can use [Anaconda](https://www.anaconda.com/products/individual)
or [Miniconda](https://docs.conda.io/en/latest/miniconda.html)
to setup up a combined R and Python environment with all the dependencies needed
to run the script:

    conda env create -f environment.yaml

Then, activate the conda environment:

    conda activate model_comparison

Use Rscript command to run the R Markdown document creation:

    Rscript main.R

## Installation - Option 2 - if you already have an R environment

 - Make sure you have phyton and the `pyam` dependency installed.
 - Make sure you have R and the required libraries installed in your system. All packages can be installed via `install.packages`

```R
pkgs <- c(
	"reticulate",
	"quitte",
	"openxlsx",
	"tidyr",
	"dplyr",
	"piamInterfaces", 
	"mip",
	"svglite",
	"ggplot2",
	"grid",
	"gridExtra",
	"gtable",
	"ggplotify",
	"ggpattern",
	"ggdark",
	"jsonlite",
	"rmarkdown",
	"graphics",
	"quitte",
	"fs",
	"RColorBrewer",
	"knitr",
	"kableExtra"
	)	  	  
install.packages(pkgs)
```

## Running

- Open the file `ECEMF_reports.R` and change the execute options at the top of the file to choose the actions you want to enable during the run.

1. Download new data (`download = TRUE`)
	Downloads data directly from the iiasa database.
	If you don't have your credentials already stored see the section below with details about how to set your credentials to have access to the database.
```python
	import pyam
	pyam.iiasa.set_config("login"", "password")`
```
	
2. Execute specific report:
	set a specific report to be created by setting it to true:
```R	
  report = list(
    individualCharts =  TRUE, 
    historicalCharts =  TRUE,
    variablesSummations =  TRUE,
    modelChanges = TRUE,
    T1p3scenarios = TRUE,
    policyBrief = TRUE
	)
```

## Running as a slurm job

- `sbatch submit.bat`


## Set the credentials for accessing the ECEMF Scenario Explorer database

This project uses the Python package [pyam](https://pyam-iamc.readthedocs.io) to query
scenario data directly from the IIASA database infrastructure.

Please run the following script once in a Python console:

```python
import pyam
pyam.iiasa.set_config("<username>", "<password>")
```

Refer to this [tutorial](https://pyam-iamc.readthedocs.io/en/stable/tutorials/iiasa_dbs.html)
for more information!

## LICENSE
This program is free software: you can redistribute it and/or modify it under the terms of the **GNU Affero General Public License** as published by the Free Software Foundation, **version 3** of the License or later. You can see the LICENSE details in https://www.gnu.org/licenses/agpl.txt


## AUTHOR

Renato Rodrigues