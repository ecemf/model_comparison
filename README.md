# Model Comparison

This project includes:
 - `main.R`: a R script to download data directly from the IIASA database and create a model comparison html report including all participating models of the ECEMF Working Package 1.
 - `ECEMP_charts.Rmd`: a R markdown script to create charts for the ECEMP presentation using the data from the ECEMF Working Package 1.

## Access to the ECEMF-internal Scenario Explorer

The ECEMF project uses the IIASA Scenario Explorer infrastructure to compile and
validate scenario data from the participating modeling teams.
The [ECEMF-internal Scenario Explorer](https://data.ece.iiasa.ac.at/ecemf-internal)
is accessible only for consortium members. Please reach out to the
[IIASA Scenario Services team](https://software.ece.iiasa.ac.at) to gain access.

## Installation

You can use [Anaconda](https://www.anaconda.com/products/individual)
or [Miniconda](https://docs.conda.io/en/latest/miniconda.html)
to setup up a combined R and Python environment with all the dependencies needed
to run the script:

    conda env create -f environment.yaml

Then, activate the conda environment:

    conda activate model_comparison

Use Rscript command to run render the document using R Markdown:

    Rscript render.R

## Installation - if you already have an R environment

 - Make sure you have R and the required libraries installed in your system. All packages can be installed via `install.packages`

```R
pkgs <- c("dplyr",
          "ggplot2",
          "ggpattern",
          "ggplotify",
          "gtable",
          "grid ", 
          "gridextra",
          "svglite",
          "tidyr",
          "rmarkdown",
          "reticulate",
	  "jsonlite",
	  "quitte"
	  )
	  	  
install.packages(pkgs)
```

 - Set `updateResults` to true in the rmd file to download data directly from the iiasa database. The downloaded data will be saved to your local data folder. You can set the option back to FALSE afterwards (`updateResults = FALSE`) to use local data instead.
 - Use Rscript command to run render the document using R Markdown::

    Rscript render.R

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
