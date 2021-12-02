# Model Comparison

This project includes:
 - `.R\model_comparison_EU28`: a R markdown script to download data directly from the IIASA database and create a model comparison html report including all participating models of the ECEMF Working Package 1.

## Installation

You can use [Anaconda](https://www.anaconda.com/products/individual)
or [Miniconda](https://docs.conda.io/en/latest/miniconda.html)
to setup up a combined R and Python environment with all the dependencies needed
to run the script:

    conda env create -f environment.yaml

Then, activate the conda environment:

    conda activate model_comparison

Rename the file `credentials_example.json` to `credentials.json` and fill it with your IIASA database credentials to download the most up to date data.

Use Rscript command to run render the document using R Markdown::

    Rscript render.R


## Installation - if you already have an R environment

 - Make sure you have R and the required libraries installed in your system. All packages can be installed via `install.packages`

```R
pkgs <- c("reticulate",
          "rjson",
          "ggplot2",
          "dplyr",
          "tidyr",
          "gridExtra",
          "grid",
          "svglite")
install.packages(pkgs)
```

 - Rename the file `credentials_example.json` to `credentials.json` and fill it with your IIASA database credentials to download the most up to date data.
 - Set `updateResults` to true in the rmd file to download data directly from the iiasa database. The downloaded data will be saved to your local data folder. You can set the option back to FALSE afterwards (`updateResults = FALSE`) to use local data instead.


## LICENSE
This program is free software: you can redistribute it and/or modify it under the terms of the **GNU Affero General Public License** as published by the Free Software Foundation, **version 3** of the License or later. You can see the LICENSE details in https://www.gnu.org/licenses/agpl.txt
