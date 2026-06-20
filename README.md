# Replication Package

This repository contains replication files for the paper titled "Scarring the Life Course: Early-Career Precarity and Long-Term Fertility Outcomes in Japan" (co-authored with Manting Chen). 
The analysis uses restricted-use SSM 2015 data, JGSS data, and publicly available Employment Status Survey data obtained through the e-Stat API.

## Directory structure

Place the files in the following structure:

```text
Replication/
├── README.md
├── Replication.Rproj
├── master.do
├── Replication/
│   ├── 1.SSM.do
│   ├── 2.JGSS.do
│   ├── 3.Reshape_SSM.do
│   ├── 4.Desc.do
│   ├── 5.1.RCode.R
│   ├── 5.2.RCode supp nse.R
│   ├── 5.3.RCode supp edu.R
│   └── 6.Figure2.R
├── Data/
│   ├── SSM/
│   └── JGSS/
├── Figures/
└── Tables/
```

Users must obtain the SSM2015 and JGSS data directly from the relevant data providers and locate them at `Data/SSM` and `Data/JGSS` respectively.

## Data requirements

### SSM 2015

The SSM 2015 restricted-use file is required. Place the file here:

```text
Data/SSM/SSM2015_v070_20170227.dta
```

### JGSS

The JGSS data are required in the directory structure expected by `2.JGSS.do`. 

The JGSS data used in the analysis include JGSS 2006, 2012, 2015, 2016, 2017, and 2018. Some intermediate processing code also includes earlier JGSS files.

### e-Stat API key

`6.Figure2.R` downloads Employment Status Survey data from e-Stat. Before running the file, replace the placeholder API key with your own e-Stat API key:

```r
appid <- "YOUR_ESTAT_API_KEY"
```

## Software requirements

### Stata

The Stata files were prepared for Stata. The descriptive statistics file uses `eststo`, `estpost`, and `esttab`, which are provided by the user-written `estout` package. Install it in Stata if needed:

```stata
ssc install estout, replace
```

### R

The R scripts require the following packages:

```r
install.packages(c(
  "here", "dplyr", "tidyr", "ggplot2", "ggthemes", "ggrepel",
  "zoo", "egg", "estatapi", "gridExtra", "purrr", "haven",
  "boot", "reshape2"
))
```

## Master Stata file

Create a `master.do` file in the project root with the following contents. Users should open Stata, change the working directory to the root of the replication package, and run `do master.do`.

```stata
clear all
set more off

* Project root: run this file from the replication package root.
global ROOT "`c(pwd)'"

* Directory paths
global SSM_DIR    "${ROOT}/Data/SSM"
global JGSS_DIR   "${ROOT}/Data/JGSS"
global Data_DIR   "${ROOT}/Data/Intermediate"
global Figures_DIR "${ROOT}/Figures"
global Tables_DIR  "${ROOT}/Tables"

* Run Stata scripts
do "${ROOT}/Replication/1.SSM.do"
do "${ROOT}/Replication/2.JGSS.do"
do "${ROOT}/Replication/3.Reshape_SSM.do"
do "${ROOT}/Replication/4.Desc.do"
```

## Order of execution

Run the files in the following order.

### 1. Prepare SSM data in Stata

```stata
do "Replication/1.SSM.do"
```

This reads the restricted-use SSM 2015 data and creates edited SSM data files.

### 2. Prepare JGSS data in Stata

```stata
do "Replication/2.JGSS.do"
```

This creates the JGSS analysis files used by the R scripts:

```text
Data/JGSSmar.dta
Data/JGSSbirth.dta
Data/JGSSbridal.dta
```

### 3. Reshape SSM data in Stata

```stata
do "Replication/3.Reshape_SSM.do"
```

This creates the SSM analysis files used by the R scripts:

```text
Data/2015mar.dta
Data/2015birth.dta
Data/2015bridal.dta
```

### 4. Produce descriptive tables in Stata

```stata
do "Replication/4.Desc.do"
```

This produces:

```text
Tables/Desc_male.csv
Tables/Desc_female.csv
```

### 5. Produce main figures in R

Open `Replication.Rproj` in RStudio, or set the working directory to the project root, and run:

```r
source("R/5.1.RCode.R")
```

This produces:

```text
Figures/Figure3.pdf
Figures/Figure4.pdf
Figures/Figure5.pdf
Figures/Figure6.pdf
```

### 6. Produce supplementary figures in R

Run:

```r
source("R/5.3.RCode supp edu.R")
source("R/5.2.RCode supp nse.R")
```

These produce:

```text
Figures/AppFigure1.pdf
Figures/AppFigure2.pdf
```

### 7. Produce Employment Status Survey figure in R

After entering your own e-Stat API key in `6.Figure2.R`, run:

```r
source("R/6.Figure2.R")
```

This produces:

```text
Figures/Figure2.pdf
```

## Expected outputs

The replication package should reproduce the following figure files:

```text
Figures/Figure2.pdf
Figures/Figure3.pdf
Figures/Figure4.pdf
Figures/Figure5.pdf
Figures/Figure6.pdf
Figures/AppFigure1.pdf
Figures/AppFigure2.pdf
```

and the following descriptive table files:

```text
Tables/Desc_male.csv
Tables/Desc_female.csv
```

## Notes on reproducibility

- Do not use absolute paths in the replication files.
- Run Stata from the project root before executing `master.do`.
- Open `Replication.Rproj` before running the R scripts, or otherwise set the R working directory to the project root.
- The restricted-use SSM and JGSS source data are not included in this package.
- Some R scripts use bootstrap resampling. The scripts set random seeds for reproducibility.
