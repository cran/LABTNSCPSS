# LABTNSCPSS

LABTNSCPSS is an R package that implements a reproducible pipeline to process episode-level clinical data and compute chronic pathology, frailty, and comorbidity indices from diagnosis codes.

The package computes:

- Charlson Comorbidity Index (CCI)
- Elixhauser Comorbidity Index (ECI)
- Combined Comorbidity Index
- Frailty Index
- Morbidity-Frailty Index

from episode-level clinical data.
URL of the paper: https://doi.org/10.1016/j.ijmedinf.2026.106709
---

## Installation

Install from CRAN:

```r
install.packages("LABTNSCPSS")
```
Load the package:
```r
library(LABTNSCPSS)
```

##Quick Start Example

The example below uses the sample dataset included in the package.
```r

tarball <- "./CRAN/LABTNSCPSS_1.0.4.tar.gz"

install.packages(
  tarball,
  repos = NULL,
  type = "source"
)
library(LABTNSCPSS)
packageVersion("LABTNSCPSS")
args(LABTNSCPSS::run_pipeline)

# Load example dataset included in the package

input_file <- system.file(
  "extdata",
  "testpackage.csv",
  package = "LABTNSCPSS"
)

if (input_file == "") {
  stop("The example file was not found in the installed package.")
}

col_mapping <- list(
  patient_id = "trajectoire_id",
  ICD = "diagnostic_code",
  start_date = "date_debut",
  end_date = "date_fin",
  episode_id = "episode_id"
)

out_dir <- tempdir()

# To use a different system, simply change the coding_system from here; possible values ("ICD-10-CA", "ICD-10-CM", "ICD-11"):
results <- LABTNSCPSS::run_pipeline(
  input_file = input_file,
  col_mapping = col_mapping,
  coding_system = "ICD-10-CA",
  out_dir = out_dir
)

results
```
