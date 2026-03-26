# LABTNSCPSS

LABTNSCPSS is an R package that implements a reproducible pipeline to process episode-level clinical data and compute chronic pathology, frailty, and comorbidity indices from diagnosis codes.

The package computes:

- Charlson Comorbidity Index (CCI)
- Elixhauser Comorbidity Index (ECI)
- Combined Comorbidity Index
- Frailty Index
- Morbidity-Frailty Index

from episode-level clinical data.

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
# Load example dataset included in the package
input_file <- system.file("extdata", "testpackage.csv", package = "LABTNSCPSS")

# Define column mapping
col_mapping <- list(
  patient_id = "trajectoire_id",
  ICD        = "diagnostic_code",
  start_date = "date_debut",
  end_date   = "date_fin",
  episode_id = "episode_id"
)

# Choose output directory
out_dir <- tempdir()

# Run the pipeline
results <- run_pipeline(
  input_file   = input_file,
  col_mapping  = col_mapping,
  coding_system = "ICD-10-CA",
  out_dir      = out_dir
)

results
```