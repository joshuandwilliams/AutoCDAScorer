
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AutoCDAScorer

<!-- badges: start -->

[![codecov](https://codecov.io/gh/joshuandwilliams/AutoCDAScorer/graph/badge.svg?token=DVSFFFKKQ4)](https://codecov.io/gh/joshuandwilliams/AutoCDAScorer)
[![R CMD
check](https://github.com/joshuandwilliams/AutoCDAScorer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshuandwilliams/AutoCDAScorer/actions/workflows/R-CMD-check.yaml)
[![tensorflow
version](https://img.shields.io/badge/tensorflow-v2.16.2-orange)](https://www.tensorflow.org/)
[![python
version](https://img.shields.io/badge/python-v3.11.11-blue)](https://www.python.org/)
<!-- badges: end -->

The goal of AutoCDAScorer is to make scoring cell death areas on UV
spectra agroinfiltration images faster, more consistent, and less
subjective.

## Installation

You can install the development version of AutoCDAScorer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshuandwilliams/AutoCDAScorer")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo joshuandwilliams/AutoCDAScorer@HEAD
#> pillar  (1.10.1 -> 1.10.2) [CRAN]
#> rlang   (1.1.5  -> 1.1.6 ) [CRAN]
#> ggplot2 (3.5.1  -> 3.5.2 ) [CRAN]
#> Installing 3 packages: pillar, rlang, ggplot2
#> 
#> The downloaded binary packages are in
#>  /var/folders/t2/x6n0myz97530m_6h9clj5ds4000fpx/T//RtmpYuSrzU/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/private/var/folders/t2/x6n0myz97530m_6h9clj5ds4000fpx/T/RtmpYuSrzU/remotes11eee7506010e/joshuandwilliams-AutoCDAScorer-256d853/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/t2/x6n0myz97530m_6h9clj5ds4000fpx/T/RtmpYuSrzU/remotes11eee7506010e/joshuandwilliams-AutoCDAScorer-256d853/DESCRIPTION’
#>   ─  preparing ‘AutoCDAScorer’:
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>        NB: this package now depends on R (>= 3.5.0)
#>        WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>      serialize/load version 3 cannot be read in older versions of R.
#>      File(s) containing such objects:
#>        ‘AutoCDAScorer/inst/extdata/base_cnn_pca.rds’
#>   ─  building ‘AutoCDAScorer_0.0.0.9000.tar.gz’
#>      Warning: invalid gid value replaced by that for user 'nobody'
#>      
#> 
```

## Usage
