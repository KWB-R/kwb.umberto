[![R-CMD-check](https://github.com/KWB-R/kwb.umberto/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.umberto/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.umberto/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.umberto/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.umberto/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.umberto)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.umberto)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.umberto)](https://kwb-r.r-universe.dev/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3604006.svg)](https://doi.org/10.5281/zenodo.3604006)

# kwb.umberto

Helper functions for data import, aggregation and
visualisation of UMBERTO (https://www.ifu.com/umberto/) model output.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.umberto' from GitHub
remotes::install_github("KWB-R/kwb.umberto")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.umberto](https://kwb-r.github.io/kwb.umberto)

Development: [https://kwb-r.github.io/kwb.umberto/dev](https://kwb-r.github.io/kwb.umberto/dev)
