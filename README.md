[![R-CMD-check](https://github.com/KWB-R/kwb.rect/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.rect/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.rect/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.rect/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.rect/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.rect)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.rect)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.rect)](https://kwb-r.r-universe.dev/)

# kwb.rect

This package provides functions to define and plot
rectangles, e.g. to plot rectangles side-by-side or on top of each
other.

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

# Install KWB package 'kwb.rect' from GitHub
remotes::install_github("KWB-R/kwb.rect")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.rect](https://kwb-r.github.io/kwb.rect)

Development: [https://kwb-r.github.io/kwb.rect/dev](https://kwb-r.github.io/kwb.rect/dev)
