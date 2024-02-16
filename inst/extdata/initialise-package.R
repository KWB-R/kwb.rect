# Set the name of your (!) new package
package <- "kwb.rect"

# Set the path to the package directory
pkg_dir <- getwd()

# Create directory for R package
#kwb.pkgbuild::create_pkg_dir(pkg_dir)

# kwb.orcid::get_kwb_orcids()
author <- list(name = "Hauke Sonnenberg", orcid = "0000-0001-9134-2871")

description <- list(
  name = package,
  title = "R Package for Plotting Rectangles",
  desc  = paste(
    "This package provides functions to define and plot rectangles, e.g. to",
    "plot rectangles side-by-side or on top of each other."
  )
)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)
