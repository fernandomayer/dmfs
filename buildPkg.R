##======================================================================
## Script to check and build the package
##======================================================================

##----------------------------------------------------------------------
## Packages
library(devtools)
library(knitr)

##----------------------------------------------------------------------
## Run checks

## Load the package (to make functiona available)
load_all()

## Create/update NAMESPACE, *.Rd files.
document()

## Check documentation.
check_man()

## Check functions, datasets, run examples, etc. Using cleanup = FALSE
## and check_dir = "../" will create a directory named FishMaps.Rcheck
## with all the logs, manuals, figures from examples, etc.
check(manual = TRUE, vignettes = FALSE, check_dir = "../")

## Examples
# Run examples from all functions of the package
run_examples()
# Run examples from a specific function
# dev_example("yscale.components.right")

## Show all exported objects.
ls("package:dmfs")
packageVersion("dmfs")

## Build the package (it will be one directory up)
build(manual = TRUE, vignettes = FALSE)
# build the binary version for windows (not used)
# build_win() # not used here. see below

##----------------------------------------------------------------------
## Test installation.

## Test install with install.packages()
pkg <- paste0("../dmfs_", packageVersion("dmfs"), ".tar.gz")
install.packages(pkg, repos = NULL)

##----------------------------------------------------------------------
## Generate README.md
knit(input = "README.Rmd")
