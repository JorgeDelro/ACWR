# Reference implementation: ACWR 0.2.0, the version on CRAN.
#
# The files in reference-0.2.0/ are R/utils.R, R/RAC.R, R/RAU.R, R/EWMA.R and
# R/ACWR.R taken unchanged from the git tag v0.2.0
# (git show v0.2.0:R/<file>). They are not sourced automatically (testthat
# only sources helper and setup files at the top level of tests/testthat):
# this helper loads them into their own environment, apart from the package
# namespace, so that test-default-windows.R can compare the current functions
# with the 0.2.0 ones on the same machine.

reference_0.2.0 <- function() {
  env <- new.env(parent = baseenv())
  for (f in c("utils.R", "RAC.R", "RAU.R", "EWMA.R", "ACWR.R")) {
    sys.source(test_path("reference-0.2.0", f), envir = env,
               keep.source = FALSE)
  }
  env
}
