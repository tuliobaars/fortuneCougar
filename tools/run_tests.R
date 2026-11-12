# Run the testthat suite from the project folder:  Rscript tools/run_tests.R
# (the same as devtools::test(); R CMD check / devtools::check() also run it)
if (!requireNamespace("testthat", quietly = TRUE)) stop("install.packages('testthat')")
testthat::test_local(".", reporter = "progress")
