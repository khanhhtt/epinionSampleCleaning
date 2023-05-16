#' Get path to epinionSampleCleaning example
#'
#' epinionSampleCleaning comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' epinionSampleCleaning_example()
#' epinionSampleCleaning_example("sample_test.xlsx")

epinionSampleCleaning_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "epinionSampleCleaning"))
  } else {
    system.file("extdata", file, package = "epinionSampleCleaning", mustWork = TRUE)
  }
}
