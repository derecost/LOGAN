#' Read SPSS process data
#'
#' This is a simple function that, by default, reads an SPSS data file
#' and save it as a data frame.
#'
#' @param filename character string: the name of the file or URL to read.
#'
#' @return This function returns a data frame.
#'
#'
#' @export
ImportSPSS <- function(filename) {
    data <- foreign::read.spss(file=filename, use.value.labels = FALSE,
                               to.data.frame = TRUE, strip.white=TRUE)
    return(data)
}
