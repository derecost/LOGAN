#' Read SPSS process data
#'
#' This is a simple function that, by default, reads an SPSS data file
#' and save it as a data frame. It is essentially a wrapper for 
#' foreign::read.spss with arguments common to log file datasets.
#'
#' @param filename character string: the name of the file or URL to read.
#'
#' @return This function returns a data frame.
ImportSPSS <- function(filename) {
    data <- foreign::read.spss(file=filename, use.value.labels = FALSE,
                               to.data.frame = TRUE, strip.white=TRUE)
    return(data)
}
