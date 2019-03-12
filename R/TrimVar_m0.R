#' Trim variables
#'
#' \code{TrimVar()} is a function that allows you to remove whitespace inside the
#' strings of a vector.
#'
#' @param data dataset
#' @param trim.vector vector of variables on the dataset to be trimmed
#'
#' @return This function returns a vector removing trailing and leading spaces
#' inside the original vector.
#' @examples
#' df <- cp025q01
#' trim.vars <- c("event")
#' trimmed.df <- m0$TrimVar(df, trim.vars)
#' table(df$event)          # untrimmed
#' table(trimmed.df$event)  # extra spaces removed
TrimVar <- function(data, trim.vector) {
    for (i in seq(length(trim.vector))) {
        events <- trim.vector[[i]]
        data[[events]] <- gsub("^\\s+|\\s+$", "", data[[events]])
    }
    return(data)
}
