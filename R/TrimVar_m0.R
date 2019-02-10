#' Trim variables
#'
#' \code{TrimVar()} is a function that allows you to remove whitespace inside the
#' strings of a vector.
#'
#' @param data data
#' @param trim.vector trim.vector
#'
#' @return This function returns a vector removing trailing and leading spaces
#' inside the original vector.
#'



#'
#' @export
TrimVar <- function (data, trim.vector){
    for(i in seq(length(trim.vector))){
        events <- trim.vector[[i]]
        data[[events]] <- gsub("^\\s+|\\s+$", "", data[[events]])
    }
    return(data)
}
