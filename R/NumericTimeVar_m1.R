#' Time var as a numeric vector
#'
#' This is a function that transforms a factor var time in numeric.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param vector.time variable containing the time
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#' vector.time <- c("CP025Q01.END", "CP025Q01.START")
#' m1$NumericTimeVar(cp025q01.treated, vector.time)
#'
NumericTimeVar <- function(data, vector.time){
    for (i in seq(length(vector.time))) {
        if (class(data[[vector.time[i]]]) == "factor") {
            data[[vector.time[i]]] <- as.numeric(levels(data[[vector.time[i]]]))[data[[vector.time[i]]]]
        }
    }
    return(data)
}
