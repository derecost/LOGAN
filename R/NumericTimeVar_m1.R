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
#' # Untreating variable
#' df <- cp025q01.treated
#' df$CP025Q01.START <- as.factor(df$CP025Q01.START)
#' class(df$CP025Q01.START)
#' 
#' # Retreating variable
#' df <- m1$NumericTimeVar(df, "CP025Q01.START")
#' class(df$CP025Q01.START)
#' 
NumericTimeVar <- function(data, vector.time) {
    time <- data[[vector.time]]
    if (class(time) == "factor") {
         data[[vector.time]] <- as.numeric(levels(time))[time]
    } else {
        stop("Variable is not a factor")
    }
    return(data)
}
