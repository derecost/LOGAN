#' Time on task variable
#'
#' This is a function that reports the number of students and a summary of time
#' on task aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param starttime.vec a vector with the individuals' identifications. It is a
#'   \code{quo()} type.
#' @param endtime.vec a vector with the group variable. It is a \code{quo()}
#'   type.
#' @param divBy a vector with the group variable. It is a \code{quo()} type.
#' @param tot.var string containing the name of the output variable
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#' df <- m1$TOTVar(cp025q01.treated, "CP025Q01.START", "CP025Q01.END",
#'                 divBy = 60, tot.var = "CP025Q01.TOT")
#' str(df)
#'
TOTVar <- function(data, starttime.vec, endtime.vec, divBy = 1, tot.var) {
    for (i in seq(length(starttime.vec))) {
        data$TOT <- data[[endtime.vec[i]]] - data[[starttime.vec[i]]] / divBy
        names(data)[length(data)] <- tot.var
    }
    return(data)
}
