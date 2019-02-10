#' Time var as a numeric vector
#'
#' This is a function that transforms a factor var time in numeric.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param vector.time vector.time
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
NumericTimeVar <- function(data, vector.time){

    for(i in seq(length(vector.time))){
        if(class(data[[vector.time[i]]]) == "factor"){
            data[[vector.time[i]]] <- as.numeric(levels(data[[vector.time[i]]]))[data[[vector.time[i]]]]
        }
    }
    return(data)
}
