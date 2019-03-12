#' Concatenate events
#'
#' This function allows you to concatenate event actions from diferent variables
#' in a unique vector.
#'
#' @param data A \code{matrix} or \code{data.frame} where the concatenated
#'   events are
#' @param concat.events a vector where all the events are listed. Each element
#'   of this vector needs to be of a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the concatenated
#'   events in the 'event.type' variable.
#'
#' @details The output dataset will be identical to the input dataset, except
#'   for the addition of one column in the end, called "event.type". Each row of
#'   event.type contains the values of concat.events of all the rows.
#'
#' @examples
#' # Basic setup and cleaning
#' df <- cp025q01
#' vars <- c("event", "event_type")
#' df.trimmed <- m0$TrimVar(df, vars)
#'
#' # Function demonstration
#' library(rlang)
#' concat.events <- c(quo(event), quo(event_type))
#' df.conc <- m0$ConcatActions(df.trimmed, concat.events)
#' names(df)
#' names(df.conc)  # notice the extra variable in the end
#' table(df.conc$event.type)
ConcatActions <- function(data, concat.events) {
    event.type <- NULL # Workaround for "no visible binding for global variable"
    for (i in seq(length(concat.events))) {
        events <- concat.events[[i]]
        if (i == 1) {
            data <- dplyr::mutate(data, event.type = !!events)
        } else {
            data <- dplyr::mutate(data, event.type = paste0(event.type, "_",
                                                            !!events))
        }
    }
    return(data)
}
