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
#' # Data preparation
#' df <- cp025q01
#' df$id <- paste(df[, 1], df[, 2], df[, 3], sep = "-")
#' df <- m0$TrimVar(df, c("event", "event_type", "diag_state"))
#'
#' # Function demonstration
#' df.conc <- m0$ConcatActions(df, c(rlang::quo(event), rlang::quo(event_type)))
#' names(df)
#' names(df.conc)  # notice the extra variable in the end
#' table(df.conc$event.type)
#'
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
