#' @title Clean events
#' @description This function allows you to clean events in the 'event.type'
#'   variable
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'event.type'
#'   variable is
#' @param event.type a vector with concatenate events. See \code{ConcatActions}
#'   function.
#' @param clear.events a vector where all the events to be cleaned are listed.
#'   Each element of this vector needs to be of a \code{"event"=""} type.
#'
#' @return This function returns a \code{data.frame} with the "new.event.type"
#'   variable that cleaned events from the "event.type" variable.
#'
#' @examples
#' \dontrun{
#'   m0$CleanActions(df.conc, event_type, clear.events)
#' }
CleanActions <- function(data, event.type, clear.events) {
    event.type <- rlang::enquo(event.type)
    data <- dplyr::mutate(data,
                          new.event.type = stringr::str_replace_all(!!event.type,
                                                                    clear.events))
    return(data)
}
