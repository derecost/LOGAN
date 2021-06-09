#' Check number of students and actions by var
#'
#' This is a function that reports the number of students and number de actions
#' (min-max) aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param id.var a vector with the individuals identification. It is a
#'   \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#' @param save.table if \code{TRUE}, will save the table generated as an object
#'   of class \code{data.frame}. Otherwise, will print the table in pandoc
#'   format, but the object will not be saved (even if the user assigns it to an object)
#' @return By default, this function returns a \code{data.frame} with the
#'   number of students and number de actions (min-max) aggregated by a 
#'   specific variable. If \code{save.table == FALSE}, however, it will print a 
#'   pandoc table instead.
#' @examples
#' m0$RangeNumberActionsbyVar(cp025q01.treated, NewID, CNT, save.table = FALSE)
#' 
RangeNumberActionsbyVar <- function(data, id.var, var.group, save.table = TRUE) {
    `%>%` <- magrittr::`%>%`  # Placeholder before removal of pipes
    n.event <- NULL # Works around the "no visible binding for global variable" note
    id.var <- rlang::enquo(id.var)
    var.group <- rlang::enquo(var.group)

    n.event.var <- data %>%
        dplyr::group_by(!!var.group, !!id.var) %>%
        dplyr::summarize(n.event = dplyr::n()) %>%
        dplyr::group_by(!!var.group) %>%
        dplyr::summarize("Total N" = dplyr::n(),
                         "Min" = min(n.event),
                         "Average" = mean(n.event),
                         "SD" = stats::sd(n.event),
                         "Median" = stats::median(n.event),
                         "Max" = max(n.event))
    n.event.var <- as.data.frame(n.event.var)

    if (save.table) {
        return(n.event.var)
    } else {
        message("Summary of number of events by country - Individual level\n")
        pander::pandoc.table(n.event.var,split.tables = 100)
    }
}
