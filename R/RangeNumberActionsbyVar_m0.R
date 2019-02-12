#' Check number of students and actions by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param id.var a vector with the individuals identification. It is a \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#' @param save.table if \code{TRUE}, will return the table printed
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @export
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
    #print(n.event.var, n=50)
    n.event.var <- as.data.frame(n.event.var)

    cat("\n Summary of number of events by country - Individual level")
    pander::pandoc.table(n.event.var,split.tables=100)
    if(save.table == TRUE) {
        return(n.event.var)
    }
}
