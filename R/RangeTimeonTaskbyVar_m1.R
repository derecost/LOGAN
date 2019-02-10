#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
RangeTimeonTaskbyVar <- function(data, tot.var, var.group) {
    tot.var <- rlang::enquo(tot.var)
    var.group <- rlang::enquo(var.group)

    n.event.var <- data %>%
        dplyr::filter(!is.na(!!tot.var)) %>%
        dplyr::group_by(!!var.group) %>%
        dplyr::summarize(n.STID = dplyr::n(), min.tot = min(!!tot.var),
                         average.tot = mean(!!tot.var),
                         sd.tot = stats::sd(!!tot.var),
                         median.tot = stats::median(!!tot.var),
                         max.tot = max(!!tot.var))
    print(n.event.var, n = 50)
    return(n.event.var)
}