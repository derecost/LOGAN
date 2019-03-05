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
RangeTimeonTaskbyVar <- function(data, tot.var, var.group) {
    tot.var <- rlang::enquo(tot.var)
    var.group <- rlang::enquo(var.group)

    filtered.data <- dplyr::filter(data, !is.na(!!tot.var))
    grouped.data <- dplyr::group_by(filtered.data, !!var.group)
    n.event.var <- dplyr::summarize(grouped.data,
                                    n.STID = dplyr::n(), min.tot = min(!!tot.var),
                                    average.tot = mean(!!tot.var),
                                    sd.tot = stats::sd(!!tot.var),
                                    median.tot = stats::median(!!tot.var),
                                    max.tot = max(!!tot.var))
    print(n.event.var, n = 50)
    return(n.event.var)
}
