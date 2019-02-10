#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
#' @param performance.item performance.item
#' @param namexlab namexlab
#' @param nameylab nameylab
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
PlotTimeonTaskbyVar <- function(data, tot.var, performance.item, namexlab, nameylab) {

    data[[performance.item]] <- as.factor(data[[performance.item]])

    ggplot2::ggplot(data, aes_string(x=tot.var, fill=performance.item))+ theme_minimal() +  scale_fill_grey() +
        geom_density(alpha = 0.5)+ xlab(namexlab) + ylab(nameylab) + xlim(0, 5) + ylim(0, 1)
}
