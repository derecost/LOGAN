#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions
#' (min-max) aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
#' @param performance.item name of the item variable
#' @param namexlab name of the plot's x-axis
#' @param nameylab name of the plot's y-axis. Defaults to "Density"
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#' m1$PlotTimeonTaskbyVar(cp025q01.treated, "CP025Q01.TOT", "CP025Q01",
#'                        namexlab = "Time on task (minutes)")
#'
PlotTimeonTaskbyVar <- function(data, tot.var, performance.item, namexlab,
                                nameylab = "Density") {

    data[[performance.item]] <- as.factor(data[[performance.item]])

    ggplot2::ggplot(data, ggplot2::aes_string(x    = tot.var,
                                              fill = performance.item)) +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_grey() +
        ggplot2::geom_density(alpha = 0.5) +
        ggplot2::xlab(namexlab) +
        ggplot2::ylab(nameylab) +
        ggplot2::xlim(0, 5) +
        ggplot2::ylim(0, 1)
}
