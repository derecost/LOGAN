#' Plot: Boxplot PV1Math by Strategy var
#'
#' @param data data
#' @param strategy.var strategy.var
#' @param performance.test performance.test
#' @param ylab.text ylab.text
#' @param xlab.text xlab.text
BoxplotStrategybyPerformance <- function(data, strategy.var, performance.test,
                                         ylab.text = "", xlab.text = "") {
    ggplot2::ggplot(data, ggplot2::aes(x = as.factor(strategy.var),
                                       y = performance.test)) +
        ggplot2::geom_boxplot() +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete(name = xlab.text)
}
