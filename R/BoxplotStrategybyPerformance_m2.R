#' Plot: Boxplot PV1Math by Strategy var
#'
#' @param data data
#' @param strategy.var strategy.var
#' @param performance.test performance.test
#' @param ylab.text ylab.text
#' @param xlab.text xlab.text
#'
#' @export
BoxplotStrategybyPerformance <- function(data, strategy.var, performance.test,
                                         ylab.text, xlab.text) {
    ggplot2::ggplot(data, aes(x = as.factor(strategy.var), y = performance.test)) +
        geom_boxplot() +
        theme_bw() +
        scale_x_discrete(name=xlab.text) #+
    #scale_y_continuous(name=ylab.text)
}
