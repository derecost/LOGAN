#' Plot: Percentage in arcsine values x PISA scores by Country
#'
#' This is a function that calculates the percentage in arcsine and plots it against
#' the PISA scores
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and performance
#' variables are
#' @param strategy.summary strategy.summary
#' @param performance.mean performance.mean
#' @param country.id  A string with the name of the countries variable. It is "quo()" type.
#' @param ylab.text A character string giving the text of the y-axis in the plot
#' @param xlab.text A character string giving the text of the x-axis in the plot
#' @param ylim.vector A numeric vector with the limits of the y-axis in the plot
#' @param xlim.vector A numeric vector with the limits of the x-axis in the plot
#'
#' @return This function returns a data frame and a plot
#'
# @examples
# DataArcSinebyPerformance(data= cp025q01.complete, strategy.var= quo(VOTAT1),
# performance.test=quo(PV1CPRO), country.id=quo(cnt), plotARcSine=T,
# ylab.text= 'PV1CPRO', xlab.text='VOTAT (percentage transformed in arcsine values)',
# ylim.vector=c(350,600), xlim.vector=c(0,1.4))
#'
#' @export
ScatterPlotbyPerformance <-
    function(data,
             strategy.summary,
             performance.mean,
             country.id,
             ylab.text,
             xlab.text,
             ylim.vector,
             xlim.vector) {

        plot(data[[strategy.summary]], data[[performance.mean]],
             xlab = xlab.text , ylab = ylab.text, ylim=ylim.vector, xlim = xlim.vector)
        text(data[[strategy.summary]], data[[performance.mean]],
             labels = data[[country.id]], cex=0.6, pos=3)
    }