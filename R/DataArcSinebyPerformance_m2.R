#' Data: Percentage in arcsine values x PISA scores by Country
#'
#' This is a function that calculates the percentage in arcsine and plots it
#' against the PISA scores
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and
#'   performance variables are
#' @param strategy.var A string with the name of the strategy variable. It is
#'   "quo()" type.
#' @param performance.test A string with the name of the test performance
#'   variable. It is "quo()" type.
#' @param country.id  A string with the name of the countries variable. It is
#'   "quo()" type.
#'
#' @return This function returns a data frame and a plot
#'
#' @export
DataArcSinebyPerformance <- function(data, strategy.var, performance.test,
                                     country.id) {

        country.id <- rlang::enquo(country.id)
        strategy.var <- rlang::enquo(strategy.var)
        performance.test <- rlang::enquo(performance.test)

        grouped.data <- dplyr::group_by(data, !!country.id)
        arcsine.PercentInteraction <-
            dplyr::summarise(grouped.data,
                             PercentInteraction = mean(!!strategy.var),
                             ArcsinePercent = asin(sqrt(PercentInteraction)),
                             MeanPV = mean(!!performance.test))

        return(arcsine.PercentInteraction)
    }