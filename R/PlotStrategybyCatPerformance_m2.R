#' Check response time by var
#'
#' This is a function that reports the number of students and number of actions
#' (min-max) aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param strategy.var strategy variable
#' @param categ.var categorizing variable
#' @param xlab name of the variable in the x-axis
#' @param ylab name of the variable in the y-axis
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#' # Data preparation
#' df <- cp025q01.treated
#' df$categ <- cut(df$PV1CPRO, c(0, 423, 488, 553, 900))
#' df.dataplot <- df[, c("top", "categ")]
#' df.dataplot[,1] <- as.factor(df.dataplot[,1])
#' df.dataplot[,2] <- as.factor(df.dataplot[,2])
#'
#' # Function demonstration
#' m2$PlotStrategybyCatPerformance(df.dataplot, top, categ,
#'                                  "Proficiency levels", "Percentage")
#'
PlotStrategybyCatPerformance <- function(data, strategy.var, categ.var,
                                         xlab, ylab) {
    `%>%` <- magrittr::`%>%`  # Placeholder before removal of pipes
     n <- NULL # Works around the "no visible binding for global variable" note
     freq <- NULL # Works around the "no visible binding for global variable" note
    strategy.var <- rlang::enquo(strategy.var)
    categ.var <- rlang::enquo(categ.var)

    aggregate.data <-  data %>%
        dplyr::group_by(!!strategy.var, !!categ.var) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::group_by(!!categ.var) %>%
        dplyr::mutate(freq = 100 * n / sum(n))

    aggregate.data <- as.data.frame(aggregate.data)

    ggplot2::ggplot(data = aggregate.data,
                    ggplot2::aes_string(x    = names(aggregate.data)[2],
                                        y    = names(aggregate.data)[4],
                                        fill = names(aggregate.data)[1])) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge()) +
        ggplot2::scale_fill_grey() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
        ggplot2::geom_text(ggplot2::aes(label = paste(n,
                                                      paste0(" (",
                                                             round(freq, 0),
                                                             "%)"),
                                                      sep = '\n')),
                           vjust = 1.1, color = "white",
                           position = ggplot2::position_dodge(0.9),
                           size = 3.5) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab)
}