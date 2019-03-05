#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions
#' (min-max) aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param strategy.var strategy.var
#' @param categ.var categ.var
#' @param namexlab namexlab
#' @param nameylab nameylab
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
PlotStrategybyCatPerformance <- function(data, strategy.var, categ.var,
                                         namexlab, nameylab) {
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
        ggplot2::xlab(namexlab) +
        ggplot2::ylab(nameylab)
}
