#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param strategy.var strategy.var
#' @param categ.var categ.var
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
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
PlotStrategybyCatPerformance <- function(data, strategy.var, categ.var, namexlab, nameylab) {

    #must be factor!!
    #data[[categ.var]] <- as.factor(data[[categ.var]])
    #data[[strategy.var]] <- as.factor(data[[strategy.var]])

    strategy.var <- rlang::enquo(strategy.var)
    categ.var <- rlang::enquo(categ.var)

    aggregate.data <-  data %>%
        dplyr::group_by(!!strategy.var, !!categ.var) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::group_by(!!categ.var) %>%
        dplyr::mutate(freq = 100* n / sum(n))

    aggregate.data <- as.data.frame(aggregate.data)

    ggplot(data=aggregate.data, aes_string(x=names(aggregate.data)[2], y=names(aggregate.data)[4], fill=names(aggregate.data)[1])) +
        geom_bar(stat="identity", position=position_dodge())+ scale_fill_grey() +
        theme(axis.text.y = element_blank()) +
        geom_text(aes(label=paste(n, paste0(" (", round(freq,0), "%)"), sep='\n')), vjust=1.1, color="white",
                  position = position_dodge(0.9), size=3.5) +
        xlab(namexlab) + ylab(nameylab)
}
