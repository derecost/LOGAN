#' Check response time by var
#'
#' This is a function that reports the number of students and number of actions
#' (min-max) aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param strategy.var strategy variable
#' @param categ.var categorizing variable
#' @param namexlab name of the variable in the x-axis
#' @param nameylab name of the variable in the y-axis
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#' # Data preparation
#' df <- cp025q01[cp025q01$cnt == "NOR", ]
#' df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
#' trim.vars <- c("event", "event_type", "top_setting", "central_setting",
#'                "bottom_setting", "diag_state")
#' df.trimmed <- m0$TrimVar(df, trim.vars)
#' library(rlang)
#' concat.events <- c(quo(event), quo(event_type), quo(top_setting),
#'                    quo(central_setting), quo(bottom_setting), quo(diag_state))
#' df.conc <- m0$ConcatActions(df.trimmed, concat.events)
#' clear.events <- c("ACER_EVENT_" = "", "_NULL" = "")
#' df.clean <- m0$CleanActions(df.conc, event.type, clear.events)
#' time.vars <- c("cnt", "schoolid", "StIDStd", "NewID")
#' df.start <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
#'                            "START_ITEM",
#'                            "CP025Q01.START")[c(time.vars, "CP025Q01.START")]
#' df.end   <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
#'                            "END_ITEM",
#'                            "CP025Q01.END")[c(time.vars, "CP025Q01.END")]
#' df.dataAct <- m0$DataActionsbyID(df.clean, NewID, new.event.type,
#'                                  "CP025Q01.ACTIONS")[c(time.vars, "CP025Q01.ACTIONS")]
#' df.time <- dplyr::left_join(df.start, df.end, by = time.vars)
#' df.timeActions <- dplyr::left_join(df.time, df.dataAct, by = time.vars)
#' df.dataAct <- m1$TOTVar(df.timeActions, "CP025Q01.START", "CP025Q01.END",
#'                         divBy = 60, tot.var = "CP025Q01.TOT")
#' df.dataAct <- m2$VarActionSearch(df.timeActions, "CP025Q01.ACTIONS", "apply_1_0_0")
#' df.dataAct$top <- as.numeric(apply(df.dataAct["freq.apply_1_0_0"], 1, sum) > 0)
#' micro <- pisa[pisa$CNT == "NOR", ]
#' names(df.dataAct)[1:2] <- c("CNT", "SCHOOLID")
#' df.complete <- dplyr::left_join(df.dataAct, micro,
#'                                 by = c("CNT", "SCHOOLID", "StIDStd"))
#'
#' df.complete$categ <- cut(df.complete$PV1CPRO, c(0, 423, 488, 553, 900))
#' df.dataplot <- df.complete[, c("top", "categ")]
#' df.dataplot[,1] <- as.factor(df.dataplot[,1])
#' df.dataplot[,2] <- as.factor(df.dataplot[,2])
#'
#' # Function demonstration
#' m2$PlotStrategybyCatPerformance(df.dataplot, top, categ,
#'                                  "Proficiency levels", "Percentage")
#'
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

