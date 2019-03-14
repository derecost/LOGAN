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
#' # Data preparation
#' df <- cp025q01
#' df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
#' trim.vars <- c("event", "event_type", "diag_state")
#' df.trimmed <- m0$TrimVar(df, trim.vars)
#' library(rlang)
#' concat.events <- c(quo(event), quo(event_type))
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
#' micro <- pisa
#' names(df.dataAct)[1:2] <- c("CNT", "SCHOOLID")
#' df.complete <- dplyr::left_join(df.dataAct, micro,
#'                                 by = c("CNT", "SCHOOLID", "StIDStd"))
#'
#' # Function demonstration
#' m1$PlotTimeonTaskbyVar(df.complete, "CP025Q01.TOT", "CP025Q01",
#'                        namexlab = "Time on task (minutes)")
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
