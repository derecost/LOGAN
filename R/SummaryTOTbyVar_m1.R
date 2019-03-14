#' Summary of time on task by var
#'
#' This is a function that reports the number of students and a summary of time
#' on task aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the time on task.
#' @param performance.item a vector with the group variable. It is a
#'   \code{quo()} type.
#' @param na.rm remove missing data in `performance.item`? Default is `FALSE`
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
#' # Function demonstration
#' m1$SummaryTOTbyVar(df.complete, "CP025Q01.TOT", "CP025Q01", TRUE)

SummaryTOTbyVar <- function(data, tot.var, performance.item, na.rm = FALSE) {
    # Removing NAs
    if (any(is.na(data[performance.item])) & na.rm) {
        message("Removing missing data in performance.item")
        data <- data[!is.na(data[performance.item]), ]
    }

    #Summary table:
    tab.freqitem <- as.data.frame(table(data[, performance.item]))
    for (j in seq(length(tab.freqitem$Var1))) {
        data2 <- as.data.frame(data[data[, performance.item] == as.character(tab.freqitem$Var1[j]), tot.var])
        names(data2) <- tot.var
        if (j == 1) {
            tab.perftest <- as.matrix.data.frame(rbind(round(length(data2[[tot.var]]), 0),
                                                       round(min(data2[[tot.var]]), 2) ,
                                                       round(stats::quantile(data2[[tot.var]], probs = 0.25), 2),
                                                       round(stats::median(data2[[tot.var]]), 2),
                                                       round(mean(data2[[tot.var]]), 2),
                                                       round(stats::sd(data2[[tot.var]]), 2),
                                                       round(stats::quantile(data2[[tot.var]], probs = 0.75), 2),
                                                       round(max(data2[[tot.var]]), 2)))
            colnames(tab.perftest)[j] <- paste0(performance.item, "=", as.character(tab.freqitem$Var1[j]))
        } else {
            tab.perftest2 <- as.matrix.data.frame(rbind(round(length(data2[[tot.var]]),0),
                                                        round(min(data2[[tot.var]]),2) ,
                                                        round(stats::quantile(data2[[tot.var]], probs = 0.25), 2),
                                                        round(stats::median(data2[[tot.var]]), 2),
                                                        round(mean(data2[[tot.var]]), 2),
                                                        round(stats::sd(data2[[tot.var]]), 2),
                                                        round(stats::quantile(data2[[tot.var]], probs = 0.75), 2),
                                                        round(max(data2[[tot.var]]), 2)))
            tab.perftest <- cbind(tab.perftest, tab.perftest2)
            colnames(tab.perftest)[j] <- paste0(performance.item, "=", as.character(tab.freqitem$Var1[j]))
        }
    }
    tot.general <- as.matrix.data.frame(rbind(round(length(data[[tot.var]]), 0),
                                              round(min(data[[tot.var]]),2) ,
                                              round(stats::quantile(data[[tot.var]], probs = 0.25), 2),
                                              round(stats::median(data[[tot.var]]), 2),
                                              round(mean(data[[tot.var]]), 2),
                                              round(stats::sd(data[[tot.var]]), 2),
                                              round(stats::quantile(data[[tot.var]], probs = 0.75),2),
                                              round(max(data[[tot.var]]), 2)))
    tab.perftest <- cbind(c("Total N", "Min", "1st.Qu", "Median", "Mean", "SD", "3st.Qu", "Max"), tot.general, tab.perftest)
    colnames(tab.perftest) <- c("Statistics","ToT Total", colnames(tab.perftest)[3:length(colnames(tab.perftest))])

    cat(paste0("\n Summary of Time on Task by ", performance.item , " - Individual level"))
    pander::pandoc.table(tab.perftest,split.tables = 100)
}
