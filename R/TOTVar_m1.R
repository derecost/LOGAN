#' Time on task variable
#'
#' This is a function that reports the number of students and a summary of time
#' on task aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param starttime.vec a vector with the individuals' identifications. It is a
#'   \code{quo()} type.
#' @param endtime.vec a vector with the group variable. It is a \code{quo()}
#'   type.
#' @param divBy a vector with the group variable. It is a \code{quo()} type.
#' @param tot.var string containing the name of the output variable
#'
#' @return This function returns a \code{data.frame} with the number of students
#'   and number de actions (min-max) aggregated by a specific variable.
#' @examples
#'
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
#'
#' # Function demonstration
#' m1$TOTVar(df.timeActions, "CP025Q01.START", "CP025Q01.END", divBy = 60,
#'           tot.var = "CP025Q01.TOT")
TOTVar <- function(data, starttime.vec, endtime.vec, divBy = NA, tot.var) {
    for (i in seq(length(starttime.vec))) {
        if (is.na(divBy)) {
            data$TOT <- data[[endtime.vec[i]]] - data[[starttime.vec[i]]]
        } else{
            data$TOT <- (data[[endtime.vec[i]]] - data[[starttime.vec[i]]]) / divBy
        }
        names(data)[length(data)] <- tot.var
    }
    return(data)
}
