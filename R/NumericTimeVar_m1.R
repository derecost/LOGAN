#' Time var as a numeric vector
#'
#' This is a function that transforms a factor var time in numeric.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param vector.time variable containing the time
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
#'
#' # Function demonstration
#' vector.time <- c("CP025Q01.END", "CP025Q01.START")
#' m1$NumericTimeVar(df.timeActions, vector.time)
NumericTimeVar <- function(data, vector.time){
    for (i in seq(length(vector.time))) {
        if (class(data[[vector.time[i]]]) == "factor") {
            data[[vector.time[i]]] <- as.numeric(levels(data[[vector.time[i]]]))[data[[vector.time[i]]]]
        }
    }
    return(data)
}
