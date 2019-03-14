#' Frequency of specifics events in a variable of Actions
#'
#' This is a function that locates specific events (using the
#' \code{actions.search} argument) and create new variables associate with this
#' strategy.
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'action.var'
#'   variable is
#' @param action.var a vector with actions. See \code{DataActionsbyID} function.
#' @param actions.search A character vector with the actions to be searched.
#'
#' @return This function returns a \code{data.frame} with the frequency of each
#'   specific events from the \code{actions.search} argument and
#'   "Freq.Actions.Search" summary.
#'
#' @examples
#' # Data preparation
#' df <- cp025q01
#' df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
#' trim.vars <- c("event", "event_type", "top_setting", "diag_state")
#' df.trimmed <- m0$TrimVar(df, trim.vars)
#' library(rlang)
#' concat.events <- c(quo(event), quo(top_setting), quo(event_type))
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
#' # Function demonstration. Counting the instances of top_setting == 1
#' df.dataAct <- m2$VarActionSearch(df.timeActions, "CP025Q01.ACTIONS", "1_apply")
#' table(df.dataAct$freq.1_apply)
VarActionSearch <- function(data, action.var, actions.search) {
    for (i in seq(length(actions.search))) {
        if (i == 1) {
            data.actionSearch <- stringr::str_count(data[[action.var]],
                                                    actions.search[i])
        } else {
            data.actionSearch <- cbind(data.actionSearch,
                                       stringr::str_count(data[[action.var]],
                                                          actions.search[i]))
        }
    }
    data.actionSearch <- as.data.frame(data.actionSearch)
    names(data.actionSearch) <- paste0("freq.", actions.search)
    data <- cbind(as.matrix.data.frame(data), data.actionSearch)
    return(data)
}