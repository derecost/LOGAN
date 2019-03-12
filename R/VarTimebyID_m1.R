#' Extracting the start or end time
#'
#' @param data data frame
#' @param id.var vector of unique identification
#' @param time.var vector with the time variable
#' @param event.var vector with the events
#' @param name.var.time name of the time string to filter (ex.: "START_ITEM" or
#'   "END_ITEM")
#' @param new.name name of the output variable
#' @return a data frame with `time` replaced with `new.name`. The variable
#'   `event.var` is dropped.
#' @examples
#' # Basic setup and cleaning
#' df <- cp025q01
#' df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
#' trim.vars <- c("event", "event_type", "diag_state")
#' df.trimmed <- m0$TrimVar(df, trim.vars)
#' library(rlang)
#' concat.events <- c(quo(event), quo(event_type))
#' df.conc <- m0$ConcatActions(df.trimmed, concat.events)
#' clear.events <- c("ACER_EVENT_" = "", "_NULL" = "")
#' df.clean <- m0$CleanActions(df.conc, event.type, clear.events)
#'
#' # Function demonstration
#' m1$VarTimebyID(df.clean, NewID, time, new.event.type, "START_ITEM",
#'                "CP025Q01.START")
#'
VarTimebyID <- function(data, id.var, time.var, event.var, name.var.time,
                        new.name) {
    `%>%` <- magrittr::`%>%`  # Placeholder before removal of pipes
    `:=` <- rlang::`:=`  # Placeholder before removal of quasiquotation

    id.var <- rlang::enquo(id.var)
    event.var <- rlang::enquo(event.var)
    time.var <- rlang::enquo(time.var)

    var_time <- data %>%
        dplyr::group_by(!!id.var) %>%
        dplyr::filter((!!event.var) == name.var.time) %>%
        dplyr::mutate(!!name.var.time := (!!time.var)) %>%
        dplyr::select(-(!!event.var),-(!!time.var))

    names(var_time)[length(var_time)] <- new.name

    return(var_time)
}