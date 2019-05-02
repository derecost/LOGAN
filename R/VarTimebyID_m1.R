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
#' # Data preparation
#' df <- cp025q01
#' df$id <- paste(df[, 1], df[, 2], df[, 3], sep = "-")
#' df <- m0$TrimVar(df, c("event", "event_type", "diag_state"))
#' df <- m0$ConcatActions(df, c(rlang::quo(event), rlang::quo(event_type)))
#' df <- m0$CleanActions(df, event.type, c("ACER_EVENT_" = ""))
#'
#' # Function demonstration
#' m1$VarTimebyID(df, id, time, new.event.type, "START_ITEM", "start")
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