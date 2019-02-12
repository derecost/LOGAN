#' Extracting the start or end time
#'
#' @param data data
#' @param id.var id.var
#' @param time.var time.var
#' @param event.var event.var
#' @param name.var.time name.var.time
#' @param new.name new.name
#'
#' @export
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