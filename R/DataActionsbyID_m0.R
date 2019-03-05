#' Wide format dataset with the sequence of actions by ID
#'
#' This is a function that translates a long to wide format dataset.
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'event.type' variable is
#' @param id.var a vector with the individuals identification. It is a \code{quo()} type.
#' @param event.var a vector with the cleaned concatenate events. See \code{CleanActions} function.
#' @param name.var.action A character string that will name the new variable of events
#'
#' @return This function returns a \code{data.frame} with the only one entry by individual
#' identification and a new 'action.var' variable.
DataActionsbyID <- function(data, id.var, event.var, name.var.action) {
    `%>%` <- magrittr::`%>%`  # Placeholder before removal of pipes
    `:=` <- rlang::`:=`  # Placeholder before removal of quasiquotation

    id.var <- rlang::enquo(id.var)
    event.var <- rlang::enquo(event.var)

    var_actions <- data %>%
        dplyr::group_by(!!id.var) %>%
        dplyr::mutate(!!name.var.action := paste0((!!event.var), collapse = " | "))  %>%
        dplyr::select( -!!event.var) %>%
        dplyr::filter(dplyr::row_number(!!id.var) == 1)

    return(var_actions)
}

