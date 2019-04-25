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
#' \dontrun{
#'   df.dataAct <- m2$VarActionSearch(df.timeActions,
#'                                    "CP025Q01.ACTIONS", #'"1_apply")
#'   table(df.dataAct$freq.1_apply)
#' }
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