#' Identify the position of specific events in a variable of Actions
#'
#' This is a function that locates specific events (using the \code{actions.search}
#' argument) and create new variables associate with this strategy.
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'action.var' variable is
#' @param action.var a vector with actions. See \code{DataActionsbyID} function.
#' @param actions.search A character vector with the actions to be searched.
#'
#' @return This function returns a \code{data.frame} with the frequency of each specific events
#' from the \code{actions.search} argument and "Freq.Actions.Search" summary.
#'
# @examples
# VarActionPosition(data = cm015q01.TimeActions, action.var = "ITEM.ACTIONS",
#                 actions.search = interactive.actions)
#
#' @export
VarActionPosition <- function(data, action.var, actions.search) {

    for(w in seq(length(actions.search))){

        data1 <- data %>%
            dplyr::filter(str_detect(get(action.var), actions.search[w]))

        data1$position <- NA

        for(i in seq(length(data1[[action.var]]))){
            str.STID <- str_locate_all(data1[[action.var]], "\\|")[[i]]

            for(j in seq(dim(str.STID)[1])){
                if(j == 1){
                    new.str <- str_sub(data1[[action.var]][i], start=1, end=str.STID[[j]]-2)
                    pos.str <- ifelse(new.str == actions.search[w], paste0(j , " | "), "")
                } else {
                    new.str <- str_sub(data1[[action.var]][i], start=str.STID[[j-1]]+2, end=str.STID[[j]]-2)
                    if(new.str == actions.search[w]){
                        pos.str <- paste0(pos.str, j , " | ")
                    }
                }
            }
            data1$position[i] <- pos.str
        }
        names(data1)[length(data1)] <- paste0("pos.",actions.search[w])
        suppressMessages(data <- dplyr::left_join(data, data1))
    }
    return(data)
}
