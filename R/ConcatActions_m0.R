#' Concatenate events
#'
#' This function allows you to concatenate event actions from diferent variables
#' in a unique vector.
#'
#' @param data A \code{matrix} or \code{data.frame} where the concatenate events
#'   are
#' @param concat.events a vector where all the events are listed. Each element
#'   of this vector needs to be of a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the concatenate events
#'   in the 'event.type' variable.
#'
#' @export
ConcatActions <- function(data, concat.events) {

    for(i in seq(length(concat.events))){
        events <- concat.events[[i]]
        #events <- rlang::quo(events)

        if(i==1){
            data <- data %>%
                dplyr::mutate(event.type = !!events)
        }else{
            data <- data %>%
                dplyr::mutate(event.type = paste0(event.type,"_",!!events))
        }
    }
    return(data)
}