#library(foreign)
#library(tidyverse)

###########################
##Read SPSS process data###
###########################
#' Read SPSS process data
#'
#' This is a simple function that, by default, reads an SPSS data file
#' and save it as a data frame.
#'
#' @param filename character string: the name of the file or URL to read.
#'
#' @return This function returns a data frame.
#'
# @examples
# ImportSPSS(filename=".\\CBA_cp025q01_logs12_SPSS.sav")
#'
#' @export
ImportSPSS <- function(filename) {
data <- foreign::read.spss(file=filename, use.value.labels = FALSE, to.data.frame = TRUE, strip.white=TRUE)
return(data)
  }

###################
##Trim variables###
###################
#' Trim variables
#'
#' \code{TrimVar()} is a function that allows you to remove whitespace inside the
#' strings of a vector.
#'
#' @param x A character vector
#'
#' @return This function returns a vector removing trailing and leading spaces
#' inside the original vector.
#'
# @examples
# TrimVar(x)
#'
#' @export
TrimVar <- function (data, trim.vector){
  for(i in seq(length(trim.vector))){
    events <- trim.vector[[i]]
    data[[events]] <- gsub("^\\s+|\\s+$", "", data[[events]])
  }
  return(data)
}

########################
##Concatenate events ###
########################
#' Concatenate events
#'
#' This function allows you to concatenate event actions from diferent variables
#' in a unique vector.
#'
#' @param data A \code{matrix} or \code{data.frame} where the concatenate events are
#' @param concat.events a vector where all the events are listed. Each element of
#' this vector needs to be of a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the concatenate events in
#' the 'event.type' variable.
#'
# @examples
# ConcatActions(cp025q01.data, concat.events)
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

###############################################
##Clear events in the event.type variable ###
###############################################
#' Clean events
#'
#' This function allows you to clean events in the 'event.type' variable
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'event.type' variable is
#' @param event.type a vector with concatenate events. See \code{ConcatActions} function.
#' @param clear.events a vector where all the events to be cleaned are listed. Each element of
#' this vector needs to be of a \code{"event"=""} type.
#'
#' @return This function returns a \code{data.frame} with the 'new.event.type'
#' variable that cleaned events from the "event.type" variable.
#'
# @examples
# ConcatActions(cp025q01.data, concat.events)
#'
#' @export
CleanActions <- function(data, event.type, clear.events) {
  event.type <- rlang::enquo(event.type)

  data <- data %>%
    dplyr::mutate(new.event.type = stringr::str_replace_all(!!event.type, clear.events))
  return(data)
}

###############################################
##Dataset with the sequence of actions by ID###
###############################################
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
#'
# @examples
# CleanActions(cp025q01.data, quo(event.type), clear.events)
#'
#' @export
DataActionsbyID <- function(data, id.var, event.var, name.var.action) {
  id.var <- rlang::enquo(id.var)
  event.var <- rlang::enquo(event.var)

  var_actions <-   data %>%
    dplyr::group_by(!!id.var) %>%
    dplyr::mutate(!!name.var.action := paste0((!!event.var), collapse = " | "))  %>%
    dplyr::select( -!!event.var) %>%
    dplyr::filter(row_number(!!id.var) == 1)

  return(var_actions)
}

###############################################
##check number of events of students by var###
###############################################
#' Check number of students and actions by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param id.var a vector with the individuals identification. It is a \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
RangeNumberActionsbyVar <- function(data, id.var, var.group, save.table = TRUE) {
  id.var <- rlang::enquo(id.var)
  var.group <- rlang::enquo(var.group)

  n.event.var <- data %>%
    dplyr::group_by(!!var.group, !!id.var) %>%
    dplyr::summarize(n.event=n()) %>%
    dplyr::group_by(!!var.group) %>%
    dplyr::summarize("Total N" = n(), "Min" = min(n.event), "Average" = mean(n.event),
                     "SD" = sd(n.event), "Median" = median(n.event),
                     "Max" = max(n.event))
  #print(n.event.var, n=50)
  n.event.var <- as.data.frame(n.event.var)
  
  cat("\n Summary of number of events by country - Individual level")
  pander::pandoc.table(n.event.var,split.tables=100)
  
if(save.table == TRUE){
  return(n.event.var)
}  

}

