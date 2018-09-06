#library(tidyverse)

#####################################
##extracting the start or end time###
#####################################
VarTimebyID <- function(data, id.var, time.var, event.var, name.var.time, new.name) {
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
#################################
##Time var as a numeric vector###
#################################
#' Time var as a numeric vector
#'
#' This is a function that transforms a factor var time in numeric.
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
NumericTimeVar <- function(data, vector.time){

  for(i in seq(length(vector.time))){
    if(class(data[[vector.time[i]]]) == "factor"){
      data[[vector.time[i]]] <- as.numeric(levels(data[[vector.time[i]]]))[data[[vector.time[i]]]]
    }
  }
    return(data)
}

##########################
##Time on task variable###
##########################
#' Time on task variable
#'
#' This is a function that reports the number of students and a summary of time on task
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param starttime.vec a vector with the individuals identification. It is a \code{quo()} type.
#' @param endtime.vec a vector with the group variable. It is a \code{quo()} type.
#' @param divBy a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
TOTVar <- function(data, starttime.vec, endtime.vec, divBy = NA, tot.var) {

  for(i in seq(length(starttime.vec))){
    if(is.na(divBy)){
      data$TOT <- data[[endtime.vec[i]]] - data[[starttime.vec[i]]]
    } else{
      data$TOT <- (data[[endtime.vec[i]]] - data[[starttime.vec[i]]])/divBy
      }

  #names(data)[length(data)] <- paste0(substr(starttime.vec[i], start = 1, stop = 8), ".TOT")
    names(data)[length(data)] <- tot.var
  }

  return(data)
}

##################################
##Time on task and performance ###
##################################
#' Summary of time on task by var
#'
#' This is a function that reports the number of students and a summary of time on task
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param start.time a vector with the individuals identification. It is a \code{quo()} type.
#' @param end.time a vector with the group variable. It is a \code{quo()} type.
#' @param performance.item a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
SummaryTOTbyVar <- function(data, tot.var, performance.item) {

  #Summary table:
  tab.freqitem <- as.data.frame(table(data[, performance.item]))
  for (j in seq(length(tab.freqitem$Var1))) {
    data2 <- as.data.frame(data[data[, performance.item]==as.character(tab.freqitem$Var1[j]), tot.var])
    names(data2) <- tot.var

        if(j==1){
      tab.perftest <- as.matrix.data.frame(rbind(round(length(data2[[tot.var]]),0), round(min(data2[[tot.var]]),2) ,
                            round(quantile(data2[[tot.var]], probs=0.25), 2), round(median(data2[[tot.var]]), 2),
                            round(mean(data2[[tot.var]]), 2), round(sd(data2[[tot.var]]), 2),
                            round(quantile(data2[[tot.var]], probs=0.75),2),
                            round(max(data2[[tot.var]]), 2)))
      colnames(tab.perftest)[j] <- paste0(performance.item, "=", as.character(tab.freqitem$Var1[j]))
    }else{
      tab.perftest2 <- as.matrix.data.frame(rbind(round(length(data2[[tot.var]]),0), round(min(data2[[tot.var]]),2) ,
                             round(quantile(data2[[tot.var]], probs=0.25), 2), round(median(data2[[tot.var]]), 2),
                             round(mean(data2[[tot.var]]), 2), round(sd(data2[[tot.var]]), 2),
                             round(quantile(data2[[tot.var]], probs=0.75),2),
                             round(max(data2[[tot.var]]), 2)))
      tab.perftest <- cbind(tab.perftest, tab.perftest2)
      colnames(tab.perftest)[j] <- paste0(performance.item, "=", as.character(tab.freqitem$Var1[j]))
    }
  }

  tot.general <- as.matrix.data.frame(rbind(round(length(data[[tot.var]]),0), round(min(data[[tot.var]]),2) ,
                                            round(quantile(data[[tot.var]], probs=0.25), 2), round(median(data[[tot.var]]), 2),
                                            round(mean(data[[tot.var]]), 2), round(sd(data[[tot.var]]), 2),
                                            round(quantile(data[[tot.var]], probs=0.75),2),
                                            round(max(data[[tot.var]]), 2)))

  tab.perftest <- cbind(c("Total N", "Min", "1st.Qu", "Median", "Mean", "SD", "3st.Qu", "Max"), tot.general, tab.perftest)
  colnames(tab.perftest) <- c("Statistics","ToT Total", colnames(tab.perftest)[3:length(colnames(tab.perftest))])

  cat("\n Summary of Time on Task by Performance - Individual level")
  pander::pandoc.table(tab.perftest,split.tables=100)

}

###########################################
##check response time of students by var###
###########################################
#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
RangeTimeonTaskbyVar <- function(data, tot.var, var.group) {
  tot.var <- rlang::enquo(tot.var)
  var.group <- rlang::enquo(var.group)

  n.event.var <- data %>%
    dplyr:: filter(!is.na(!!tot.var)) %>%
    dplyr::group_by(!!var.group) %>%
    dplyr::summarize(n.STID = n(), min.tot = min(!!tot.var), average.tot = mean(!!tot.var),
                     sd.tot = sd(!!tot.var), median.tot = median(!!tot.var),
                     max.tot = max(!!tot.var))
  print(n.event.var, n=50)
  return(n.event.var)
}

###########################################
##Plot ToT by var###
###########################################
#' Check response time by var
#'
#' This is a function that reports the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
#' @param  data A \code{matrix} or \code{data.frame}
#' @param tot.var a vector with the total time. It is a \code{quo()} type.
#' @param var.group a vector with the group variable. It is a \code{quo()} type.
#'
#' @return This function returns a \code{data.frame} with the number of students and number de actions (min-max)
#' aggregated by a specific variable.
#'
# @examples
# RangeNumberActionsbyVar(data = cp025q01.data, id.var = quo(NewID), var.group = quo(cnt))
#'
#' @export
PlotTimeonTaskbyVar <- function(data, tot.var, performance.item, namexlab, nameylab) {

  data[[performance.item]] <- as.factor(data[[performance.item]])

  ggplot2::ggplot(data, aes_string(x=tot.var, fill=performance.item))+ theme_minimal() +  scale_fill_grey() +
    geom_density(alpha = 0.5)+ xlab(namexlab) + ylab(nameylab) + xlim(0, 5) + ylim(0, 1)
}
