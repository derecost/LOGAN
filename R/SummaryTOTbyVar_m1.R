#' Summary of time on task by var
#'
#' This is a function that reports the number of students and a summary of time on task
#' aggregated by a specific variable.
#'
#' @param data A \code{matrix} or \code{data.frame}
#' @param tot.var tot.var
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

    cat(paste0("\n Summary of Time on Task by ", performance.item ," - Individual level"))
    pander::pandoc.table(tab.perftest,split.tables=100)

}
