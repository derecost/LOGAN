#' Frequency of specifics events in a variable of Actions - Summary
#'
#' This is a function that locates specific events (using the
#' \code{actions.search} argument) and create new variables associate with this
#' strategy.
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'action.var'
#'   variable is
#' @param freqact.var freqact.var
#' @param var var
#' @return This function returns a \code{data.frame} with the frequency of each
#'   specific events from the \code{actions.search} argument and
#'   "Freq.Actions.Search" summary.
FreqActionsSummary <- function(data, freqact.var, var){
    # Summary table FREQACTIONS by var
    tab.freqitem <- as.data.frame(table(data[, var]))
    for (j in seq(length(tab.freqitem$Var1))) {
        if (j == 1) {
            tab.perftest <- cbind(summary(data[data[, var] == as.character(tab.freqitem$Var1[j]), freqact.var]))
            colnames(tab.perftest)[j] <- paste0(var, "=", as.character(tab.freqitem$Var1[j]))
            tab.perftest <- rbind(tab.perftest, tab.freqitem$Freq[j])
        } else {
            tab.perftest2 <- cbind(summary(data[data[, var] == as.character(tab.freqitem$Var1[j]), freqact.var]))
            tab.perftest2 <- rbind(tab.perftest2, tab.freqitem$Freq[j])
            tab.perftest <- cbind(tab.perftest,tab.perftest2)
            colnames(tab.perftest)[j] <- paste0(var, "=", as.character(tab.freqitem$Var1[j]))
        }
    }
    tab.perftest2 <- as.data.frame.matrix(tab.perftest,
                                          row.names = seq(dim(tab.perftest)[1]))
    tot.general <- as.data.frame.matrix(rbind(cbind(summary(data[[freqact.var]])), length(data[[freqact.var]])),
                                        row.names = seq(dim(tab.perftest)[1]))
    colnames(tot.general) <- "Total"
    row.names(tab.perftest)[length(row.names(tab.perftest))] <- "Total N"

    tab.perftest <- cbind(row.names(tab.perftest), tot.general, tab.perftest2)
    names(tab.perftest)[1] <- "Statistics"
    cat(paste0("\n Summary of frequency of actions by ", var ," - Individual level"))
    pander::pandoc.table(tab.perftest,split.tables = 100)
}
