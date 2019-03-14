#' Report: Descriptive statistics by strategy
#'
#' This is a function that reports a descriptive analysis of the strategy and
#' students performance
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and
#'   performance variables are
#' @param strategy.var A character string with the name of the strategy variable
#' @param performance.item A character string with the name of the item
#'   performance variable
#' @param performance.test A character string with the name of the test
#'   performance variable
#' @param PartialCredit Logical. It can be used when the item is partial credit
#'   score.
#'
#' @return This function returns a report with a descriptive analysis of the
#'   strategy and students performance
#'
#' @examples
#' # Data preparation
#' df <- cp025q01
#' df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
#' trim.vars <- c("event", "event_type", "top_setting", "central_setting",
#'                "bottom_setting", "diag_state")
#' df.trimmed <- m0$TrimVar(df, trim.vars)
#' library(rlang)
#' concat.events <- c(quo(event), quo(event_type), quo(top_setting),
#'                    quo(central_setting), quo(bottom_setting), quo(diag_state))
#' df.conc <- m0$ConcatActions(df.trimmed, concat.events)
#' clear.events <- c("ACER_EVENT_" = "", "_NULL" = "")
#' df.clean <- m0$CleanActions(df.conc, event.type, clear.events)
#' time.vars <- c("cnt", "schoolid", "StIDStd", "NewID")
#' df.start <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
#'                            "START_ITEM",
#'                            "CP025Q01.START")[c(time.vars, "CP025Q01.START")]
#' df.end   <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
#'                            "END_ITEM",
#'                            "CP025Q01.END")[c(time.vars, "CP025Q01.END")]
#' df.dataAct <- m0$DataActionsbyID(df.clean, NewID, new.event.type,
#'                                  "CP025Q01.ACTIONS")[c(time.vars, "CP025Q01.ACTIONS")]
#' df.time <- dplyr::left_join(df.start, df.end, by = time.vars)
#' df.timeActions <- dplyr::left_join(df.time, df.dataAct, by = time.vars)
#' df.dataAct <- m1$TOTVar(df.timeActions, "CP025Q01.START", "CP025Q01.END",
#'                         divBy = 60, tot.var = "CP025Q01.TOT")
#' actions <- c("apply_1_0_0", "apply_-1_0_0", "apply_2_0_0", "apply_-2_0_0",
#'              "apply_0_1_0", "apply_0_-1_0", "apply_0_2_0", "apply_0_-2_0",
#'               "apply_0_0_1", "apply_0_0_-1", "apply_0_0_2", "apply_0_0_-2")
#' df.dataAct <- m2$VarActionSearch(df.timeActions, "CP025Q01.ACTIONS", actions)
#' df.dataAct$top <- as.numeric(apply(df.dataAct[, 8:11], 1, sum) > 0)
#' df.dataAct$bot <- as.numeric(apply(df.dataAct[, 12:15], 1, sum) > 0)
#' df.dataAct$mid <- as.numeric(apply(df.dataAct[, 16:19], 1, sum) > 0)
#' df.dataAct$votat  <- as.numeric(df.dataAct$top > 0 & df.dataAct$bot > 0 &
#'                                  df.dataAct$mid > 0)
#' micro <- pisa
#' names(df.dataAct)[1:2] <- c("CNT", "SCHOOLID")
#' df.complete <- dplyr::left_join(df.dataAct, micro,
#'                                 by = c("CNT", "SCHOOLID", "StIDStd"))
#' df.complete <- df.complete[!is.na(df.complete$CP025Q01), ]
#' df.complete$CP025Q01 <- as.numeric(df.complete$CP025Q01 == 2)
#'
#' # Function Demonstration
#' m2$DescriptiveStrategy(df.complete, "votat", "CP025Q01", "PV1CPRO")
#'
DescriptiveStrategy <- function(data, strategy.var, performance.item,
                                performance.test, PartialCredit = FALSE) {
    old <- options(warn = 0)
    options(warn = -1)
    if (PartialCredit == TRUE) {
        # Frequency table (N): categorical variables with partialCredit: only
        # accepts options: 0, 0.5 and 1
        tab.freqitem <- as.data.frame(table(data[, performance.item]))
        for (i in seq(length(tab.freqitem$Var1))) {
            if (i == 1 & tab.freqitem$Var1[i] != 0.5) {
                scalename2 <- tab.freqitem$Var1[i]
                n2 <- tab.freqitem$Freq[i]
                total2 <- tab.freqitem$Freq[i]
                perc2 <- sprintf("%.0f%%", 100 * n2/sum(tab.freqitem$Freq))
            } else if (tab.freqitem$Var1[i] == 0.5) {
                i = i + 1
                scalename2 <- paste0(scalename2, "|", tab.freqitem$Var1[i])
                n2 <- paste0(n2, "|", tab.freqitem$Freq[i - 1] + tab.freqitem$Freq[i])
                total2 <- total2 + tab.freqitem$Freq[i - 1] + tab.freqitem$Freq[i]
                perc2 <- paste0(perc2, "|",
                                sprintf("%.0f%%", 100 *
                                            (tab.freqitem$Freq[i - 1] * 0.5 +
                                                 tab.freqitem$Freq[i]) /
                                            sum(tab.freqitem$Freq)))
            }
        }
    } else {
        tab.freqitem <- as.data.frame(table(data[, performance.item]))
        for (i in seq(length(tab.freqitem$Var1))) {
            if (i == 1) {
                scalename2 <- tab.freqitem$Var1[i]
                n2 <- tab.freqitem$Freq[i]
                total2 <- tab.freqitem$Freq[i]
                perc2 <- sprintf("%.0f%%", 100 * n2 / sum(tab.freqitem$Freq))
            } else{
                scalename2 <- paste0(scalename2, "|", tab.freqitem$Var1[i])
                n2 <- paste0(n2, "|", tab.freqitem$Freq[i])
                total2 <- total2 + tab.freqitem$Freq[i]
                perc2 <- paste0(perc2, "|",
                                sprintf("%.0f%%", 100 * tab.freqitem$Freq[i] /
                                            sum(tab.freqitem$Freq)))
            }
        }
    }
    # Frequency table (N): categorical variables
    tab.freqvotat <- as.data.frame(table(data[, strategy.var]))
    for (i in seq(length(tab.freqvotat$Var1))) {
        if (i == 1) {
            scalename1 <- tab.freqvotat$Var1[i]
            n1 <- tab.freqvotat$Freq[i]
            total1 <- tab.freqvotat$Freq[i]
            perc1 <- sprintf("%.0f%%", 100 * tab.freqvotat$Freq[i] / sum(tab.freqvotat$Freq))
        } else{
            scalename1 <- paste0(scalename1, "|", tab.freqvotat$Var1[i])
            n1 <- paste0(n1, "|", tab.freqvotat$Freq[i])
            total1 <- total1 + tab.freqvotat$Freq[i]
            perc1 <- paste0(perc1, "|",
                            sprintf("%.0f%%", 100 * tab.freqvotat$Freq[i] /
                                        sum(tab.freqvotat$Freq)))
        }
    }

    tab.print <-
        as.data.frame(rbind(
            cbind(paste0("Strategy: ", strategy.var), scalename1, perc1, n1, total1),
            cbind(paste0("Performance: ", performance.item), scalename2, perc2, n2, total2)
        ))

    colnames(tab.print) <- c("Construct", "Scale","Relat. Freq.", "Frequencies", "Total N")
    cat("\n Frequency table - Individual level")
    pander::pandoc.table(tab.print,split.tables = 100)

    #Chi-squared test of independence
    crostab.freqvotat <- table(data[, c(strategy.var, performance.item)])
    polychoric.crostab <- psych::polychoric(data[, c(strategy.var, performance.item)])
    xtest.crostab <- stats::chisq.test(crostab.freqvotat)

    cat("Measures of association between", strategy.var, "and",
        performance.item, "- Individual level")
    crostab.freqvotat <- rbind(crostab.freqvotat, apply(crostab.freqvotat, 2, sum))
    crostab.freqvotat <- cbind(crostab.freqvotat, apply(crostab.freqvotat, 1, sum))
    crostab.freqvotat <- cbind(row.names(crostab.freqvotat),
                               as.data.frame.matrix(crostab.freqvotat,
                                                    row.names = seq(dim(crostab.freqvotat)[1])))
    crostab.freqvotat[, 1] <- as.character(crostab.freqvotat[, 1])
    crostab.freqvotat[dim(crostab.freqvotat)[1], 1] <- "Total"
    names(crostab.freqvotat)[1] <- paste0(strategy.var, " / ", performance.item)
    names(crostab.freqvotat)[length(names(crostab.freqvotat))] <- "Total"

    value.p <- ifelse(round(xtest.crostab$p.value,4) < 0.01, "0.01",
                      round(xtest.crostab$p.value,4))
    pander::pandoc.table(crostab.freqvotat,split.tables = 100)
    cat(paste0("Chi-squared = ",round(xtest.crostab$statistic, 2),", df = ",
               xtest.crostab$parameter, ", p-value < ", value.p, "\n"))

    if (prod(dim(table(data[, c(strategy.var, performance.item)]))) == 4) {
        phi.crostab <- psych::phi(table(data[, c(strategy.var, performance.item)]))
        cat(paste0("Phi coefficient = ", round(phi.crostab, 4), "\n"))
    }

    #Summary table: continua variables - Summary of  PV1CPRO  by strategy
    for (j in seq(length(tab.freqvotat$Var1))) {
        data2 <- as.data.frame(data[data[, strategy.var] == as.character(tab.freqvotat$Var1[j]), performance.test])
        names(data2) <- performance.test

        if (j == 1) {
            tab.perftest <- as.matrix.data.frame(rbind(round(length(data2[[performance.test]]),0),
                                                       round(min(data2[[performance.test]]),2),
                                                       round(stats::quantile(data2[[performance.test]],
                                                                             probs = 0.25), 2),
                                                       round(stats::median(data2[[performance.test]]), 2),
                                                       round(mean(data2[[performance.test]]), 2),
                                                       round(stats::sd(data2[[performance.test]]), 2),
                                                       round(stats::quantile(data2[[performance.test]],
                                                                             probs = 0.75),2),
                                                       round(max(data2[[performance.test]]), 2)))
            colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
        } else {
            tab.perftest2 <- as.matrix.data.frame(rbind(round(length(data2[[performance.test]]),0),
                                                        round(min(data2[[performance.test]]), 2),
                                                        round(stats::quantile(data2[[performance.test]],
                                                                              probs = 0.25), 2),
                                                        round(stats::median(data2[[performance.test]]), 2),
                                                        round(mean(data2[[performance.test]]), 2),
                                                        round(stats::sd(data2[[performance.test]]), 2),
                                                        round(stats::quantile(data2[[performance.test]],
                                                                              probs = 0.75),2),
                                                        round(max(data2[[performance.test]]), 2)))
            tab.perftest <- cbind(tab.perftest, tab.perftest2)
            colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
        }
    }

    tot.general <- as.matrix.data.frame(rbind(round(length(data[[performance.test]]),0),
                                              round(min(data[[performance.test]]),2) ,
                                              round(stats::quantile(data[[performance.test]],
                                                                    probs = 0.25), 2),
                                              round(stats::median(data[[performance.test]]), 2),
                                              round(mean(data[[performance.test]]), 2),
                                              round(stats::sd(data[[performance.test]]), 2),
                                              round(stats::quantile(data[[performance.test]],
                                                                    probs = 0.75),2),
                                              round(max(data[[performance.test]]), 2)))

    tab.perftest <- cbind(c("Total N", "Min", "1st.Qu", "Median", "Mean", "SD", "3st.Qu", "Max"),
                          tot.general, tab.perftest)
    colnames(tab.perftest) <- c("Statistics", "Total",
                                colnames(tab.perftest)[3:length(colnames(tab.perftest))])

    cat("\n Summary of",performance.test,"by",strategy.var,"- Individual level")
    pander::pandoc.table(tab.perftest,split.tables = 100)
    polyserial.cor <- psych::polyserial(x = as.matrix(data[, performance.test]),
                                        y = as.matrix(data[, strategy.var]))
    cat(paste0("Biserial/Polyserial correlation = ",round(polyserial.cor[1],4),"\n"))

    on.exit(options(old), add = TRUE)
}