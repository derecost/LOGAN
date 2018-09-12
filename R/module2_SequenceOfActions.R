#library(tidyverse)
#library(pander)
#library(psych)
##########################################
##Dataset with the frequency of actions###
##########################################
#' Frequency of specifics events in a variable of Actions - Summary
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
# VarActionSearch(data = cm015q01.TimeActions, action.var = "ITEM.ACTIONS",
#                 actions.search = interactive.actions)
#
#' @export
FreqActionsSummary <- function(data, freqact.var, var){
  #Summary table FREQACTIONS by var
    tab.freqitem <- as.data.frame(table(data[, var]))
    for (j in seq(length(tab.freqitem$Var1))) {
      if(j==1){
        tab.perftest <- cbind(summary(data[data[, var]==as.character(tab.freqitem$Var1[j]), freqact.var]))
        colnames(tab.perftest)[j] <- paste0(var, "=", as.character(tab.freqitem$Var1[j]))
        tab.perftest <- rbind(tab.perftest, tab.freqitem$Freq[j])
      }else{
        tab.perftest2 <- cbind(summary(data[data[, var]==as.character(tab.freqitem$Var1[j]), freqact.var]))
        tab.perftest2 <- rbind(tab.perftest2, tab.freqitem$Freq[j])
        tab.perftest <- cbind(tab.perftest,tab.perftest2)
        colnames(tab.perftest)[j] <- paste0(var, "=", as.character(tab.freqitem$Var1[j]))
      }

    }

    tab.perftest2 <- as.data.frame.matrix(tab.perftest,
                                          row.names=seq(dim(tab.perftest)[1]))
    tot.general <- as.data.frame.matrix(rbind(cbind(summary(data[[freqact.var]])), length(data[[freqact.var]])),
                                        row.names=seq(dim(tab.perftest)[1]))
    colnames(tot.general) <- "Total"
    row.names(tab.perftest)[length(row.names(tab.perftest))] <- "Total N"

    tab.perftest <- cbind(row.names(tab.perftest), tot.general, tab.perftest2)
    names(tab.perftest)[1] <- "Statistics"
    cat(paste0("\n Summary of frequency of actions by ", var ," - Individual level"))
    pander::pandoc.table(tab.perftest,split.tables=100)

}

####################################################################
##Frequency of a specific event in the concatenate var of Actions###
####################################################################
#' Frequency of specifics events in a variable of Actions
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
# VarActionSearch(data = cm015q01.TimeActions, action.var = "ITEM.ACTIONS",
#                 actions.search = interactive.actions)
#
#' @export
VarActionSearch <- function(data, action.var, actions.search) {

  for(i in seq(length(actions.search))){
    if(i==1){
      data.actionSearch <- stringr::str_count(data[[action.var]],actions.search[i])
    }else{
      data.actionSearch <- cbind(data.actionSearch,str_count(data[[action.var]],actions.search[i]))
    }
  }
  data.actionSearch <- as.data.frame(data.actionSearch)
  names(data.actionSearch) <- paste0("freq.", actions.search)
  #data.actionSearch$Freq.Actions.Search <-  apply(data.actionSearch, 1, sum) #row.sums
  data <- cbind(as.matrix.data.frame(data), data.actionSearch)
  return(data)
}

################################################################
##Order of a specific event in the concatenate var of Actions###
################################################################
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

#############################################
#Report: Descriptive statistics by Strategy #
#############################################
#' Report: Descriptive statistics by strategy
#'
#' This is a function that reports a descriptive analysis of the strategy and students
#' performance
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and performance
#' variables are
#' @param strategy.var A character string with the name of the strategy variable
#' @param performance.item A character string with the name of the item performance variable
#' @param performance.test A character string with the name of the test performance variable
#' @param PartialCredit Logical. It can be used when the item is partial credit score.
#'
#' @return This function returns a report with a descriptive analysis of the strategy and students
#' performance
#'
# @examples
# DescriptiveStrategy(data=cp025q01.complete, strategy.var="VOTAT1", performance.item="CP025Q01",
# performance.test= "PV1CPRO", PartialCredit=FALSE)
#
#' @export
DescriptiveStrategy <-
  function(data,
           strategy.var,
           performance.item,
           performance.test,
           PartialCredit=FALSE) {

    old <- options(warn = 0)
    options(warn = -1)

    if(PartialCredit==TRUE){

      #Frequency table (N): categorical variables with partialCredit: only accepts options: 0, 0.5 and 1
      tab.freqitem <- as.data.frame(table(data[, performance.item]))
      for (i in seq(length(tab.freqitem$Var1))) {
        if (i == 1 & tab.freqitem$Var1[i]!=0.5) {
          scalename2 <- tab.freqitem$Var1[i]
          n2 <- tab.freqitem$Freq[i]
          total2 <- tab.freqitem$Freq[i]
          perc2 <-sprintf("%.0f%%", 100 * n2/sum(tab.freqitem$Freq))
        } else if(tab.freqitem$Var1[i]==0.5){
          i=i+1
          scalename2 <- paste0(scalename2, "|", tab.freqitem$Var1[i])
          n2 <- paste0(n2, "|", tab.freqitem$Freq[i-1] + tab.freqitem$Freq[i])
          total2 <- total2 + tab.freqitem$Freq[i-1] + tab.freqitem$Freq[i]
          perc2 <- paste0(perc2, "|", sprintf("%.0f%%", 100 * (tab.freqitem$Freq[i-1]*0.5+tab.freqitem$Freq[i])/sum(tab.freqitem$Freq)))
        }
      }

    }else{

       tab.freqitem <- as.data.frame(table(data[, performance.item]))
       for (i in seq(length(tab.freqitem$Var1))) {
         if (i == 1) {
           scalename2 <- tab.freqitem$Var1[i]
           n2 <- tab.freqitem$Freq[i]
           total2 <- tab.freqitem$Freq[i]
           perc2 <-sprintf("%.0f%%", 100 * n2/sum(tab.freqitem$Freq))
         } else{
           scalename2 <- paste0(scalename2, "|", tab.freqitem$Var1[i])
           n2 <- paste0(n2, "|", tab.freqitem$Freq[i])
           total2 <- total2 + tab.freqitem$Freq[i]
           perc2 <- paste0(perc2, "|", sprintf("%.0f%%", 100 * tab.freqitem$Freq[i]/sum(tab.freqitem$Freq)))

         }
       }
    }

    #Frequency table (N): categorical variables
    tab.freqvotat <- as.data.frame(table(data[, strategy.var]))
    for (i in seq(length(tab.freqvotat$Var1))) {
      if (i == 1) {
        scalename1 <- tab.freqvotat$Var1[i]
        n1 <- tab.freqvotat$Freq[i]
        total1 <- tab.freqvotat$Freq[i]
        perc1 <-sprintf("%.0f%%", 100 * tab.freqvotat$Freq[i]/sum(tab.freqvotat$Freq))
      } else{
        scalename1 <- paste0(scalename1, "|", tab.freqvotat$Var1[i])
        n1 <- paste0(n1, "|", tab.freqvotat$Freq[i])
        total1 <- total1 + tab.freqvotat$Freq[i]
        perc1 <- paste0(perc1, "|", sprintf("%.0f%%", 100 * tab.freqvotat$Freq[i]/sum(tab.freqvotat$Freq)))
      }
    }

    tab.print <-
      as.data.frame(rbind(
        cbind(paste0("Strategy: ", strategy.var), scalename1, perc1, n1, total1),
        cbind(paste0("Performance: ", performance.item), scalename2, perc2, n2, total2)
      ))

    colnames(tab.print) <- c("Construct", "Scale","Relat. Freq.", "Frequencies", "Total N")
    cat("\n Frequency table - Individual level")
    pander::pandoc.table(tab.print,split.tables=100)

    #Chi-squared test of independence
    crostab.freqvotat <- table(data[, c(strategy.var, performance.item)])
    polychoric.crostab <- psych::polychoric(data[, c(strategy.var, performance.item)])
    xtest.crostab <- chisq.test(crostab.freqvotat)

    cat("Measures of association between",strategy.var,"and",performance.item,"- Individual level")
    crostab.freqvotat <- rbind(crostab.freqvotat, apply(crostab.freqvotat, 2, sum))
    crostab.freqvotat <- cbind(crostab.freqvotat, apply(crostab.freqvotat, 1, sum))
    crostab.freqvotat <- cbind(row.names(crostab.freqvotat),as.data.frame.matrix(crostab.freqvotat,
                                                                                 row.names=seq(dim(crostab.freqvotat)[1])))
    crostab.freqvotat[, 1] <- as.character(crostab.freqvotat[, 1])
    crostab.freqvotat[dim(crostab.freqvotat)[1], 1] <- "Total"
    names(crostab.freqvotat)[1] <- paste0(strategy.var, " / ", performance.item)
    names(crostab.freqvotat)[length(names(crostab.freqvotat))] <- "Total"

    value.p <- ifelse(round(xtest.crostab$p.value,4)<0.01, "0.01",round(xtest.crostab$p.value,4))
    pander::pandoc.table(crostab.freqvotat,split.tables=100)
    cat(paste0("Chi-squared = ",round(xtest.crostab$statistic,2),", df = ",xtest.crostab$parameter,
               ", p-value < ", value.p,"\n"))
    cat(paste0("Tetrachoric/Polychoric correlation = ",round(polychoric.crostab$rho[1,2],4),"\n"))

    if(prod(dim(table(data[, c(strategy.var, performance.item)])))==4){
      phi.crostab <- phi(table(data[, c(strategy.var, performance.item)]))
      cat(paste0("Phi coefficient = ", round(phi.crostab, 4), "\n"))
    }

    #Summary table: continua variables - Summary of  PV1CPRO  by strategy
    for (j in seq(length(tab.freqvotat$Var1))) {
      data2 <- as.data.frame(data[data[, strategy.var]==as.character(tab.freqvotat$Var1[j]), performance.test])
      names(data2) <- performance.test

      if(j==1){
        tab.perftest <- as.matrix.data.frame(rbind(round(length(data2[[performance.test]]),0), round(min(data2[[performance.test]]),2) ,
                                                   round(quantile(data2[[performance.test]], probs=0.25), 2), round(median(data2[[performance.test]]), 2),
                                                   round(mean(data2[[performance.test]]), 2), round(sd(data2[[performance.test]]), 2),
                                                   round(quantile(data2[[performance.test]], probs=0.75),2),
                                                   round(max(data2[[performance.test]]), 2)))
        colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
      }else{
        tab.perftest2 <- as.matrix.data.frame(rbind(round(length(data2[[performance.test]]),0), round(min(data2[[performance.test]]),2) ,
                                                    round(quantile(data2[[performance.test]], probs=0.25), 2), round(median(data2[[performance.test]]), 2),
                                                    round(mean(data2[[performance.test]]), 2), round(sd(data2[[performance.test]]), 2),
                                                    round(quantile(data2[[performance.test]], probs=0.75),2),
                                                    round(max(data2[[performance.test]]), 2)))
        tab.perftest <- cbind(tab.perftest, tab.perftest2)
        colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
      }
    }

    tot.general <- as.matrix.data.frame(rbind(round(length(data[[performance.test]]),0), round(min(data[[performance.test]]),2) ,
                                              round(quantile(data[[performance.test]], probs=0.25), 2), round(median(data[[performance.test]]), 2),
                                              round(mean(data[[performance.test]]), 2), round(sd(data[[performance.test]]), 2),
                                              round(quantile(data[[performance.test]], probs=0.75),2),
                                              round(max(data[[performance.test]]), 2)))

    tab.perftest <- cbind(c("Total N", "Min", "1st.Qu", "Median", "Mean", "SD", "3st.Qu", "Max"), tot.general, tab.perftest)
    colnames(tab.perftest) <- c("Statistics","Total", colnames(tab.perftest)[3:length(colnames(tab.perftest))])

    cat("\n Summary of",performance.test,"by",strategy.var,"- Individual level")
    pander::pandoc.table(tab.perftest,split.tables=100)
    polyserial.cor <- psych::polyserial(x=as.matrix(data[, performance.test]),
                                        y=as.matrix(data[, strategy.var]))
    cat(paste0("Biserial/Polyserial correlation = ",round(polyserial.cor[1],4),"\n"))

    # for (j in seq(length(tab.freqvotat$Var1))) {
    #   if(j==1){
    #     tab.perftest <- cbind(summary(data[data[, strategy.var]==as.character(tab.freqvotat$Var1[j]),
    #                                        performance.test]))
    #     colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
    #     tab.perftest <- rbind(tab.perftest, tab.freqvotat$Freq[j])
    #   }else{
    #     tab.perftest2 <- cbind(summary(data[data[, strategy.var]==as.character(tab.freqvotat$Var1[j]),
    #                                         performance.test]))
    #     tab.perftest2 <- rbind(tab.perftest2, tab.freqvotat$Freq[j])
    #     tab.perftest <-cbind(tab.perftest,tab.perftest2)
    #     colnames(tab.perftest)[j] <- as.character(tab.freqvotat$Var1[j])
    #   }
    # }
    # tab.perftest <- cbind(row.names(tab.perftest),as.data.frame.matrix(tab.perftest,
    #                                                                    row.names=seq(dim(tab.perftest)[1])))
    # names(tab.perftest)[1] <- paste0("Statistics / ", strategy.var)
    # tab.perftest[, 1] <- as.character(tab.perftest[, 1])
    # tab.perftest[dim(tab.perftest)[1], 1] <- "Total N"
    #
    # cat("\n Summary of",performance.test,"by",strategy.var,"- Individual level")
    # pander::pandoc.table(tab.perftest,split.tables=100)
    # polyserial.cor <- psych::polyserial(x=as.matrix(as.numeric(data[, performance.test])),
    #                              y=as.matrix(data[, strategy.var]))
    # cat(paste0("Biserial/Polyserial correlation = ",round(polyserial.cor[1],4),"\n"))

    on.exit(options(old), add = TRUE)
  }

########################################
#Plot: Boxplot PV1Math by Strategy var #
########################################
BoxplotStrategybyPerformance <-
  function(data,
           strategy.var,
           performance.test,
           ylab.text,
           xlab.text) {
    ggplot2::ggplot(data, aes(x = as.factor(strategy.var), y = performance.test)) +
      geom_boxplot() +
      theme_bw() +
      scale_x_discrete(name=xlab.text) #+
      #scale_y_continuous(name=ylab.text)
  }

###################################################
#Data: Percentage in arcsine values x PISA scores #
###################################################
#' Data: Percentage in arcsine values x PISA scores by Country
#'
#' This is a function that calculates the percentage in arcsine and plots it against
#' the PISA scores
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and performance
#' variables are
#' @param strategy.var A string with the name of the strategy variable. It is "quo()" type.
#' @param performance.test A string with the name of the test performance variable. It is "quo()" type.
#' @param country.id  A string with the name of the countries variable. It is "quo()" type.
#' @param plotARcSine Logical. Default is "TRUE".
#' @param ylab.text A character string giving the text of the y-axis in the plot
#' @param xlab.text A character string giving the text of the x-axis in the plot
#' @param ylim.vector A numeric vector with the limits of the y-axis in the plot
#' @param xlim.vector A numeric vector with the limits of the x-axis in the plot
#'
#' @return This function returns a data frame and a plot
#'
# @examples
# DataArcSinebyPerformance(data= cp025q01.complete, strategy.var= quo(VOTAT1),
# performance.test=quo(PV1CPRO), country.id=quo(cnt), plotARcSine=T,
# ylab.text= 'PV1CPRO', xlab.text='VOTAT (percentage transformed in arcsine values)',
# ylim.vector=c(350,600), xlim.vector=c(0,1.4))
#'
#' @export
DataArcSinebyPerformance <-
  function(data,
           strategy.var,
           performance.test,
           country.id) {

    country.id <- rlang::enquo(country.id)
    strategy.var <- rlang::enquo(strategy.var)
    performance.test <- rlang::enquo(performance.test)

    arcsine.PercentInteraction <-  data %>%
      dplyr::group_by(!!country.id) %>%
      dplyr::summarise(PercentInteraction = mean(!!strategy.var),
                ArcsinePercent = asin(sqrt(PercentInteraction)),
                MeanPV = mean(!!performance.test))

    return(arcsine.PercentInteraction)
  }

###################################################
#Plot: Percentage in arcsine values x PISA scores #
###################################################
#' Plot: Percentage in arcsine values x PISA scores by Country
#'
#' This is a function that calculates the percentage in arcsine and plots it against
#' the PISA scores
#'
#' @param data A \code{matrix} or \code{data.frame} where the 'strategy.var' and performance
#' variables are
#' @param strategy.var A string with the name of the strategy variable. It is "quo()" type.
#' @param performance.test A string with the name of the test performance variable. It is "quo()" type.
#' @param country.id  A string with the name of the countries variable. It is "quo()" type.
#' @param plotARcSine Logical. Default is "TRUE".
#' @param ylab.text A character string giving the text of the y-axis in the plot
#' @param xlab.text A character string giving the text of the x-axis in the plot
#' @param ylim.vector A numeric vector with the limits of the y-axis in the plot
#' @param xlim.vector A numeric vector with the limits of the x-axis in the plot
#'
#' @return This function returns a data frame and a plot
#'
# @examples
# DataArcSinebyPerformance(data= cp025q01.complete, strategy.var= quo(VOTAT1),
# performance.test=quo(PV1CPRO), country.id=quo(cnt), plotARcSine=T,
# ylab.text= 'PV1CPRO', xlab.text='VOTAT (percentage transformed in arcsine values)',
# ylim.vector=c(350,600), xlim.vector=c(0,1.4))
#'
#' @export
ScatterPlotbyPerformance <-
  function(data,
           strategy.summary,
           performance.mean,
           country.id,
           ylab.text,
           xlab.text,
           ylim.vector,
           xlim.vector) {

      plot(data[[strategy.summary]], data[[performance.mean]],
           xlab = xlab.text , ylab = ylab.text, ylim=ylim.vector, xlim = xlim.vector)
      text(data[[strategy.summary]], data[[performance.mean]],
           labels = data[[country.id]], cex=0.6, pos=3)
  }


###########################################
##Plot Strategy by PV1 ###
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
PlotStrategybyCatPerformance <- function(data, strategy.var, categ.var, namexlab, nameylab) {

  #must be factor!!
  #data[[categ.var]] <- as.factor(data[[categ.var]]) 
  #data[[strategy.var]] <- as.factor(data[[strategy.var]]) 
  
  strategy.var <- rlang::enquo(strategy.var)
  categ.var <- rlang::enquo(categ.var)

  aggregate.data <-  data %>%
    dplyr::group_by(!!strategy.var, !!categ.var) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(!!categ.var) %>%
    dplyr::mutate(freq = 100* n / sum(n))
  
  aggregate.data <- as.data.frame(aggregate.data)

    ggplot(data=aggregate.data, aes_string(x=names(aggregate.data)[2], y=names(aggregate.data)[4], fill=names(aggregate.data)[1])) +
      geom_bar(stat="identity", position=position_dodge())+ scale_fill_grey() +
      theme(axis.text.y = element_blank()) +
      geom_text(aes(label=paste(n, paste0(" (", round(freq,0), "%)"), sep='\n')), vjust=1.1, color="white",
                position = position_dodge(0.9), size=3.5) +
      xlab(namexlab) + ylab(nameylab)
}


