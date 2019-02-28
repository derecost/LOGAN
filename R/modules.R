#' Module 0
#' @name Module0
#' @export
m0 <- modules::module({
  expose("R/m0/CleanActions_m0.R")
  expose("R/m0/ConcatActions_m0.R")
  expose("R/m0/DataActionsbyID_m0.R")
  expose("R/m0/ImportSPSS_m0.R")
  expose("R/m0/RangeNumberActionsbyVar_m0.R")
  expose("R/m0/TrimVar_m0.R")
})

#' Module 1
#' @name Module1
#' @export
m1 <- modules::module({
    expose("R/m1/NumericTimeVar_m1.R")
    expose("R/m1/PlotTimeonTaskbyVar_m1.R")
    expose("R/m1/RangeTimeonTaskbyVar_m1.R")
    expose("R/m1/SummaryTOTbyVar_m1.R")
    expose("R/m1/TOTVar_m1.R")
    expose("R/m1/VarTimebyID_m1.R")
})

#' Module 2
#' @name Module2
#' @export
m2 <- modules::module({
    expose("R/m2/BoxplotStrategybyPerformance_m2.R")
    expose("R/m2/DataArcSinebyPerformance_m2.R")
    expose("R/m2/DescriptiveStrategy_m2.R")
    expose("R/m2/FreqActionsSummary_m2.R")
    expose("R/m2/PlotStrategybyCatPerformance_m2.R")
    expose("R/m2/ScatterPlotbyPerformance_m2.R")
    expose("R/m2/VarActionPosition_m2.R")
    expose("R/m2/VarActionSearch_m2.R")
})