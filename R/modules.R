#' Module 0: Data preparation
#' @name m0
#' @export
m0 <- modules::module({
    export("CleanActions", "ConcatActions", "DataActionsbyID", "ImportSPSS",
           "RangeNumberActionsbyVar", "TrimVar")
    expose("R/CleanActions_m0.R")
    expose("R/ConcatActions_m0.R")
    expose("R/DataActionsbyID_m0.R")
    expose("R/ImportSPSS_m0.R")
    expose("R/RangeNumberActionsbyVar_m0.R")
    expose("R/TrimVar_m0.R")
})

#' Module 1: Time
#' @name m1
#' @export
m1 <- modules::module({
    export("NumericTimeVar", "PlotTimeonTaskbyVar", "RangeTimeonTaskbyVar",
           "SummaryTOTbyVar", "TOTVar", "VarTimebyID")
    expose("R/NumericTimeVar_m1.R")
    expose("R/PlotTimeonTaskbyVar_m1.R")
    expose("R/RangeTimeonTaskbyVar_m1.R")
    expose("R/SummaryTOTbyVar_m1.R")
    expose("R/TOTVar_m1.R")
    expose("R/VarTimebyID_m1.R")
})

#' Module 2: Actions (cognitive related)
#' @name m2
#' @export
m2 <- modules::module({
    export("BoxplotStrategybyPerformance", "DataArcSinebyPerformance",
           "DescriptiveStrategy", "FreqActionsSummary",
           "PlotStrategybyCatPerformance", "ScatterPlotbyPerformance",
           "VarActionPosition", "VarActionSearch")
    expose("R/BoxplotStrategybyPerformance_m2.R")
    expose("R/DataArcSinebyPerformance_m2.R")
    expose("R/DescriptiveStrategy_m2.R")
    expose("R/FreqActionsSummary_m2.R")
    expose("R/PlotStrategybyCatPerformance_m2.R")
    expose("R/ScatterPlotbyPerformance_m2.R")
    expose("R/VarActionPosition_m2.R")
    expose("R/VarActionSearch_m2.R")
})