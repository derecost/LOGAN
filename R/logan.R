#' @title LOGAN: Log File Analysis in International Large-scale Assessments
#'
#' @description This package enables users to handle the dataset cleaning for
#'   conducting specific analyses with the log files from two international
#'   educational assessments: the Programme of International Student Assessment
#'   (PISA) and the Programme for International Assessment of Adult Competencies
#'   (PIAAC).
#'
#' @section LOGAN functions: The LOGAN functions The LOGAN functions are
#'   organized in modules, so to call a function you must prefix it with, e.g.,
#'   `m0$`, where "m0" is the module to which a certain function pertains.
#'
#'   What follows is a list of Functions organized per module:
#'
#'   Module 0:
#'   \itemize{
#'       \item{CleanActions}{}
#'       \item{ConcatActions}{}
#'       \item{DataActionsbyID}{}
#'       \item{ImportSPSS}{}
#'       \item{RangeNumberActionsbyVar}{}
#'       \item{TrimVar}{}
#'   }
#'   Module 1:
#'   \itemize{
#'       \item{NumericTimeVar}{}
#'       \item{PlotTimeonTaskbyVar}{}
#'       \item{RangeTimeonTaskbyVar}{}
#'       \item{SummaryTOTbyVar}{}
#'       \item{TOTVar}{}
#'       \item{VarTimebyID}{}
#'   }
#'   Module 2:
#'   \itemize{
#'       \item{BoxplotStrategybyPerformance}{}
#'       \item{DataArcSinebyPerformance}{}
#'       \item{DescriptiveStrategy}{}
#'       \item{FreqActionsSummary}{}
#'       \item{PlotStrategybyCatPerformance}{}
#'       \item{ScatterPlotbyPerformance}{}
#'       \item{VarActionPosition}{}
#'       \item{VarActionSearch}{}
#'   }
#' @docType package
#' @name LOGAN
#' @author \itemize{ \item{Denise Reis Costa [aut, cre]}, \item{Waldir Leoncio
#'   Netto [aut]}}
NULL
