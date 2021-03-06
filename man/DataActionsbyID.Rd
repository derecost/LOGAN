% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataActionsbyID_m0.R
\name{DataActionsbyID}
\alias{DataActionsbyID}
\title{Wide format dataset with the sequence of actions by ID}
\usage{
DataActionsbyID(data, id.var, event.var, name.var.action)
}
\arguments{
\item{data}{A \code{matrix} or \code{data.frame} where the 'event.type'
variable is}

\item{id.var}{a vector with the individuals identification. It is a
\code{quo()} type.}

\item{event.var}{a vector with the cleaned concatenate events. See
\code{CleanActions} function.}

\item{name.var.action}{A character string that will name the new variable of
events}
}
\value{
This function returns a \code{data.frame} with the only one entry by
  individual identification and a new 'action.var' variable.
}
\description{
This is a function that translates a long to wide format dataset.
}
\examples{
# Data preparation
df <- cp025q01
df$id <- paste(df[, 1], df[, 2], df[, 3], sep = "-")
df <- m0$TrimVar(df, c("event", "event_type", "diag_state"))
df <- m0$ConcatActions(df, c(rlang::quo(event), rlang::quo(event_type)))
df <- m0$CleanActions(df, event.type, c("ACER_EVENT_" = ""))

# Function demonstration
m0$DataActionsbyID(df, id, new.event.type, "actions")

}
