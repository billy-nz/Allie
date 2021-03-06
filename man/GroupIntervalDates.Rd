% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GroupIntervalDates.R
\name{GroupIntervalDates}
\alias{GroupIntervalDates}
\title{Group overlapping time intervals and provide a sequential index}
\usage{
GroupIntervalDates(dat, start, end, by, ...)
}
\arguments{
\item{dat}{an data.frame or data.table containing input data (see details)}

\item{start}{date variable in the dataset representing the start of an observation}

\item{end}{date variable in the dataset representing the end of an observation}

\item{by}{person or group id}

\item{...}{further arguments (see values)}
}
\value{
Returns a sequential vector of the same length as input x. Two further \code{...} arguments are possible:
\item{lag}{specify the lag days; default is 1}
\item{zero.index}{specify whether the indexing sequence should start at 0 instead of 1; default is FALSE}
}
\description{
\code{GroupIntervalDates} returns a sequential vector of the same length as input x. Overlapping time intervals are groupped, sequenced, and indexed starting with 1.
If grouping implemented, sequence begins will begin with 1 for each group. Each obversation requires a start and end date to form the time interval. 
Two modalities of grouping are available: consecutive and overlapping. See details.
}
\examples{
DATA$lag1   <- GroupIntervalDates(dat=DATA, start=start_date, end=end_date, by=UID)
DATA$lag0   <- GroupIntervalDates(dat=DATA, start=start_date, end=end_date, by=UID, lag = 0)
DATA$python <- GroupIntervalDates(dat=DATA, start=start_date, end=end_date, by=UID, zero.index = TRUE)
                                                         
}
