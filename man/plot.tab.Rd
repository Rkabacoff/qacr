% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.tab.R
\name{plot.tab}
\alias{plot.tab}
\title{Plot a tab object}
\usage{
\method{plot}{tab}(x, fill = "deepskyblue2", size = 3.5, ...)
}
\arguments{
\item{x}{An object of class \code{tab}}

\item{fill}{Fill color for bars}

\item{size}{numeric. Size of bar text labels.}

\item{...}{Parameters passed to a function}
}
\value{
a ggplot2 graph
}
\description{
Plot a frequency or cumulative frequency table
}
\examples{
tbl1 <- tab(cars74, carb)
plot(tbl1)

tbl2 <- tab(cars74, carb, sort = TRUE)
plot(tbl2)

tbl3 <- tab(cars74, carb, cum=TRUE)
plot(tbl3)
}
