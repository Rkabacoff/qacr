% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histograms.R
\name{histograms}
\alias{histograms}
\title{Histograms}
\usage{
histograms(data, fill = "deepskyblue2", color = "white", bins = 30)
}
\arguments{
\item{data}{data frame}

\item{fill}{fill color for histogram bars}

\item{color}{border color for histogram bars}

\item{bins}{number of bins (bars) for the histograms}
}
\value{
a ggplot graph
}
\description{
Create histograms for all quantitative variables in a data frame.
}
\details{
The \code{histograms} function will only plot quantitative variables from
a data frame. Categorical variables are ignored.
}
\examples{
histograms(cars74)
histograms(cars74, bins=15, fill="darkred")
}
