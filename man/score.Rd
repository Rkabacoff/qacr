% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score.R
\name{score}
\alias{score}
\title{Factor and component scores}
\usage{
score(data, model)
}
\arguments{
\item{data}{the data frame used to generate a PCA or FA model.}

\item{model}{the model generated by the \code{PCA} or \code{FA} functions.}
}
\value{
the data frame with component or factor scores appended.
}
\description{
\code{score} calculates principal component or factor scores
based on the model generated by \code{\link{PCA}} and \code{\link{FA}}
functions
}
\details{
The \code{score} function adds component or factor scores to a data frame. Scores
are calculated using the regression method.
}
\examples{
ratings <- read.csv("https://www.promptcloud.com/wp-content/uploads/2017/02/EFA.csv")
fit.pca <- PCA(ratings, nfactors=2, rotate="varimax")
newdf <- score(ratings, fit.pca)
head(newdf)
}
\seealso{
\link{PCA}, \link{FA}.
}
