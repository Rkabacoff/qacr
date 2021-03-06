% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heart.R
\docType{data}
\name{heart}
\alias{heart}
\title{Heart disease}
\format{
A data frame with 303 rows and 14 variables:
\describe{
  \item{\code{age}}{integer. age in years.}
  \item{\code{sex}}{factor with 2 levels. male, female.}
  \item{\code{cp}}{factor with 4 levels.  chest pain type (typical angina,
  atypical angina, non-anginal pain, asymptomatic).}
  \item{\code{testbps}}{integer. resting blood pressure in mm Hg on admission to the hospital.}
  \item{\code{chol}}{integer. serum cholestoral in mg/dl.}
  \item{\code{fbs}}{integer. fasting blood sugar > 120 mg/dl (1 = true; 0 = false).}
  \item{\code{restecg}}{factor with 3 levels. resting electrocardiographic results (normal,
  having ST-T wave abnormality,left ventricular hypertrophy).}
  \item{\code{thalach}}{integer. maximum heart rate achieved.}
  \item{\code{exang}}{integer. exercise induced angina (1 = yes; 0 = no).}
  \item{\code{oldpeak}}{double. ST depression induced by exercise relative to rest.}
  \item{\code{slope}}{factor with 3 levels. the slope of the peak exercise ST segment (upslope, flat, downsloping).}
  \item{\code{ca}}{integer. number of major vessels (0-3) colored by flourosopy.}
  \item{\code{thal}}{factor with 3 levels. normal, fixed defect, reversable defect.}
  \item{\code{disease}}{factor with 2 levels. heart disease (yes, no). This is the outcome variable of interest.}
}
}
\source{
Data obtained from the \href{https://archive.ics.uci.edu/ml/datasets/Heart+Disease}{UCI Machine Learning Repository}.

Creators:

\enumerate{
\item Hungarian Institute of Cardiology. Budapest: Andras Janosi, M.D.
\item University Hospital, Zurich, Switzerland: William Steinbrunn, M.D.
\item University Hospital, Basel, Switzerland: Matthias Pfisterer, M.D.
\item V.A. Medical Center, Long Beach and Cleveland Clinic Foundation: Robert Detrano, M.D., Ph.D.
}

Donor: David W. Aha (aha '@' ics.uci.edu) (714) 856-8779
}
\usage{
heart
}
\description{
Predict heart disease from lab data
}
\examples{
summary(heart)
}
\keyword{datasets}
