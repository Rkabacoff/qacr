## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(qacr)

## ----message=FALSE, warning=FALSE----------------------------------------
data(iris)

str(iris)

## ----message=FALSE, warning=FALSE----------------------------------------
df_summary(iris)

## ------------------------------------------------------------------------
summary(airquality)

## ------------------------------------------------------------------------
df_summary(airquality)

