## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(qacr)

## -----------------------------------------------------------------------------
crosstab(mtcars, cyl, gear)

## -----------------------------------------------------------------------------
crosstab(mtcars, cyl, gear, type = "rowpercent")

## -----------------------------------------------------------------------------
crosstab(mtcars, cyl, gear, type = "colpercent")

## -----------------------------------------------------------------------------
result <- crosstab(mtcars, cyl, gear)
result$tbl
result$cellPcts
result$rowPcts
result$colPcts


