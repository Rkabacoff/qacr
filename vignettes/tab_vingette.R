## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(qacr)


## ---- include=TRUE-------------------------------------------------------
tab(venues, status)

## ---- include=TRUE-------------------------------------------------------
tab(venues, country, sort=TRUE, total=TRUE, digits=4)

## ---- include=T----------------------------------------------------------
tab(venues, state, sort = TRUE, na.rm = TRUE, total = TRUE, maxcat=10)

## ---- include=T----------------------------------------------------------
tab(venues, state,  minp=0.05)

