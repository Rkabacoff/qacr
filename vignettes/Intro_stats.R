## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, eval=TRUE, error=FALSE, warning=FALSE, message=FALSE---------
library(qacr)

## ----include=TRUE-------------------------------------------------------------
dstats(oil,operator)

## ----include=TRUE-------------------------------------------------------------
dstats(oil,operator, stats = c("median"))

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, stats = c("median", "mean", "sd"))

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, Group, Contour, Depth)

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, Group, Contour, stats=c("mean", "median", "sd"))

## ----include=TRUE-------------------------------------------------------------
#custom statistic
my_n <- function(xs) length(xs)

#calling the custom statistic
dstats(soils, pH, stats = c("my_n"))

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, Group, Contour, Depth, stats=c("my_n","mean"))

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, Depth, stats=c("mean","median","sd"),  na.rm=FALSE)

## ----include=TRUE-------------------------------------------------------------
dstats(soils, pH, Depth, stats=c("mean","median","sd"), na.rm=FALSE, digits=3)

