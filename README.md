![statfunctions](graphs.png)

# qacr

<!-- badges: start -->
<!-- badges: end -->

The goal of **qacr** is to provide convenient statistical results for data science students and practitioners.

Why **qacr**? This package is created and maintained by advanced undergradutes in the [Quantitative Analysis Center](http://qac.wesleyan.edu) (QAC) at Wesleyan University. It just seemed appropriate.

## Installation

You can install this package with the following code:

``` r
if(!require(devtools)){
   install.packages("devtools")
}
devtools::install_github("rkabacoff/qacr")
```

## Example

Here are some basic examples demonstrating a few of these functions. Each function has options allowing for customized output.

``` r
library(qacr)

# summarize a data frame
contents(cars74)


# obtain descriptive statistics on a quantitative variable 
# for each level of a grouping variable
dstats(cars74, mpg, cyl)


# tabulate a categorical variable
tab(cars74, cyl)

# cross-tabulate two categorical variables
crosstab(cars74, cyl, gear)

# plot the distributions of quantitative and categorical variables
histograms(cars74)
densities(cars74)
barcharts(cars74)

# plot a correlation matrix among quantitative variables
corplot(cars74)

# visualize a data frame
dfPlot(coffee)
```

