#' @title Correlation matrix plot
#' @description Create a correlation matrix for all quantitative variables in a data frame.
#' @param data data frame
#' @param sort logical. If \code{TRUE}, reorder variables to place variables
#' with similar correlation patterns together.
#' @param lab_size size for correlation coefficient labels (default=4).
#' @return a ggplot graph
#' @details
#' The \code{cor_plot} function will only select quantitative variables from
#' a data frame. Categorical variables are ignored.
#'
#' The correlation matrix is presented as a lower triangle matrix.
#' Correlations are Pearson Product Moment correlation coefficients.
#'
#' Missing values are deleted in listwise fashion.
#'
#' @note
#' This function is a wrapper for the \code{\link[ggcorrplot:ggcorrplot]{ggcorrplot}} function.
#' @examples
#' cor_plot(cars74)
#' cor_plot(cars74, sort=TRUE)
#' cor_plot(coffee, lab_size=2, sort=TRUE)
#' @rdname cor_plot
#' @import ggcorrplot
#' @import ggplot2
#' @export
cor_plot <- function(data, sort=FALSE, lab_size=4){
  index <- sapply(data, is.numeric)
  qdata <- data[index]
  # bind global variables to keep check from warning

  r <- stats::cor(qdata, use="complete.obs")
  p <- ggcorrplot(r,
                  hc.order = sort,
                  colors = c("red", "white", "blue"),
                  type = "lower",
                  lab = TRUE,
                  lab_size=lab_size)
  p <- p + labs(title = "Correlation Matrix")
  return(p)
}
