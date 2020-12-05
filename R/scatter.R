#' @title Scatterplot
#' @description Create a scatter plot between two
#' quantitative variables.
#' @param data data frame
#' @param x quantitative predictor variable
#' @param y quantitative response variable
#' @param fit logical. If \code{TRUE}, add a line
#' of best fit with 95 percent confidence interval.
#' @param jitter logical. If \code{TRUE}, points
#' are jittered
#' @param color character. color for points
#' @return a ggplot graph
#' @details
#' The \code{scatter} function creates a scatter plot with
#' a linear line of best fit. The equation and R-squared,
#' along with level of significance.
#'

#' @examples
#' scatter(cars74, wt, mpg)
#'
#' @rdname scatter
#' @import ggplot2
#' @import dplyr
#' @export
scatter <- function(data, x, y, fit=TRUE,
                    jitter=FALSE,
                    color="deepskyblue2"){

  y <- enquo(y)
  x <- enquo(x)
  df <- data %>% select(!!x, !!y)
  R2 <- round(stats::cor(df[1], df[2])^2, 2)
  p <- ggplot(data, aes(x = !!x, y = !!y)) +
    theme_minimal()
  title <- paste("Scatterplot of", as_label(x),
                 "with", as_label(y))
  if (jitter) {
    p <- p + geom_jitter(color=color)
  } else {
    p <- p + geom_point(color=color)
  }

  p <- p + labs(title=title)

  if (fit){
    p <- p + geom_smooth(method="lm", color="steelblue")
    p <- p +
      labs(caption=paste("R-squared=", R2))
  }

  p <- p + labs(title=title)
  return(p)
}
