#' Plot a factor solution
#'
#' This function plots the pattern matrix for a principal components or
#' common factor analysis solution
#'
#' @param x an object of class \code{factorAnalysis} produced by the \code{\link{PCA}}
#' or \code{\link{FA}} functions.
#' @param sort logical. If \code{TRUE}, sort the pattern matrix.
#' @param type generate a table plot (\code{"table"}) or bar plot (\code{"bar"}).
#' @param ... not currently used
#' @export
#' @return a ggplot2 graph
#' @import ggplot2
#' @importFrom grDevices dev.off pdf
#' @importFrom stats prcomp var
#'
#' @seealso \link{PCA} and \link{FA}.
#' @examples
#' fit.pca <- PCA(Harman74.cor$cov, nfactors=4, rotate="varimax")
#' plot(fit.pca)
#' plot(fit.pca, type="bar")
plot.factorAnalysis <- function(x, sort=TRUE, type=c("table", "bar"), ...) {
  if (!inherits(x, "factorAnalysis")){
    stop("x must be of class factorAnalysis", call. = FALSE)
  }

  type <- match.arg(type)
  x <- x$loadings
  x <- as.matrix(x)
  factors <- ncol(x)

  if (sort == TRUE) {
    x <- unclass(x)
    p <- nrow(x)
    mx <- max.col(abs(x))
    ind <- cbind(1L:p, mx)
    mx[abs(x[ind]) < 0.5] <- factors + 1
    x <- x[order(mx, 1L:p), ]
  }

  x <- as.data.frame(x)
  x$var <- row.names(x)

  facorder <- rev(x$var)
  x <- tidyr::gather(x, key="key", value="value", 1:factors)
  x$var <- factor(x$var, levels=facorder)

  if (type == "bar"){
    p <- ggplot(x, aes(.data[["var"]],
                       abs(.data[["value"]]),
                       fill=.data[["value"]])) +
      facet_wrap(~ .data[["key"]], nrow=1) + #place the factors in separate facets
      geom_bar(stat="identity") + #make the bars
      coord_flip() + #flip the axes so the test names can be horizontal
      #define the fill color gradient: blue=positive, red=negative
      scale_fill_gradient2(name = "Loading",
                           high = "blue", mid = "white", low = "red",
                           midpoint=0, guide=F) +
      ylab("Loading Strength") + #improve y-axis label
      xlab("Variables")+
      theme_bw(base_size=10) #use a black-and-white theme with set font size
  }

  if (type == "table"){
    p <- ggplot(x, aes(.data[["key"]],
                       .data[["var"]])) +
      geom_tile(aes(fill = .data[["value"]])) +
      geom_text(aes(label = round(.data[["value"]], 2)), size=3) +
      scale_fill_gradientn(colors = c( "red",
                                       "white",
                                       "blue"),
                           limits = c(-1, 1)) +
      theme_minimal()+
      theme(panel.grid.major=element_blank()) +
      labs(title = "Pattern Matrix",
           fill="Coefficient",
           x = "",
           y = "")

  }

  p
}
