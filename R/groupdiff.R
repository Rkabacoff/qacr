#' @title
#' Test of group differences
#'
#' @description
#' One-way analysis (ANOVA or Kruskal-Wallis Test)
#' with post-hoc comparisons and plots
#'
#' @param formula formula of the form \code{Y ~ X} where \code{Y} is the response
#' variable, and \code{X} is the categorical explanatory variable.
#' @param data a data frame.
#' @param method character. Either \code{"anova"}, or \code{"kw"} (see details).
#' @param digits Number of significant digits to print.
#' @param offset space at top of graph for post-hoc comparison letters.
#' @param horizontal logical. It \code{TRUE}, boxplots are plotted horizontally.
#' @import dplyr
#' @import ggplot2
#' @importFrom multcompView multcompLetters
#' @importFrom PMCMRplus kwAllPairsConoverTest
#' @return a list with 2 components
#' @export
#' @details
#' The \code{groupdiff} function performs one of two analyses:
#' \describe{
#' \item{\code{anova}}{A one-way analysis of variance, with TukeyHSD
#' post-hoc comparisons.}
#' \item{\code{kw}}{A Kruskal Wallis Rank Sum Test, with Conover
#' Test post-hoc comparisons.}
#' }
#' In each case, a boxplot and summary statistics are provided.
#' @seealso \link[PMCMRplus]{kwAllPairsConoverTest},
#' \link[multcompView]{multcompLetters}.
#' @examples
#' # parametric analysis
#' groupdiff(hp ~ gear, mtcars)
#'
#' # nonparametric analysis
#' groupdiff(popularity ~ vehicle_style, cardata, method="kw",
#'           offset=500, horizontal=TRUE)
groupdiff <- function(formula, data,
                      method=c("anova", "kw"),
                      digits=2,
                      offset = 5,
                      horizontal=FALSE){

  x <- as.character(formula[[3]])
  y <- as.character(formula[[2]])

  data <- na.omit(data[c(x, y)])

  data[x] <- factor(data[[x]])
  data[x] <- reorder(data[[x]], data[[y]], mean, na.rm=TRUE)
  levels(data[[x]]) <- gsub("-", " ", levels(data[[x]]))

  # plot colors
  #colors <- brewer.pal(nlevels(data[[x]]), "Set3")


  method <- match.arg(method)
  if(method == "anova"){

    # omnibus
    a <- aov(formula, data)
    fit <- unlist(summary(a))
    F <- round(fit[7], digits)
    df1 <- fit[1]
    df2 <- fit[2]
    p   <- fit[9]
    fit.result <- paste0("F(", df1, ", ", df2, ") = ", F,
                         ", p-value = ", format.pval(p, digits))

    # summary statistics
    mystats <-  data %>%
      group_by(.data[[x]]) %>%
      summarise(n = n(),
                mean = round(mean(.data[[y]], na.rm=TRUE), digits),
                sd = round(sd(.data[[y]], na.rm=TRUE), digits),
                .groups = 'drop') %>%
      as.data.frame()

    # posthoc comparisons
    tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
    Tukey.levels <- tHSD[[1]][,4]
    Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
    plot.labels <- names(Tukey.labels[['Letters']])
    plot.levels <- data.frame(plot.labels, Tukey.labels,
                              stringsAsFactors = FALSE,
                              row.names=NULL)
    plot.levels$y <- max(data[y], na.rm=TRUE) + offset

    # plot
    p <- ggplot(data, aes_string(x=x, y=y)) +
      geom_hline(yintercept=mean(data[[y]], na.rm=TRUE),
                 linetype="dashed",
                 color="darkgrey") +
      geom_boxplot(aes_string(fill=x)) +
      geom_point(data=mystats,
                 mapping=aes_string(x=x, y="mean"),
                 size=1, shape=3) +
      geom_text(data = plot.levels,
                aes(x = plot.labels,
                    y = y,
                    label = Letters)) +
      theme_minimal() +
      theme( plot.subtitle = element_text(size = 9),
             plot.caption = element_text(size=8),
             legend.position="none") +
      labs(title = "Group Differences",
           subtitle = paste("ANOVA:", fit.result),
           caption = "Boxplots that share letters are not significantly different (TukeyHSD p < 0.05).\nCrosses are group means. Dashed line is the mean value for all obs.")

    if(horizontal) p <- p + coord_flip()
    plot(p)

  }

  if(method == "kw"){

    # omnibus
    fit <- kruskal.test(formula, data)
    fit.result <- paste("Chi-squared = ",
                        round(fit$statistic, digits),
                        ", df =",
                        fit$parameter,
                        ", p-value =",
                        format.pval(fit$p.value, digits))


    # summary statistics
    mystats <-  data %>%
      group_by(.data[[x]]) %>%
      summarise(n = n(),
                median = round(median(.data[[y]], na.rm=TRUE), digits),
                mad = round(mad(.data[[y]], na.rm=TRUE), digits),
                .groups = 'drop') %>%
      as.data.frame()



    # posthoc
    phtest <- kwAllPairsConoverTest(formula, data)
    n <- ncol(phtest$p.value)
    w <- matrix(nrow=n+1, ncol=n+1)
    nms <- c(dimnames(phtest$p.value)[[2]], dimnames(phtest$p.value)[[1]][n])
    dimnames(w) <- list(nms, nms)
    w[2:nrow(w), 1:n] <- phtest$p.value

    makeSymm <- function(m) {
      m[upper.tri(m)] <- t(m)[upper.tri(m)]
      return(m)
    }
    w <- makeSymm(w)
    plot.levels <- data.frame(
      plot.labels = rownames(w),
      Letters = as.list(multcompLetters(w))$Letters,
      y = max(data[y], na.rm=TRUE) + offset
    )

    # plot
    p <- ggplot(data, aes_string(x=x, y=y)) +
      geom_hline(yintercept=median(data[[y]], na.rm=TRUE),
                 linetype="dashed",
                 color="darkgrey") +
      geom_boxplot(aes_string(fill = x)) +
      geom_text(data = plot.levels,
                aes(x = plot.labels,
                    y = y,
                    label = Letters)) +
      theme_minimal() +
      theme( plot.subtitle = element_text(size = 9),
             plot.caption = element_text(size=8),
             legend.position="none") +
      labs(title = "Group Differences",
           subtitle = paste("Kruskal-Wallis:", fit.result),
           caption = "Boxplots that share letters are not significantly different (Conover Test p < 0.05).\nDashed line is the median value for all obs.")
    if(horizontal) p <- p + coord_flip()
    plot(p)
  }

  # return results
  result <- list(result = fit.result, summarystats = mystats)
  return(result)
}


