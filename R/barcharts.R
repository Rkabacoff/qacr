#' @title Barcharts
#' @description Create barcharts for all categorical variables in a data frame.
#' @param data data frame
#' @param fill fill color for bars
#' @param color color for bar labels
#' @param labels if \code{TRUE}, bars are labeled with percents
#' @param maxcat numeric. barcharts with more than this number of bars will not be plotted.
#' @param abbrev numeric. abbreviate bar labels to at most, this character length.
#' @return a ggplot graph
#' @examples
#' barcharts(cars74)
#' @rdname barcharts
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#' @export
barcharts <- function(data, fill="deepskyblue2", color="grey30", labels=TRUE,
                 maxcat=20, abbrev=20){


  index <- sapply(data, function(x) is.factor(x) | is.character(x)|is.logical(x))
  cdata <- data[index]

  # delete categories with more than n levels and print warning
  if (length(cdata)==0){
    stop("No variables are categorical.")
  }

  # delete categories with more than n levels and print warning
  for(i in seq_along(cdata)){
    cdata[[i]] <- as.factor(cdata[[i]])
  }

  index2 <- sapply(cdata, nlevels)
  cdata2 <- cdata[index2 <= maxcat]

  if (length(cdata2) == 0){
    stop(paste("No variables had less then", maxcat+1, "category."))
  }

  if (length(cdata2) < length(cdata)){
    cat("The following variable had more than", maxcat,
        "levels and were not graphed:\n",
        names(cdata[index2 > maxcat]))
  }

  # abbreviate level labels
  for(i in seq_along(cdata2)){
    levels(cdata2[[i]]) <- abbreviate(levels(cdata2[[i]]), minlength=abbrev)
  }


  cdata_long <- suppressWarnings(tidyr::gather(cdata2))

  require(dplyr)
  cdatl_a <- cdata_long %>% group_by(key, value) %>% summarize(n=n())
  cdatl_b <- cdatl_a %>% group_by(key) %>% summarize(tot = sum(n))

  cdata_long <- inner_join(cdatl_a, cdatl_b, by="key") %>%
    mutate(pct = n/ tot,
           pctlabel = paste0(round(pct*100), "%"))

  require(ggplot2)
  require(scales)
  p <- ggplot(data=cdata_long, aes(x=value, y=pct)) +
    geom_bar(fill=fill, stat="identity") +
    labs(x="Value", y="Percent", title="Bar charts") +
    scale_y_continuous(breaks=pretty_breaks()) +
    facet_wrap(~key, scale="free") +
    theme_bw() + coord_flip()  +
    theme(panel.grid.major.y=element_blank())
  if(labels)
    p <- p + geom_text(aes(label = pctlabel), hjust=1, size=3, color=color)
  return(p)
}
