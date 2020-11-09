#' @title Print a crosstab object
#' @description This function prints the results of a calculated two-way
#' frequency table.
#' @param x An object of class \code{crosstab}
#' @return NULL
#' @examples
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "freq", digits = 2)
#' print(mycrosstab)
#'
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "rowpercent", digits = 3)
#' print(mycrosstab)
#' @rdname print.crosstab
#' @export
print.crosstab <- function(x, ...) {
  if(!inherits(x, "crosstab")) stop("Object must be of type crosstab")
  if (x$type == "freq"){
    tb <- x$counts
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
    print.table(tb, right=TRUE, justify="right")
  }

  if (x$type == "cellpercent"){
    tb <- x$cellPcts
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))
    print.table(tb, right=TRUE, justify="right")
  }

  if (x$type == "rowpercent"){
    tb <- x$rowPcts
    if (x$total){
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))
    print.table(tb, right=TRUE, justify="right")
  }

  if (x$type == "colpercent"){
    tb <- x$colPcts
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))
    print.table(tb, right=TRUE, justify="right")
  }
}
