#' @title Print a tab object
#' @description Print the results of calculating a frequency table
#' @param x An object of class \code{tab}
#' @param ... Parameters passed to the print function
#' @return NULL
#' @examples
#' \dontrun{
#' frequency <- tab(venues, type, sort = TRUE, na.rm = FALSE)
#' print(frequency)
#' }
#' @rdname print.tab
#' @export

print.tab <- function(df, ...) {
  if(!inherits(df, "tab")) stop("Must be class 'tab'")
  digits <- attr(df, "digits")
  df$percent = paste(as.character(round(df$percent, digits)),
                     "%", sep = "")
  if (length(df) == 5)
    df$cum_percent = paste(as.character(round(df$cum_percent, digits)),
                           "%", sep = "")
  print.data.frame(df, row.names=FALSE, ...)
}

