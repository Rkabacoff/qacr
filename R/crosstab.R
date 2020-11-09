#' @title Two-way frequency table
#' @description This function creates a two way frequency table.
#' @param data data frame
#' @param rowvar row factor (unquoted)
#' @param colvar column factor (unquoted)
#' @param type statistics to print. Options are \code{"freq"}, \code{"cellpercent"},
#' \code{"rowpercent"}, or \code{"colpercent"} for frequencies, cell percents,
#' row percents, or column percents).
#' Default: \code{"freq"}.
#' @param total if TRUE, includes total percents, Default: TRUE
#' @param na.rm if TRUE, deletes cases with missing values, Default: TRUE
#' @param digits number of decimal digits to report for percents, Default: 2
#' @param plot logical. If \code{TRUE} generate stacked bar chart. Default: FALSE.
#' @return a list with 9 components:
#' \itemize{
#' \item{\code{tbl}}{ (table). Table of frequencies}
#' \item{\code{cellPcts}}{ (table). Table of cell percents}
#' \item{\code{rowPcts}}{ (table). Table of row percents}
#' \item{\code{colPcts}}{ (table). Table of column percents}
#' \item{\code{type}}{ (character). Type of table to print}
#' \item{\code{total}}{ (logical). If \code{TRUE}, print row and or column totals}
#' \item{\code{digits}}{ (numeric). number of digits to print}
#' \item{\code{rowname}}{ (character). Row variable name}
#' \item{\code{colname}}{ (character). Column variable name}
#' }
#'
#' @details This function calculates and prints a two way frequency table.
#' Given a data frame, a row factor, a column factor, and a type (frequencies, cell percents,
#' row percents, or column percents) the function prints a table with labeled rows
#' and columns, frequencies or percents with the percentage sign depending on
#' type, a level labeled <NA> if na.rm = FALSE, and a level labeled Total if
#' total = TRUE.
#' @examples
#' crosstab(mtcars, cyl, gear)
#'
#' crosstab(mtcars, cyl, gear, type="cellpercent")
#'
#' crosstab(mtcars, cyl, gear, type="colpercent")
#'
#' crosstab(mtcars, cyl, gear, type="rowpercent")
#' @rdname crosstab
#' @export

crosstab <- function(data, rowvar, colvar,
                     type=c("freq", "cellpercent",
                            "rowpercent", "colpercent"),
                     total=TRUE,
                     na.rm=TRUE,
                     digits=2,
                     plot=FALSE){

  rowvar <- deparse(substitute(rowvar))
  colvar <- deparse(substitute(colvar))
  type <- match.arg(type)
  if (na.rm){
    useNA <- "no"
  } else {
    useNA <- "always"
  }
  tbl <- table(data[[rowvar]], data[[colvar]], useNA=useNA)
  names(dimnames(tbl)) <- c(rowvar, colvar)

  # percents
  tbl_cellpct <- prop.table(tbl)
  tbl_rowpct <- prop.table(tbl, 1)
  tbl_colpct <- prop.table(tbl, 2)

  # package results
  results <- list(counts = tbl,
                  cellPcts = tbl_cellpct,
                  rowPcts = tbl_rowpct,
                  colPcts = tbl_colpct,
                  type=type,
                  total=total,
                  digits=digits,
                  rowname = rowvar,
                  colname = colvar)


  class(results) <- c("crosstab", "list")
  if (plot) {
    x <- plot(results)
    print(x)
  }
  return(results)
}
