#' @title Import a dataset
#' @description The \code{import} function can import data from
#' delimited text files, Excel spreadsheets, and statistical
#' packages such as SAS, SPSS, and Stata.
#' @param file datafile to import. If missing, the user is prompted to select a
#' file interactively.
#' @param ... parameters passed to the import function. See details.
#' @param quietly logical. If \code{quietly=FALSE}, the data frame is described
#' after import.
#' @return a data frame (tibble)
#' @details
#' The \code{import} function is a wrapper for the
#' \href{https://haven.tidyverse.org/}{haven},
#' \href{https://readxl.tidyverse.org/}{readxl}, and
#' \href{https://github.com/r-lib/vroom}{vroom} packages. The \code{vroom}
#' package provides one of the fastest methods for importing delimited
#' text files currently available.
#'
#' Files are imported based on their extension (\code{.sas7bdat} for SAS,
#' \code{.sav} for SPSS, \code{.dta} for Stata, and \code{.xls}
#' or \code{.xlsx} for Excel). All other file extensions are assumed to
#' belong to delimited text files (e.g., comma or tab delimited files).
#'
#' If you import a delimited text file without options, the structure
#' and delimiters are determined from the first 100 rows.You can override
#' these guesses by specifying \link[vroom:vroom]{options}
#' (e.g., \code{sep=";", na="999"}).
#' @examples
#' \dontrun{
#'
#'  # import a comma delimited file
#'  mydataframe <- import("mydata.csv")
#'
#'  # import a SAS binary datafile and describe the results
#'  mydataframe <- import("mydata.sas7bdat", quietly=FALSE)
#'
#'  # import the second worksheet of an Excel workbook
#'  mydataframe <- import("mydata.xlsx", sheet=2)
#'
#'  # prompt for a file to import
#'  mydataframe <- import()

#' }
#' @seealso
#'  \code{\link[haven]{read_sas}},
#'  \code{\link[haven]{read_dta}},
#'  \code{\link[haven]{read_spss}},
#'  \code{\link[readxl]{read_excel}},
#'  \code{\link[vroom]{vroom}}
#' @rdname import
#' @export
#' @importFrom tools file_ext
#' @importFrom haven read_sas read_stata read_spss
#' @importFrom readxl read_excel
#' @importFrom vroom vroom
#' @importFrom dplyr glimpse
import <- function(file, quietly=TRUE, ...){

  # if no file specified, prompt user

  if(missing(file))
    file <- file.choose()
    print(paste("Importing:", file))


  # get file info

  file <- tolower(file)
  basename <- basename(file)
  extension <- tools::file_ext(file)


  # import dataset

  df <- switch(extension,
               "sas7bdat" = haven::read_sas(file, ...),
               "dta" = haven::read_stata(file, ...),
               "sav" = haven::read_spss(file, ...),
               "xlsx" = readxl::read_excel(file, ...),
               "xls" = readxl::read_excel(file, ...),
               vroom::vroom(file, ...)
  )

  if (!quietly){
    dplyr::glimpse(df)
  }
  # return data frame
  return(df)

}
