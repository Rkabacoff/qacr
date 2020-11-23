#' @title Summary statistics for a quantitative variable
#' @description This function provides descriptive statistics for a quantitative
#' variable alone or seperately by groups. You can provide user-defined statistics.
#' @param data data frame
#' @param x numeric variable in data (unquoted)
#' @param statistics statistics to calculate (any function that produces a
#'  value), Default: c("n", "mean", "sd")
#' @param na.rm if TRUE, delete cases with missing values on x and or grouping
#'  variables, Default: TRUE
#' @param digits number of decimal digits to print, Default: 2
#' @param ... list of grouping variables
#' @importFrom purrr map_dfc
#' @import dplyr
#' @import haven
#' @return a data frame, where columns are grouping variables (optional) and
#' statistics
#' @examples
#' # If no keyword arguments are provided, default values are used
#' dstats(mtcars, mpg, am, gear)
#'
#' # You can supply as many (or no) grouping variables as needed
#' dstats(mtcars, mpg)
#'
#' dstats(mtcars, mpg, am, gear, cyl, carb)
#'
#' # You can input user-defined functions
#' # my_n <- function(xs) length(xs)
#' # dstats(mtcars, mpg, stats = c("my_n"), am, gear)
#' @rdname dstats
#' @export
dstats <- function(data, x, ...){
  x <- enquo(x)
  dots <- enquos(...)
  if(!is.numeric(data %>% pull(!!x))){
    stop("data$x is not numeric")
  }

  ## stats
  median <- function(xs){
    ys <- sort(xs)
    m <- length(xs)/2
    if(length(xs) %% 2 == 0){
      mean(c(ys[m], ys[m+1]))
    } else {
      ys[floor(m) + 1]
    }
  }

  n <- function(xs){
    length(xs)
  }

  ## Auxiliary functions
  my_sum <- function(data, col, cus_sum) {
    col <- enquo(col)
    cus_sum_name <- cus_sum
    cus_sum <- rlang::as_function(cus_sum)

    data %>%
      summarise(!!cus_sum_name := cus_sum(!!col))
  }

  my_sums <- function(data, col, cus_sums) {
    col <- enquo(col)
    purrr::map_dfc(cus_sums, my_sum, data = data, col = !!col)
  }


  extract_list_unnamed_elems <- function(my_list){
    unnamed_idxs <- (1:length(my_list))[names(my_list)  == ""]
    my_list[unnamed_idxs]
  }


  stats <- if(is.null(dots$stats)) {
      c("n", "mean", "sd")
    } else {
      eval(rlang::quo_get_expr(dots$stats))
    }
  na.rm <- if(is.null(dots$na.rm)) TRUE else rlang::quo_get_expr(dots$na.rm)
  digits <- if(is.null(dots$digits)) 2 else rlang::quo_get_expr(dots$digits)
  grouping_vars <- extract_list_unnamed_elems(dots)

  data <- data %>% select(!!x, !!!grouping_vars)

  if(na.rm){
    data <- na.omit(data)
  }

  data %>%
    mutate_at(vars(!!!grouping_vars), as_factor) %>%
    group_by(!!!grouping_vars) %>%
    group_modify(~my_sums(.x, col = !!x, cus_sums = stats)) %>%
    mutate_at(vars(-group_cols()), ~ round(as.double(.x), digits = digits)) %>%
    ungroup() %>% as.data.frame()
}

