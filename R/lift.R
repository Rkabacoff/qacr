#' Lift and gain charts
#'
#' @description
#' Create lift and gain charts for binary
#' classification problems
#'
#' @param actual actual class.
#' @param prob predicted probability of target class.
#' @param positive which class is the target class?
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @return a data frame with lift and gain values for each decile.
#' @examples
#' # logistic regression example
#' data(heart)
#' heart <- na.omit(heart)
#' fit <- glm(disease ~ . , heart, family=binomial)
#' pred <- predict(fit, newdata=heart, type="response")
#' results <- lift_plot(heart$disease, pred, positive="yes")
#' print(results)
lift_plot <- function(actual, prob, positive="yes"){

  df <- data.frame(prob_positive = prob, actual=actual)
  df <- df %>% mutate(decile = 11 - ntile(prob_positive, 10))

  tbl1 <- df %>% group_by(decile) %>% summarize(ncases=n())

  tbl2 <- df %>% filter(actual == positive) %>%
    group_by(decile) %>% summarize(nresp=n())

  tbl <- inner_join(tbl1, tbl2, by="decile")
  tbl <- tbl %>% mutate(cumresp = cumsum(nresp))
  tbl <- tbl %>% mutate(pctevents = round(nresp/sum(nresp)*100, 2))
  tbl <- tbl %>% mutate(gain = cumsum(pctevents))
  tbl <- tbl %>% mutate(cumlift = gain /(decile*10))
  tbl <- tbl %>% mutate(decile = 10* decile)

  # add 0,0
  tbl0 <- tbl[1,] %>% mutate(decile=0, gain=0)
  rndm <- tibble(x=seq(0,100, 10), y=seq(0,100, 10))

  # create gain chart
  p1 <- ggplot(data=rbind(tbl0, tbl), aes(x=decile, y=gain)) +
    geom_point(size=3) + geom_line() +
    scale_x_continuous(breaks = seq(0, 100, 10),
                       limits=c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       limits=c(0, 101)) +
    geom_point(data=rndm, aes(x=x, y=y), color="red") +
    geom_line(data=rndm, aes(x=x, y=y), color="red", linetype=2) +
    labs(title = "Gain Chart",
         x = "% of data",
         y = "% of events") +
    theme(panel.grid.minor=element_blank())

  # create lift chart
  rndm <- tibble(x=seq(10,100, 10), y=1)
  p2 <- ggplot(data=tbl, aes(x=decile, y=cumlift)) +
    geom_point() + geom_line() +
    scale_x_continuous(breaks = seq(10, 100, 10),
                       limits=c(10, 100)) +
    geom_point(data=rndm, aes(x=x, y=y), color="red") +
    geom_line(data=rndm, aes(x=x, y=y), color="red", linetype=2) +
    labs(title = "Lift Chart",
         x = "% of data",
         y = "Lift") +
    theme(panel.grid.minor.x=element_blank())

  plot(p1)
  plot(p2)
  return(as.data.frame(tbl))

}
