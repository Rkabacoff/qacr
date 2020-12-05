#' @title ROC Plot
#'
#' @description Plot a receiver operating curve for a binary predictive model
#'
#' @details
#' Takes a model produced by the train function in the caret package and
#' the binary level to predict, and generates a ROC plot.
#' @param model predictive model from the caret train function.
#' @param positive label for the class to predict.
#' @param n.cuts number of probability cutpoints to plot.
#' @param labelsize size of cutpoint labels.
#' @param labelround number of decimal digits in the cutpoint labels.
#' @export
#' @import caret
#' @import ggplot2
#' @import plotROC
#' @return a ggplot2 graph
#' @examples
#'
#' # logistic regression example
#' library(caret)
#' data(GermanCredit)
#' model.lr <- train(Class ~ Age + Amount + Duration,
#'                   data=GermanCredit,
#'                   method="glm", family="binomial")
#' rocPlot(model=model.lr, positive="Bad")
#'
rocPlot <- function(model, positive, n.cuts=20, labelsize=3,
                    labelround=2){

  if (!inherits(model, "train")){
    stop("Model must result from caret::train().")
  }

  df <- model[["trainingData"]]


  # actual outcome
  outcome <- df[[".outcome"]]
  outcome <- as.character(outcome)

  # check positive value
  x <- table(outcome)
  if (length(x) != 2) stop("Outcome must have 2 levels.")
  if (missing(positive)) {
    positive <- names(x)[length(x)]
  } else if(!(positive %in% names(x))){
    stop("Specified outcome is not one of the existing levels.")
  }

  text <- paste("predicting:", positive)

  outcome <- ifelse(outcome == positive, 1, 0)

  # predicted probability
  prob <- stats::predict(model, df, type="prob")
  prob <- prob[[positive]]

  df <- data.frame(d = outcome,
                   m = prob)

  p <- ggplot(df, aes(d=.data[["d"]], m=.data[["m"]])) +
    geom_roc(n.cuts=n.cuts, labelsize=labelsize,
             labelround=labelround, size=.5) +
    geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
    theme_bw() +
    labs(title="ROC Plot",
         x="1 - Specificity (False Positive Fraction)",
         y="Sensitivity (True Positive Fraction)") +
    scale_x_continuous(breaks=seq(0,1, .1))+
    scale_y_continuous(breaks=seq(0,1, .1))


  auc <- round(calc_auc(p)[1,3], 3)
  p + labs(subtitle=paste("Area under curve:", auc),
           caption=paste("predicting:", positive))

}
