#' Computes a prediction for sgd poisson regression
#' 
#' Get a prediction on a new data point for the current sgd poisson regression 
#' fit.
#'
#' @param fit The current object fit of class \code{SL.sgd.poisson}
#' @param newdata A \code{data.frame} object with data on which predictions are 
#' desired. 

#' @return pred A vector of predictions for current sgd poisson regression.
#'
#' @export
#' 
#' @examples
#' Examples to come
#' 

predict.SL.sgd.boundedLogistic <- function(x, newdata, ...){
    Y <- rep(0,nrow(newdata))
    Xmat <- model.matrix(as.formula(x$fit$formula),newdata)
    ptild <- plogis(Xmat %*% unlist(x$fit$theta))
    pred <- ptild*(x$fit$upper - x$fit$lower) + x$fit$lower
    pred
}