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

predict.SL.sgd.poisson <- function(object, newdata, ...){
    Y <- rep(0,nrow(newdata))
    Xmat <- model.matrix(as.formula(object$fit$formula),newdata)
    pred <- exp(Xmat %*% unlist(object$fit$theta))
    pred
}