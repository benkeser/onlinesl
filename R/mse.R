#' Squared error loss function for ensemble
#' 
#' This function computes the average squared-error loss for a given set of
#' outcomes Y of length, an n x K matrix of predictions p, and a row vector
#' of weights alpha. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#' @param ensemblePredictFn The function used to compute ensemble predictions.
#' If null, the function assumes there is no ensemble to compute loss for and 
#' instead computes loss for vector Y and vector p. 
#' @param risk If \code{TRUE} compute average loss over a vector of predictions
#' \code{p}. If \code{FALSE} compute loss on single data point. 
#'
#' @return loss The scalar value of the squared-error loss or mean square-error
#' (if risk = TRUE)

.mse <- function(Y, p, alpha=NULL, ensemblePredictFn=NULL, risk=FALSE){
    if(is.null(ensemblePredictFn)){
        if(risk){
            mean((Y-p)^2)
        }else{
            (Y-p)^2
        }
    }else{
        pEnsemble <- do.call(ensemblePredictFn, args=list(p=p,alpha=alpha))
        if(risk){
            mean((Y - pEnsemble)^2)
        }else{
            (Y - pEnsemble)^2
        }
    }
}