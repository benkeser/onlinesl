#' Bounded negative log-likelihood loss function for ensemble
#' 
#' This function computes the average bounded negative log-likelihood
#' loss for a given set of outcomes Y of length, an n x K matrix of predictions 
#' p, and a row vector of weights alpha. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#' @param ensemblePredictFn The function used to compute ensemble predictions.
#' If null, the function assumes there is no ensemble to compute loss for and 
#' instead computes loss for vector Y and vector p. 
#' @param upper The upper bound on \code{Y} 
#' @param lower The lower bound on \code{Y}
#' @return loss The scalar value of the negative log-likelihood loss


.loglikBounded <- function(Y, p, alpha, ensemblePredictFn=NULL, risk=TRUE,
                           upper, lower, trimLogit = 1e-5){
    Ytild <- (Y-lower)/(upper-lower)
    if(is.null(ensemblePredictFn)){
        ptild <- (p - lower)/(upper - lower)
        ptild[ptild < trimLogit] <- trimLogit
        ptild[ptild > 1-trimLogit] <- 1-trimLogit
        if(!risk){
           - Ytild*log(ptild) - (1-Ytild)*log(1-ptild)
        }else{
            mean(- Ytild*log(ptild) - (1-Ytild)*log(1-ptild))
        }
    }else{
        pEnsemble <- do.call(ensemblePredictFn, args=list(p=p,alpha=alpha))
        pEtild <- (pEnsemble - lower)/(upper - lower)
        pEtild[pEtild < trimLogit] <- trimLogit
        pEtild[pEtild > 1-trimLogit] <- 1-trimLogit
        
        if(!risk){
            -Ytild*log(pEtild) - (1-Ytild)*log(1-pEtild)
        }else{
            mean(-Ytild*log(pEtild) - (1-Ytild)*log(1-pEtild))
        }
    }
}