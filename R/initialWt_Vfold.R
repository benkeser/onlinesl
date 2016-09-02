#' Compute initial estimates for online learning algorithms
#' 
#' This function creates a list of otherArgs used by each online algorithm by
#' calling learner.init for each learner in \code{SL.library}. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param X The data.frame of predictors, with \code{nl} rows 
#' @param SL.library The K length character vector of wrappers for candidate
#' online learners.
#' @param V The number of folds for V-fold cross validation, defaults to 5
#' @param lossFn The loss function used by the super learner
#' @param ensembleControl The list of controls for the ensemble method. The 
#' function uses \code{ensembleControl$ensemblePredictFn} to compute predictions
#' on held out data to minimize the \code{lossFn} using \code{solnp}. 
#' @param hessian Boolean of whether to return the estimated Hessian at the 
#' solution of the \code{solnp} algorithm. Must be set to \code{TRUE} if 
#' \code{ensembleControl$updateWtFn} requires an estimate of the Hessian to 
#' update the weights (e.g., if \code{updateWtFn} corresponds to second-order
#' stochastic gradient descent). 
#'
#' @return dsl The name of the algorithm from \code{SL.library} that was the
#' discrete super learner.
#' @return alpha A K-length vector of initial weight estimates for the super 
#' learner
#' @return otherWtArgs A list with entry \code{hessian} containing the estimated
#' hessian (set to null if \code{initialHessian = FALSE}). 
#' 
#' @export

initialWt_Vfold <- function(Y, 
                            X, 
                            SL.library,
                            V=5,
                            lossFn,
                            ensembleControl,
                            initialHessian = FALSE){
    # Call each 'learner'.init function specified using the first n_l observations
    nl <- length(Y)
    K <- length(SL.library)
    # get sample splits
    validRows <- split(sample(1:nl), rep(1:V, length = nl))
    # in each split
    predList <- lapply(validRows, function(folds){
        pred <- t(aaply(SL.library, 1, function(learner){
            tmp <- do.call(paste0(learner),
                           args=list(Y=Y[-folds],X=X[-folds,,drop=FALSE],
                                     newX=X[folds,,drop=FALSE],initial = TRUE))
            tmp$pred
        }))
        pred
    })
    pred <- Reduce("rbind", predList)

    # figure out which is discrete SL
    risk <- apply(pred, 2, function(p){
        do.call(lossFn, args=list(Y=Y[unlist(validRows)], p=p, alpha=NULL,
                                  ensemblePredictFn=NULL, risk=TRUE))
    })
    # index of discrete Super Learner
    dsl <- SL.library[which(risk == min(risk))[1]]
        
    # get continuous SL weights
    .wtFn <- function(wt,Y,predictions,predictFunction,lossFn){
        do.call(lossFn, args=list(Y=Y,p=predictions,alpha=wt,
                                  ensemblePredictFn=predictFunction,
                                  risk=TRUE))
    }
    fitWt <- Rsolnp::solnp(
        pars=matrix(rep(1/K, K)), fun=.wtFn, LB=rep(0,K),
        eqfun=function(wt,Y,predictions,predictFunction,lossFn){1-sum(wt)},
        control=list(trace=0),
        eqB=0, Y=Y[unlist(validRows)], predictions=pred,
        predictFunction=ensembleControl$ensemblePredictFn, lossFn=lossFn
        )
    
    hessian <- NULL
    if(initialHessian){
        hessian <- fitWt$hessian * nl
    }
    return(list(
        dsl = dsl,
        alpha = matrix(fitWt$pars),
        otherWtArgs = list(hessian=hessian)
    ))
}


