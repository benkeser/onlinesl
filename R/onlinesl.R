#' Computes the online super learner for a data set
#' 
#' This function uses online cross-validation to gauge the performance of 
#' a user-select library of online algorithms. More description to come.
#'
#' @param inputParameter1 A description of the input parameter 
#' @param inputParameter2 A description of the input parameter 
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' Examples to come
#' 

onlinesl <- function(Y, 
                     X, 
                     family, 
                     SL.library,
                     nl=200,
                     parallel="seq",
                     ensembleControl = list(initialWtFn="initialWt_Vfold",
                                            ensemblePredictFn="predict_convexLinCom",
                                            updateWtFn="sgdWt_convexLinCom",
                                            maxCorrectWtStep = 100),
                     trace = list(verbose = FALSE,
                                  saveRisk = NULL),
                     lossFn = ".mse"
                     ){
    # some preliminary parameters
    N <- length(Y)
    K <- length(SL.library)
    call <- match.call(expand.dots = TRUE)

    # get initial otherArgs for each online learner
    fit.t <- alply(SL.library, 1, function(learner){
        do.call(eval(parse(text=learner)),
                args=list(Y=Y[1:nl],X=X[1:nl,,drop=FALSE],
                          newX=X[1:nl,,drop=FALSE],initial=TRUE))
    })
    
    # get initial weight values for online super learner
    slFit.t <- do.call(
        ensembleControl$initialWtFn,
        args = list(Y=Y[1:nl], X=X[1:nl,,drop=FALSE], SL.library=SL.library,
                    lossFn=lossFn, ensembleControl=ensembleControl)
        )

    # Initialize list of online risk for all learners
    RCV.t <- 0
    learnerList <- split(SL.library, 1:length(SL.library))
    traceRisk <- NULL
    if(!is.null(trace$saveRisk)){
        i <- 0
        traceRisk <- matrix(NA, ncol=K+3, nrow=round((N-nl)/trace$saveRisk))
    }
    
    if(trace$verbose) cat("Beginning candidate updates \n")
    for(tplus1 in (nl+1):N){
        if(trace$verbose & (tplus1 %% 1000==0)){
            cat(paste0("Iteration: ", tplus1, "\n"))
            cat("    SL Weights: ", round(slFit.t$alpha,3), "\n")
            print(RCV.t)
        }
        if(!is.null(trace$saveRisk) & tplus1%%trace$saveRisk==0){
            i <- i+1
            traceRisk[i,] <- c(tplus1, RCV.t)
        }
        
        # Call all learners
        fit.tplus1 <- mapply(
            learner = learnerList, fit = fit.t, 
            FUN=function(learner,fit){
                do.call(learner, 
                        args=list(Y=Y[tplus1],newX=X[tplus1,,drop=FALSE],
                                  t=tplus1,fit=fit$fit,initial=FALSE))
            }, SIMPLIFY = FALSE)
        
        # get prediction for all current learners on new data point
        p.P_t <- matrix(laply(fit.tplus1, function(f){f$pred}),nrow=1)
        
        # get prediction for discrete super learner on new data point
        dslp.P_t <- p.P_t[which(SL.library == slFit.t$dsl)]
        
        # get prediction for super learner on new data point
        slp.P_t <- do.call(ensembleControl$ensemblePredictFn, 
                               args=list(p = p.P_t, alpha=slFit.t$alpha))
        
        # calculate loss of each learner on new data point
        Lp.P_t <- do.call(lossFn,
                          args=list(Y=Y[tplus1], p=c(p.P_t, dslp.P_t, slp.P_t),
                                    risk=FALSE))
        
        # update risk
        RCV.tplus1 <- (tplus1-nl)/(tplus1-nl+1)*RCV.t + 1/(tplus1-nl+1)*Lp.P_t
        
        # update discrete sl
        # SL.library[which(risk == min(risk))[1]]
            
        # update super learner weights
        slFit.tplus1 <- do.call(ensembleControl$updateWtFn, 
                                args=list(Y=Y[tplus1],slFit.t=slFit.t, 
                                          p.t = p.P_t, tplus1 = tplus1))
        
        # check whether risk improved
        slp.alpha.tplus1 <- do.call(ensembleControl$ensemblePredictFn, 
                                    args=list(p = p.P_t, 
                                              alpha=slFit.tplus1$alpha))
        goodStep <- do.call(lossFn, args=list(Y[tplus1],p=slp.alpha.tplus1)) - 
            Lp.P_t[K+2] <= 1e-8
        
        # if risk is not improved by step, take corase grid search to find
        # convex combination resulting in decreased risk
        ct <- 1
        while(!goodStep & ct < ensembleControl$maxCorrectWtStep){
            ct <- ct + 1
            alphaMod <- matrix(1/(ct^2-1) * slFit.tplus1$alpha + 
                                   (1-1/(ct^2-1))*slFit.t$alpha,ncol=1)
            slp.alphaMod <- do.call(ensembleControl$ensemblePredictFn, 
                                        args=list(p = p.P_t, 
                                                  alpha=alphaMod))
            goodStep <- do.call(lossFn, args=list(Y[tplus1],p=slp.alphaMod)) - 
                Lp.P_t[K+2] <= 1e-8
        }
        # if we made an adjustment, add it to slFit.tplus1
        if(ct > 1){
            slFit.tplus1$alpha <- alphaMod
        }
        
        # update slFit
        slFit.t <- slFit.tplus1
        slFit.t$dsl <- 
            SL.library[which(RCV.tplus1[1:K] == min(RCV.tplus1[1:K]))[1]]
        # update fit
        fit.t <- fit.tplus1
        # update risk
        RCV.t <- RCV.tplus1
    }
    out <- list(call=call, libraryNames=SL.library, 
                coef=slFit.t$alpha, CVRisk = RCV.t, 
                fitLibrary=fit.t, traceRisk = traceRisk
    )
    class(out) <- "onlinesl"
    out
}
