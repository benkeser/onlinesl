#' Predict method for onlinesl objects
#' 
#' This function computes the ensemble prediction for the online super learner.
#' 
#' @param object An object of class \code{onlinesl}
#' @param newdata A \code{data.frame} object on which to obtain new predictions.
#' @param X Not sure yet
#' @param Y Not sure yet
#' @param onlySL Return predictions only for algorithms with non-zero weight in the online 
#' super learner?
#' 
#' @return pred The continuous online super learner predictions
#' @return library.predict The predictions from each candidate online algorithm
#' 
#' @export


predict.onlinesl <- function(object, newdata, X=NULL, Y=NULL, onlySL=FALSE,...){
    if(missing(newdata)){
        stop("Missing newdata. Please specify")
    }
    k <- length(object$libraryNames)
    predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
    colnames(predY) <- object$libraryNames
    if (onlySL) {
        whichLibrary <- which(object$coef > 0)
        predY <- matrix(0, nrow = nrow(newdata), ncol = k)
        for (mm in whichLibrary) {
            #newdataMM <- subset(newdata, select = object$whichScreen[object$SL.library$library[mm,2], ])
            newdataMM <- newdata                                                                                  
            family <- object$family
            XMM <- if(is.null(X)){
                NULL
            }else{
                X
            }
            predY[, mm] <- do.call("predict", list(object = object$fitLibrary[[mm]], 
                                                   newdata = newdataMM, family = family, X = XMM, 
                                                   Y = Y, ...))
        }
        getPred <- do.call(object$ensembleControl$ensemblePredictFn,
                           list(p=predY, alpha=object$coef))
        out <- list(pred = getPred, library.predict = predY)
    }
    else {
        for (mm in seq(k)) {
            #newdataMM <- subset(newdata, select = object$whichScreen[object$SL.library$library[mm, 2], ])
            newdataMM <- newdata                                                                                  
            family <- object$family
            XMM <- if(is.null(X)){
                NULL
            }else{
                X
            }
            #subset(X, select = 
            #object$whichScreen[object$SL.library$library[mm,2], ])
            predY[, mm] <- do.call("predict", list(object = object$fitLibrary[[mm]], 
                                                   newdata = newdataMM, family = family, X = XMM, 
                                                   Y = Y, ...))
        }
        getPred <- do.call(object$ensembleControl$ensemblePredictFn,
                           list(p=predY, alpha=object$coef))
        out <- list(pred = getPred, library.predict = predY)
    }
    return(out)
}


