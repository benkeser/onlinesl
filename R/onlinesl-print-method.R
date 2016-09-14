#' Print the output of onlinesl object
#'
#' @param object An object of class \code{onlinesl}
#' 
#' @export

print.onlinesl <- function(object,...){
    cat("\nCall: ", deparse(object$call, width.cutoff = 0.9 * getOption("width")), 
        "\n\n", fill = getOption("width"))
    K <- length(object$libraryNames)
    cat("Risk of discrete SL = ", round(object$CVRisk[[K+1]],5), "\n\n")
    cat("Risk of SL = ", round(object$CVRisk[[K+2]],5), "\n\n")
    out <- data.frame(Risk = round(unlist(object$CVRisk)[1:K],5), 
                 Coef = round(unlist(object$coef),3))
    row.names(out) <- object$libraryNames
    print(out)
}
