#' Project vector of weights to L1 simplex
#' 
#' This function is used by online super learner to project weights onto
#' probability simplex 
#'
#' @param w A vector of weights to be projected

#' @return w_proj The vector projected onto L1 simplex


# projection function for weights to L-1 simplex
.projToL1Simp <- function(w){
    len_w <- length(w)
    sort_w <- sort(w, TRUE)
    K <- rev(which(sort_w - (cumsum(sort_w) - 1)/seq(len_w) > 0))[1]
    tau <- (sum(sort_w[seq(K)]) - 1)/K
    w_pos <- w - tau
    w_proj <- ifelse(w_pos >= 0, w_pos, 0) 
    w_proj
}