normalEqn <- function(X, y) {
    #NORMALEQN Computes the closed-form solution to linear regression
    #   NORMALEQN(X,y) computes the closed-form solution to linear
    #   regression using the normal equations.
    source("pinv.R")
    theta <- rep(0,length(y))
    
    # ---------------------- YOUR CODE HERE ----------------------
    # Instructions: Complete the code to compute the closed form solution
    #               to linear regression and put the result in theta.
    #
    
    xtx_inv <- pinv(t(X) %*% X)
    theta <- as.numeric(xtx_inv %*% t(X) %*% y)
    
    theta
    # ------------------------------------------------------------
    
}
