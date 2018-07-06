gradientDescentMulti <- function(X, y, theta, alpha, num_iters) {
    #GRADIENTDESCENTMULTI Performs gradient descent to learn theta
    #   theta <- GRADIENTDESCENTMULTI(x, y, theta, alpha, num_iters) updates theta by
    #   taking num_iters gradient steps with learning rate alpha
    
    # Initialize some useful values
    m <- length(y) # number of training examples
    n <- ncol(X) # number of features
    J_history <- rep(0,num_iters)
    
    for (iter in 1:num_iters) {
        # ---------------------- YOUR CODE HERE ----------------------
        # Instructions: Perform a single gradient step on the parameter vector
        #               theta.
        #
        # Hint: While debugging, it can be useful to print out the values
        #       of the cost function (computeCostMulti) and gradient here.
        #
        h <- X %*% theta
        series <- numeric(n)
        
        for (j in 1:n){
            series[j] <- sum((h - y) * X[, j])
        }
        
        for (j in 1:n){
            theta[j] <- theta[j] - (series[j] * alpha / m)
        }
        
        # series0 <- sum(h - y)
        # series1 <- sum((h - y) * X[, 2])
        # series2 <- sum((h - y) * X[, 3])
        # 
        # theta[1] <- theta[1] - (series0 * alpha / m)
        # theta[2] <- theta[2] - (series1 * alpha / m)
        # theta[3] <- theta[3] - (series2 * alpha / m)
    
        # Save the cost J in every iteration
        J_history[iter] <- computeCostMulti(X, y, theta)
    }
    
    # ------------------------------------------------------------
    
    
    
    list(theta = theta, J_history = J_history)
}
