gradientDescent <- function(X, y, theta, alpha, num_iters) {
    #GRADIENTDESCENT Performs gradient descent to learn theta
    #   theta <- GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by
    #   taking num_iters gradient steps with learning rate alpha
    
    # Initialize some useful values
    m <- length(y) # number of training examples
    J_history = rep(0,num_iters + 1)
    theta_history = matrix(0,num_iters + 1,length(theta))
    
    
    
    for (iter in 2:(num_iters + 1)) {
        # ---------------------- YOUR CODE HERE ----------------------
        # Instructions: Perform a single gradient step on the parameter vector
        #               theta.
        #
        # Hint: While debugging, it can be useful to print out the values
        #       of the cost function (computeCost) and gradient here.
        #
        h <- X %*% theta
        series0 <- sum(h - y)
        series1 <- sum((h -y) * X[, 2])
        
        theta[1] <- theta[1] - (series0 * alpha / m)
        theta[2] <- theta[2] - (series1 * alpha / m)
        
        theta_history[iter,] = t(theta)
        J_history[iter] = computeCost(X, y, theta)
    }
    
    list(theta = theta, J_history = J_history, theta_history = theta_history)
    # ------------------------------------------------------------
}
