costFunction  <- function(X, y) {
  #COSTFUNCTION Compute cost for logistic regression
  #   J <- COSTFUNCTION(theta, X, y) computes the cost of using theta as the
  #   parameter for logistic regression.
  
  function(theta) {
    # Initialize some useful values
    m <- length(y); # number of training examples
    
    # You need to return the following variable correctly
    J <- 0
    
    # ----------------------- YOUR CODE HERE -----------------------
    # Instructions: Compute the cost of a particular choice of theta.
    #               You should set J to the cost.

    # compute h = g(X * theta)
    h <- sigmoid(X %*% theta)
    
    # [-t(y) * log(h)]
    y0 <- t(y) %*% log(h)
    #[t(1-y) * log(1-h)]
    y1 <- t(1-y) %*% log(1-h)
    
    # compute J = 1/m * (-[t(y) * log(h)] - [t(1-y) * log(1-h)]) 
    J <- (-y0 - y1) / m
    return(J)
    # ----------------------------------------------------
  }
}




grad <- function(X, y) {
  #COSTFUNCTION Compute gradient for logistic regression
    #   J <- COSTFUNCTION(theta, X, y) computes the gradient of the cost
    #   w.r.t. to the parameters.
  function(theta) {

    # You need to return the following variable correctly
    grad <- matrix(0,dim(as.matrix(theta)))
    m <- length(y)
    # ----------------------- YOUR CODE HERE -----------------------
    # Instructions: Compute the partial derivatives and set grad to the partial
    #               derivatives of the cost w.r.t. each parameter in theta
    #
    # Note: grad should have the same dimensions as theta
    #
    
    # compute p1 = g(X * theta) - y
    p1 <- sigmoid(X %*% theta) - y
    
    # compute grad = 1/m [t(X) * p1]
    grad <- (t(X) %*% p1) / m
    return(grad)
    # ----------------------------------------------------
    
  }
}