plotData <-
  function (X, y, axLables = c("Exam 1 score","Exam 2 score"), legLabels =
              c('Admitted', 'Not admitted')) {
    #PLOTDATA Plots the data points X and y into a new device
    #   PLOTDATA(x,y) plots the data points with + for the positive examples
    #   and o for the negative examples. X is assumed to be a Mx2 matrix.
    
    # ----------------------- YOUR CODE HERE -----------------------
    # Instructions: Plot the positive and negative examples on a
    #               2D plot, using the option pch=3 for the positive (plus)
    #               examples and pch=21 for the negative (circle) examples.
    #
    adm <- which(y == 1)
    not_adm <- which(y == 0)
    
    plot(X[adm, 1], X[adm, 2], pch = 3,
         xlim = round(range(X[, 1])),
         ylim = round(range(X[, 2])),
         xlab = axLables[1],
         ylab = axLables[2])
    points(X[not_adm, 1], X[not_adm, 2], pch = 21, bg = "yellow")
    legend(x = max(X[, 1] - 12), y = max(X[, 2] + 4), legend = legLabels,
           pch = c(3, 21), pt.bg = c("black", "yellow"),
           bty = "n")
    
    
    # ----------------------------------------------------
}