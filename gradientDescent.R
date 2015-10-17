# use gradient descent to minimize the cost function
gradientDescent <- function(X,y,theta,alpha,iteration){
  #initialisation de la matrice history pour sauvegarder les
  # valeurs successive de costFunction
  J_history <- as.matrix(rep.int(0,iteration))
  for(i in 1:iteration){
    # gradient descent step
    error <- (X %*% theta) - y
    # mise à jour des valeur de theta
    theta <- theta - (t(X) %*% error) %*% (alpha/dim(y)[1])
    #plot(X[,2],(X %*% theta))
    #Sys.sleep(2)
    J_history[i,1] <- computeCost(X,y,theta)
  }
  return(J_history)
}