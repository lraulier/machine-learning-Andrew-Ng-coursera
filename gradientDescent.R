# use gradient descent to minimize the cost function
gradientDescent <- function(X,y,theta,alpha,iteration){
  #initialisation de la matrice history pour sauvegarder les
  # valeurs successive de costFunction
  J_history <- as.matrix(rep.int(0,iteration))
  T_history <- matrix(rep(c(0,0),iteration),nrow = 2)
  for(i in 1:iteration){
    # gradient descent step
    error <- (X %*% theta) - y
    # mise à jour des valeur de theta
    theta <- theta - (t(X) %*% error) %*% (alpha/dim(y)[1])
    #fitline <- lm(X[,2] ~ (X %*% theta))
    #plot(X[,2],y[,1])
    #abline(fitline,lwd = 2)
    #Sys.sleep(0.01)
    J_history[i,1] <- computeCost(X,y,theta)
    T_history[1,i] <- theta[1,1]
    T_history[2,i] <- theta[2,1]
  }
  list.computation <- list(J_history,T_history)
  return(list.computation)
}