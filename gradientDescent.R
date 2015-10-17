# use gradient descent to minimize the cost function
gradientDescent <- function(X,y,theta,iteration){
  #initialisation de la matrice history pour sauvegarder les
  # valeurs successive de costFunction
  J_history <- as.matrix(rep.int(0,iteration))
  for(i in 1:iteration){
    J_history[i,1] <- computeCost(X,y,theta)
  }
  return(J_history)
}