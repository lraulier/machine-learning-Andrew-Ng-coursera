#computeCost <- function(problem.data.X.matrice, problem.data.y.matrice, problem.data.initial.theta){
#  return(sum(((problem.data.X %*% problem.data.initial.theta) - problem.data.y.matrice)^2))
#}
computeCost <- function(X, y, theta){
  return(sum(((X %*% theta) - y)^2)/(2 * dim(y)[1]))
}

