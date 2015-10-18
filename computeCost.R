#computeCost <- function(problem.data.X.matrice, problem.data.y.matrice, problem.data.initial.theta){
#  return(sum(((problem.data.X %*% problem.data.initial.theta) - problem.data.y.matrice)^2))
#}
computeCost <- function(X, y, theta){
  # calcule le cout de la function (notre hypothese) 
  # utilisation des moindres carrés
  # pour des valeurs theta définies
  # c'est en modifiant ces valeurs que l'on va reduire le cout de la fonction
  return(sum(((X %*% theta) - y)^2)/(2 * dim(y)[1]))
}

