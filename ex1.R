url <- "../ex1data1.txt"
# fichier de 2 colonnes séparé par une virgule , séparateur décimaux = . (point)
problem.data <- read.table(url,header = FALSE,sep = ",",dec = ".")
head(problem.data)