
beta=0.85


n <- nrow(adj_matrix)
vec=rep(0, n)



# Initialisation des valeurs de PageRank
pagerank <- rep(1 , n)

for (iter in 1:max_iter) {

  # Calcul du nouveau PageRank
  pagerank <- beta * (adj_matrix %*% pagerank) + ((1 - beta) / n) *
}
  
v <- rep(1.0 , n)
s = 1/n