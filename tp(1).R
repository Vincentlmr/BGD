library(readr)
library(igraph)


#lecture du fichier csv issu du fichier tsv
paths_finished_nettoye <- read.csv("~/IS4/Semestre8/Big_Data/wikispeedia_paths-and-graph/paths_finished_nettoye.csv", header=FALSE, sep=";")

#on commence par nettoyer la table, c'est à  dire on retire les "<" et l'élément qui arrive avant 
#la taille correspond à la longueur de la ligne, c'est  à dire le nombre de clics pour arriver du début à la fin de la recherche
#cette variable est utilisée afin de réduire le temps des boucles dans les parties suivantes
#temps d'éxécution : environ 30 secondes
taille=c()
for (i in 1:nrow(paths_finished_nettoye)){
  j=1
  while (j<ncol(paths_finished_nettoye) && paths_finished_nettoye[i,j]!=""){
    if(paths_finished_nettoye[i,j]=="<"){
      paths_finished_nettoye[i,j-1]=""
      paths_finished_nettoye[i,j]=""
  }
    j=j+1
  }
  taille<-append(taille,j-1)

}


#dans cette partie, on récupére toutes les associations source-cible
#ce sont ces associations qui sont ensuite utilisées pour construire le graph avec igraph
#dans le cas ou la cible a été supprimé suite à la première partie, on prend la prochaine cible non nulle.
#attention, cette partie est assez longue pour être éxécutée : environ 4 minutes
list=c()
for(i in 1:nrow(paths_finished_nettoye)){
  j=1
  while(j<taille[i]-1){
    sour=paths_finished_nettoye[i,j]
    j=j+1
    while(paths_finished_nettoye[i,j]=="")  {
      j=j+1
    }
    targ=paths_finished_nettoye[i,j]
    list=append(list,c(sour,targ))
  }
}

#on construit le graphe avec igraph et on l'affiche
#le grpahique est très peu lisible dû au grand nombre de données, de plus, certains liens sont doublés, on va donc essayer de retirer cette multiplication de liens
g2 <- graph(edges=list)
plot(g2)

#on récupére la matrice d'adjacence obtenue avec igraph
mat=as_adj(g2)
adj=as.data.frame(as.matrix(mat))

#ppour qu'il y ait au maximum un lien pour chaque association, on modifie la matrice d'adjacence
#si il y a un lien entre deux éléments, il suffit de remplacer le nombre de fois où l'association a été trouvé par 1
#si ce nombre vaut 0, c'est uniquement car il n'y a pas d'associtiation entre les 2 éléments, on laisse donc le 0
adj[adj !=0] <- 1

# Convertir en matrice numérique pour pouvoir ensuite utiliser igraph avec la matrice d'adjacence
adj_numeric <- as.matrix(adj)
#on met ausi la diagonale de la matrice d'adjacence à 0, ceci est dû à la suppression d'éléments un élémént ne peut être à la fois une source et une cible
diag(adj_numeric)=0

# Utiliser la matrice numérique pour créer le graphe
g2 <- graph_from_adjacency_matrix(adj_numeric)

# Affichage du graphe
plot(g2)

#afin de pouvoir utiliser PageRank, on commence par normaliser les lignes puis on transpose la matrice
norm=rowSums(adj_numeric)
for (i in 1:nrow(adj_numeric)) {
  adj_numeric[i,] <- adj_numeric[i,] / norm[i]
}
transpose=t(adj_numeric)
adj_matrix=transpose

#on vérifie que la somme par colonne de la matrice vaille 
colSums(adj_matrix)



#pour la suite de l'algorithme, on vérifie qu'il n'y ait pas de NaN dans la matrice d'adjacence

# Vérifier s'il y a des valeurs NaN dans la matrice adj_matrix
if (any(is.nan(adj_matrix))) {
  print("La matrice contient des valeurs NaN.")
} else {
  print("La matrice ne contient pas de valeurs NaN.")
}

# Trouver les indices des valeurs NaN dans la matrice adj_matrix
nan_indices <- which(is.nan(adj_matrix), arr.ind = TRUE)

# Afficher les indices des valeurs NaN
print(nan_indices)
#on supprime toute les colonnes qui posent problème, ainsi que les lignes correspondantes
adj_matrix <- adj_matrix[, -2832]
adj_matrix <- adj_matrix[ -2832,]
adj_matrix <- adj_matrix[, -2962]
adj_matrix <- adj_matrix[ -2962,]
adj_matrix <- adj_matrix[, -3367]
adj_matrix <- adj_matrix[ -3367,]
adj_matrix <- adj_matrix[, -3926]
adj_matrix <- adj_matrix[ -3926,]


#on commence maintenant à faire le PageRank avec un beta de 0.85
beta=0.85
n <- nrow(adj_matrix)

# Initialisation des valeurs de PageRank
pagerank <- rep(1.0, n)
#on prend une tolérance de 10^-6 pour arrêter la boucle
tolerance=0.000001
for (iter in 1:1000) {
  prev_pagerank = pagerank
  #on calcule le pagrank à l'aide de la méthode itérative de la puissance
  pagerank <- beta * (adj_matrix %*% pagerank) + (1 - beta) / n*rep(1.0, n)
  #si la norme du  vecteur est inférieure à la tolérance, alors la méthode converge et on arrête
  if (sum(abs(pagerank - prev_pagerank)) < tolerance) {
    break
  }
}


#on regarde le nombre d'itération afin d'avoir la convergence
#on regarde donc quel élément à le plus haut PageRank
print(iter)
valmax=max(pagerank)
valmax
position <- which(pagerank == valmax, arr.ind = TRUE)
position


# on récupére maintenant  les indices des 20 premiers éléments dans le vecteur de PageRank
top_indices <- order(pagerank, decreasing = TRUE)[1:20]

# on obtient les noms des lignes correspondant aux 20 premiers éléments dans le vecteur de PageRank
noms_premiers <- noms_lignes[top_indices]

# on affiche les noms des lignes correspondant aux 20 premiers éléments
print(noms_premiers)


#on refait le PageRank avec des valeurs de Beta différentes afin de voir la convergence pour chaque Beta
valeurs_iter <- numeric(length = length(seq(0.1, 0.99, by = 0.01)))
for (i in seq(0.01, 0.99, by = 0.01)){
  beta=i
  n <- nrow(adj_matrix)
  # Initialisation des valeurs de PageRank
  pagerank <- rep(1.0, n)
  tolerance=0.000001
  for (iter in 1:1000) {
    prev_pagerank = pagerank
    pagerank <- beta * (adj_matrix %*% pagerank) + (1 - beta) / n*rep(1.0, n)
    if (sum(abs(pagerank - prev_pagerank)) < tolerance) {
      break
    }
    
  }
  valeurs_iter[round(i / 0.01)] <- iter
}

#on affiche le nombre d'itérations nécessaires pour la convergence du PageRank pour chaque valeurs de Beta
plot(seq(0.01, 0.99, by = 0.01), valeurs_iter, type = "l", xlab = "Valeur de beta", ylab = "Nombre d'itérations", main = "Nombre d'itérations en fonction de beta")




#on effectue maintenant le pageRank personnalisé
# Conversion de la matrice en un vecteur
vecteur <- as.vector(pagerank)

# Trier les valeurs uniques par ordre décroissant
valeurs_triees <- sort(unique(vecteur), decreasing = TRUE)

# Sélectionner la 6ème plus grande valeur
elt <- valeurs_triees[6]

#on regarde à qeul élément correspond cette valeur
position <- which(pagerank == elt, arr.ind = TRUE)
position

#on effectue le PageRank personnalisé pour la 51eme ligne, qui correspond à la 6eme plus grande valeur du PageRank
beta=0.85
#on initialise v comme un vecteur de 0, avec un 1 sur la colonne correspondant à la France
v <- rep(0.0, n)
v[51]=1
# Initialisation des valeurs de PageRank
pagerankp <- rep(1.0, n)
tolerance=0.000001
for (iter in 1:1000) {
  prev_pagerank = pagerankp
  #on reutilise la méthode de la puissance jusqu'à convergence
  pagerankp <- beta * (adj_matrix %*% pagerankp) + (1 - beta) / n*v
  if (sum(abs(pagerankp - prev_pagerank)) < tolerance) {
    break
  }
  
}

# on récupére maintenant  les indices des 20 premiers éléments dans le vecteur de PageRank
top_indices <- order(pagerankp, decreasing = TRUE)[1:20]

# on obtient les noms des lignes correspondant aux 20 premiers éléments dans le vecteur de PageRank
noms_premiers <- noms_lignes[top_indices]

# on affiche les noms des lignes correspondant aux 20 premiers éléments
print(noms_premiers)


# Sélectionner la 19ème plus grande valeur
elt <- valeurs_triees[19]

#on regarde à qeul élément correspond cette valeur
position <- which(pagerank == elt, arr.ind = TRUE)
position

#on effectue le PageRank personnalisé pour la 51eme ligne, qui correspond à la 6eme plus grande valeur du PageRank
beta=0.85
#on initialise v comme un vecteur de 0, avec un 1 sur la colonne correspondant à la France
v <- rep(0.0, n)
v[368]=1
# Initialisation des valeurs de PageRank
pagerankp <- rep(1.0, n)
tolerance=0.000001
for (iter in 1:1000) {
  prev_pagerank = pagerankp
  #on reutilise la méthode de la puissance jusqu'à convergence
  pagerankp <- beta * (adj_matrix %*% pagerankp) + (1 - beta) / n*v
  if (sum(abs(pagerankp - prev_pagerank)) < tolerance) {
    break
  }
  
}

# on récupére maintenant  les indices des 20 premiers éléments dans le vecteur de PageRank
top_indices <- order(pagerankp, decreasing = TRUE)[1:20]

# on obtient les noms des lignes correspondant aux 20 premiers éléments dans le vecteur de PageRank
noms_premiers <- noms_lignes[top_indices]

# on affiche les noms des lignes correspondant aux 20 premiers éléments
print(noms_premiers)




# Sélectionner la 31ème plus grande valeur
elt <- valeurs_triees[31]

#on regarde à qeul élément correspond cette valeur
position <- which(pagerank == elt, arr.ind = TRUE)
position

#on effectue le PageRank personnalisé pour la 51eme ligne, qui correspond à la 6eme plus grande valeur du PageRank
beta=0.85
#on initialise v comme un vecteur de 0, avec un 1 sur la colonne correspondant à la France
v <- rep(0.0, n)
v[1154]=1
# Initialisation des valeurs de PageRank
pagerankp <- rep(1.0, n)
tolerance=0.000001
for (iter in 1:1000) {
  prev_pagerank = pagerankp
  #on reutilise la méthode de la puissance jusqu'à convergence
  pagerankp <- beta * (adj_matrix %*% pagerankp) + (1 - beta) / n*v
  if (sum(abs(pagerankp - prev_pagerank)) < tolerance) {
    break
  }
  
}

# on récupére maintenant  les indices des 20 premiers éléments dans le vecteur de PageRank
top_indices <- order(pagerankp, decreasing = TRUE)[1:20]

# on obtient les noms des lignes correspondant aux 20 premiers éléments dans le vecteur de PageRank
noms_premiers <- noms_lignes[top_indices]

# on affiche les noms des lignes correspondant aux 20 premiers éléments
print(noms_premiers)
