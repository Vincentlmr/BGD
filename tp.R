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

##PageRank
beta=0.85


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
print(iter)
valmax=max(pagerank)
valmax
position <- which(pagerank == valmax, arr.ind = TRUE)
position

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


plot(seq(0.01, 0.99, by = 0.01), valeurs_iter, type = "l", xlab = "Valeur de beta", ylab = "Nombre d'itérations", main = "Nombre d'itérations en fonction de beta")








#pageRank personnalisé
# Conversion de la matrice en un vecteur
vecteur <- as.vector(pagerank)

# Trier les valeurs uniques par ordre décroissant
valeurs_triees <- sort(unique(vecteur), decreasing = TRUE)

# Sélectionner la 19ème plus grande valeur
six <- valeurs_triees[6]

# Afficher la 19ème plus grande valeur
print(six)

position6 <- which(pagerank == six, arr.ind = TRUE)
position6



# Conversion de la matrice en un vecteur
vecteur_pagerank <- as.vector(pagerank)

# Trier les valeurs uniques par ordre décroissant
valeurs_triees <- sort(unique(vecteur_pagerank), decreasing = TRUE)

# Sélectionner la 6ème plus grande valeur
six <- valeurs_triees[6]

# Trouver la position de la 6ème plus grande valeur dans la matrice pagerank
position <- which(pagerank == six, arr.ind = TRUE)

# Afficher la position
print(position)

beta=0.85
v <- rep(0.0, n)
v[51]=1
# Initialisation des valeurs de PageRank
pagerank <- rep(1.0, n)
tolerance=0.000001
for (iter in 1:1000) {
  prev_pagerank = pagerank
  pagerank <- beta * (adj_matrix %*% pagerank) + (1 - beta) / n*v
  if (sum(abs(pagerank - prev_pagerank)) < tolerance) {
    break
  }
  
}




# Conversion de la matrice en un vecteur
vecteur_pagerank <- as.vector(pagerank)

# Trier les valeurs uniques par ordre décroissant
valeurs_triees <- sort(unique(vecteur_pagerank), decreasing = TRUE)

# Sélectionner la 6ème plus grande valeur
six <- valeurs_triees[6]

# Trouver la position de la 6ème plus grande valeur dans la matrice pagerank
position <- which(pagerank == six, arr.ind = TRUE)

# Afficher la position
print(position)
