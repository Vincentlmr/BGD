#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  7 17:29:33 2024

@author: vlemeur
"""

import pandas as pd
import networkx as nx
import numpy as np


# Charger les données à partir du fichier (assumant que le fichier est au format CSV)
# Si le fichier est dans un format différent, ajustez la fonction de lecture en conséquence.
file_path = "/home/v/l/vlemeur/IS4/S8/BGD/wikispeedia_paths-and-graph/paths_finished_nettoye2.csv"
data = pd.read_csv(file_path)

# Créer un graphe dirigé à partir des données
G = nx.from_pandas_edgelist(data, source='SourceColumn', target='TargetColumn', create_using=nx.DiGraph())

# Obtenir la matrice d'adjacence sous forme de tableau NumPy
adjacency_matrix = nx.to_numpy_matrix(G)

# Convertir la matrice en un tableau NumPy standard
adjacency_matrix = np.array(adjacency_matrix)

# Afficher la matrice d'adjacence
print("Matrice d'adjacence :")
print(adjacency_matrix)
