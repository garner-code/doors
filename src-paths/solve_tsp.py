'''
lydia barnes, april 2024
from: https://www.geeksforgeeks.org/traveling-salesman-problem-tsp-implementation/
see also https://gis.stackexchange.com/questions/420940/shortest-path-touching-all-points

'''

from sys import maxsize 
from itertools import permutations
import numpy as np
 
# implementation of traveling Salesman Problem 
def travelling_salesman(graph,nodes): 
    V = np.shape(graph)[0]

    # list all vertices
    vertex = [] 
    for i in range(V): 
        vertex.append(i) 
 
    # store minimum weight
    min_lweight = maxsize 
    next_permutation=permutations(vertex)
    paths = []; lweight = [];
    for i in next_permutation:
 
        # compute current path weight 
        current_lweight = 0
        for c,j in enumerate(i): 
            k = i[c-1]
            current_lweight += graph[k][j] #regardless of what happens above, add a step to the loop weight
 
        # update minimum 
        min_lweight = min(min_lweight, current_lweight) 

        paths.append(nodes[list(i)])
        lweight.append(current_lweight)
    
    paths = np.asarray(paths)
    idx = np.where(np.round(lweight,4) == np.round(min_lweight,4))[0]
    paths = paths[idx,:]

    return [paths,min_lweight]


def hamiltonian_path(graph,nodes,s): 
    V = np.shape(graph)[0]
 
    # store all vertices apart from source vertex 
    vertex = [] 
    for i in range(V): 
        if i != s: 
            vertex.append(i) 
 
    # store minimum weight
    min_path = maxsize 
    next_permutation=permutations(vertex)
    paths = []; weights = []
    for i in next_permutation:
 
        # compute current path weight 
        current_pathweight = 0
        k = s 
        for j in i: 
            current_pathweight += graph[k][j] 
            k = j 
 
        # update minimum 
        min_path = min(min_path, current_pathweight) 
        
        i = (s,) + i #make sure you record the starting point
        paths.append(nodes[list(i)])
        weights.append(current_pathweight)
    
    paths = np.asarray(paths)
    idx = np.where(np.round(weights,4) == np.round(min_path,4))[0]
    min_paths = []; min_weights = []
    for i in idx:
        min_paths.append(paths[i,:])
        min_weights.append(min_path)
         
    return [min_paths,min_weights] 