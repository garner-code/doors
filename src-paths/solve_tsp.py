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
    min_path = maxsize 
    next_permutation=permutations(vertex)
    paths = []; weights = []
    for i in next_permutation:
        # store current Path weight(cost) 
        current_pathweight = 0
 
        # compute current path weight 
        for c,j in enumerate(i): 
            try:
                k = i[c-1]
            except:
                k = i[len(i)]
            current_pathweight += graph[k][j] 
 
        # update minimum 
        min_path = min(min_path, current_pathweight) 

        paths.append(nodes[list(i)])
        weights.append(current_pathweight)
    
    paths = np.asarray(paths)
    idx = np.where(np.round(weights,4) == np.round(min_path,4))[0]
    paths = paths[idx,:]

    return paths 


def hamiltonian_cycle(graph,nodes,s): 
    V = np.shape(graph)[0]
 
    # store all vertex apart from source vertex 
    vertex = [] 
    for i in range(V): 
        if i != s: 
            vertex.append(i) 
 
    # store minimum weight
    min_path = maxsize 
    next_permutation=permutations(vertex)
    paths = []; weights = []
    for i in next_permutation:
 
        # store current Path weight(cost) 
        current_pathweight = 0
 
        # compute current path weight 
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