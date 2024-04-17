'''
lydia barnes, april 2024

applies dijkstra algorithm to get shortest paths between doors in doors project.

inputs:
    graph, a numpy file (nrows = ncolumns = nnodes) of connection weights. in
    this case, connection weights are physical distances between doors in a.u.,
    with 1 = the distance between two adjacent doors. 
    sub_infos, a .mat file of trial codes for the subjects whose paths you want
    to analyse. each row is one subject. columns 2:5 (zero-indexed) indicate 
    target door identities in Context A, 6:9 Context B. 

NB:
    if we consider the trials as a continuous sequence, there is only one 
    shortest path for any group of four doors. if we treat a single trial as an
    independent event, one starting door will give the shortest path. that is,
    for nodes ABCDA the shortest path remains the same no matter which door you
    select first. for nodes ABCD, the shortest path will be the shortest loop
    path, but dropping the longest vertex.
'''

#TODO: get the shortest route through ALL doors. see https://gis.stackexchange.com/questions/420940/shortest-path-touching-all-points.
#TODO: save output.
#TODO: export dependencies to .yaml 

## get libraries
import numpy as np
import scipy

from solve_tsp import travelling_salesman, hamiltonian_cycle

#  read the graph
graph = np.load('src-dijkstra/graph.npy') 

#   read the trial list from sub_infos.mat
trial_list = scipy.io.loadmat('src-dijkstra/sub_infos')
trial_list = trial_list['sub_infos']

idx = range(2,6)
step = 4
for subject in range(0,np.shape(trial_list)[1]):
    for context in range(0,2):

        #   select the context-relevant doors
        if context==0:
            doors = trial_list[subject,idx]
        else:
            doors = trial_list[subject,idx+(step*context)]

        #   get their connections
        this_graph = np.empty((len(doors),len(doors)))
        for i,door in enumerate(doors):
            this_graph[i,:] = graph[door-1,doors-1]

        #   get the shortest path(s), requiring a full loop
        shortest_path_tsp = travelling_salesman(this_graph)

        #   get the shortest path(s), allowing a single visit to each node
        shortest_path_hc = []; min_path = [];
        for i in doors:
            [a,b] = hamiltonian_cycle(this_graph,i)
            shortest_path_hc.append(a)
            min_path.append(b)
        idx = np.where(min_path == np.min(min_path))[0]
        shortest_path_hc = shortest_path_hc[idx,:]
        shortest_path_hc = np.asarray(shortest_path_hc)
