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

outputs:
    travelling salesman solution: the shortest path that touches all target 
    doors and returns to the start. the start is irrelevant, because the path
    is a loop. there will be eight solutions: each start point x each 
    direction.

    hamiltonian cycle solution: the shortest path that touches all target 
    doors, visiting each only once (no loop). here, where you start is 
    important, as it allows you to exclude one section of the loop. there will 
    be two solutions, one the exact opposite of the other. some doors will 
    give more solutions, as the door array is quite symmetrical.

'''
#https://stackoverflow.com/questions/75114841/debugger-warning-from-ipython-frozen-modules
#TODO: export dependencies to .yaml 

## get libraries
import numpy as np
import scipy
import json
from solve_tsp import travelling_salesman, hamiltonian_cycle

#  read the graph
graph = np.load('graph.npy') 

#   read the trial list from sub_infos.mat
trial_list = scipy.io.loadmat('sub_infos')
trial_list = trial_list['sub_infos']

tsp_solutions = []
hc_solutions = []
for subject in range(0,np.shape(trial_list)[1]):

    tsp = []
    hc = []
    for context in range(0,2):

        #   select the context-relevant doors
        if context==0:
            idx = range(2,6)
        else:
            idx = range(6,10)
        doors = trial_list[subject,idx]

        #   get their connections
        this_graph = np.empty((len(doors),len(doors)))
        for i,door in enumerate(doors):
            this_graph[i,:] = graph[door-1,doors-1]

        #   get the shortest path(s), requiring a full loop
        sp_tsp = travelling_salesman(this_graph,doors)

        #   get the shortest path(s), allowing a single visit to each node
        #       first, get the shortest paths for each start point
        sp_hc = []; min_path = []
        for i in range(0,len(doors)):
            [a,b] = hamiltonian_cycle(this_graph,doors,i)
            for j in range(0,len(a)):
                sp_hc.append(a[j])
                min_path.append(b[j])
        #       some start points will allow for shorter paths than others. 
        #       find those:
        min_idx = np.where(min_path == np.min(min_path))[0]
        sp_hc = np.asarray(sp_hc)[min_idx,:]

        tsp.append(sp_tsp.tolist())
        hc.append(sp_hc.tolist())
    
    tsp_solutions.append(tsp)
    hc_solutions.append(hc)

f = open('tsp_solutions.json','w')
json.dump(tsp_solutions,f)
f.close()

f = open('hc_solutions.json','w')
json.dump(hc_solutions,f)
f.close()

print()
