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

#TODO: write your own dijkstra.py to get the shortest route through ALL doors.
#TODO: save output.
#TODO: export dependencies to .yaml 

## get libraries
import numpy as np
import scipy

from dijkstras import Graph

#  read the graph
graph = np.load('src-dijkstra/graph.npy') 

#   format the graph for dijkstra algorithm
g = Graph()
for row in range(0,np.shape(graph)[0]): 
    name = str(row+1) #don't zero-index
    edges = dict(enumerate(graph[row],1))
    edges = {str(k):v for k,v in edges.items()} #Graph class expects dictionary keys to be strings

    g.add_vertex(name,edges)

#   read the trial list from sub_infos.mat
trial_list = scipy.io.loadmat('src-dijkstra/sub_infos')
trial_list = trial_list['sub_infos']

print(g.shortest_path('1','16'))

idx = range(2,6)
step = 4
for subject in range(0,np.shape(trial_list)[1]):
    for context in range(0,2):
        print()