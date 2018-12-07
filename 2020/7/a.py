#!/usr/bin/env python3
import networkx as nx
import queue
import re
import sys


lns = [
    tuple(
        map(lambda s: re.sub('\sbags*$', '', s),
            filter(
                lambda s: s and s != 'no other bags',
                re.split(r'(?:\scontain\s|,\s*|\.)', ln.strip())
            )
        )
    ) for ln in sys.stdin if ln.strip()
]

graph = nx.DiGraph()

for node, *edges in lns:
    print(node)
    graph.add_node(node)
    for edge in edges:
        print('\t|', edge)
        weight, color = edge.split(' ', 1)
        weight = int(weight)
        graph.add_edge(node, color, weight=weight)

print(len(nx.ancestors(graph, 'shiny gold')))


def count_bags(node='shiny gold'):
    sum = 0 if node == 'shiny gold' else 1
    for intern in graph.neighbors(node):
        m = graph[node][intern]['weight']
        sum += m * count_bags(intern)
    return sum

print(count_bags())
