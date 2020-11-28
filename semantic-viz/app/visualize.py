#!/usr/bin/python3

import os
import ast
import argparse
import networkx
import matplotlib.pyplot as plt


def main(filename):
    with open(filename, 'r') as fp:
        data = fp.read()

    cleanData = data.split()[1]
    listData = ast.literal_eval(cleanData)
    adjacencyList = dict(listData)
    graph = networkx.Graph(adjacencyList)

    for key, vals in adjacencyList.items():
        for v in vals:
            graph.add_edge(key, v)

    networkx.draw_spring(graph, with_labels=True)
    plt.show()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog='Graph Visualizer')
    parser.add_argument("filename", help="filename of adjacency list")
    args = parser.parse_args()
    main(args.filename)