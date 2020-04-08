# Create a graph from a CSV with time data
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams
import matplotlib.ticker as ticker
import math
import csv

rcParams['font.family'] = 'serif'
linewidth=0.9

target = { "InfectionRate": 0.2 , "GroupSize": 3, "TreeDegree": 3 }

def isTarget(d):
    ok = True
    for key in target:
        ok = ok and target[key] == d[key]
    return ok

def sumUpTo(n, arr):
    acc = 0
    for i in range(0, n+1):
        acc += arr[i]
    return acc

def project (arr, key):
    return [d[key] for d in arr]

def toScenario(d):
    return [ d["Population"] , d["InfectionRate"] , d["GroupSize"] , d["TreeDegree"] ]

def sameScenario(d1, d2):
    return str(toScenario(d1)) == str(toScenario(d2))

def drawGraph(ax, scenario):
    scenario = scenarios[key]

    testsUsed   = project(scenario['values'], "TestsUsed")
    probability = project(scenario['values'], "Probability")
    ax.plot(
        testsUsed,
        probability,
        label="Exactly n tests"
    )

    accColor='red'
    accProbability = [sumUpTo(n, probability) for n in range(0, len(probability))]
    plt.fill_between(x=testsUsed, y1=accProbability, color=accColor, alpha=0.2)
    ax.plot(
        testsUsed,
        accProbability,
        color=accColor,
        label="Up to n tests"
    )

with open('plots/stats-0.csv') as csv_file:
    stats = []
    for d in csv.DictReader(csv_file):
        row = {
              "Population"    : int(d["Population"])
            , "InfectionRate" : float(d["InfectionRate"])
            , "GroupSize"     : int(d["GroupSize"])
            , "TreeDegree"    : int(d["TreeDegree"])
            , "TestsUsed"     : int(d["TestsUsed"])
            , "Probability"   : float(d["Probability"])
            }
        stats.append(row)

    stats = sorted(stats, key=lambda x: x['TestsUsed'])

    scenarios = {}
    for d in stats:
        key = str(toScenario(d))
        if not(key in scenarios):
            scenarios[key] = { 'dict': d , 'values': [] }

        scenarios[key]['values'].append(d)

    # Data for plotting
    fig, ax = plt.subplots()
    for key in scenarios:
        scenario = scenarios[key]
        if not(isTarget(scenario['dict'])):
            continue
        drawGraph(ax, scenario)


    ax.set(
        xlabel='Tests used',
        ylabel='Probability',
        title='Amount of tests needed on a population of ' + str(stats[0]["Population"]) + ' subjects'
    )

    ax.yaxis.set_major_formatter(ticker.PercentFormatter(xmax=1))
    plt.legend(loc='lower right')
    plt.grid()
    plt.show()

