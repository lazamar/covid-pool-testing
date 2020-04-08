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

def colId(scenario):
    return scenario['dict']['InfectionRate']

def colNumber(columns, scenario):
    return columns[colId(scenario)]

# Create a mapping from colId to column number
def createColumns(scenarios):
    maxCol=0
    columns = {}
    for _, scenario in scenarios.iteritems():
        col = colId(scenario)
        if not(col in columns):
            maxCol += 1
            columns[col] = maxCol
    return (maxCol, columns)

def rowId(scenario):
    return str(scenario['dict']['GroupSize']) + str(scenario['dict']['TreeDegree'])

def rowNumber(rows, scenario):
    return 1 + rows[colId(scenario)].index(rowId(scenario))

# Create a mapping from colId rowId to row number
def createRows(scenarios, columns):
    maxCol=1
    row_sets = {}
    for _, scenario in scenarios.iteritems():
        col = colId(scenario)
        row = rowId(scenario)
        if not(col in row_sets ):
            row_sets[col] = set([])
        row_sets[col].add(row)


    rows = {}
    maxRow = 0
    for col, vals in row_sets.iteritems():
        rows[col] = list(vals)
        maxRow = max(maxRow, len(vals))

    return (maxRow, rows)


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


    (maxCol, columns) = createColumns(scenarios)
    (maxRow, rows)    = createRows(scenarios, columns)

    # Data for plotting
    fig, ax = plt.subplots()
    for key, scenario in scenarios.iteritems():
        ix = colNumber(columns, scenario) + (rowNumber(rows, scenario) - 1) * maxCol
        ax = plt.subplot(maxRow, maxCol, ix)
        ax.set(
            title='Group of '
                + str(scenario['dict']["GroupSize"])
                + ', tree degree of '
                + str(scenario['dict']["TreeDegree"])
                + ', infection rate of'
                + str(scenario['dict']["InfectionRate"])
        )
        ax.yaxis.set_major_formatter(ticker.PercentFormatter(xmax=1))
        drawGraph(ax, scenario)


    plt.legend(loc='lower right')
    plt.grid()
    plt.show()

