# Create a graph from a CSV with time data
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams
import math

rcParams['font.family'] = 'serif'
linewidth=0.9

# Worst case scenario with binary distribution
def worst_case_binary(n):
    # max nodes on the path from the root to infected leaves
    def atLevel(level, infected):
        levelNodes = math.pow(2, level)
        return min(levelNodes, infected)

    def nb(infected):
        treeLevels = math.ceil(math.log(n, 2)) + 1;

        testsNeeded = 0
        for level in np.arange(0, treeLevels):
            lInfected = atLevel(level, infected)

            # Nodes at this level that need testing but would
            # be tested negative.
            if level == 0:
                lClear = 0
            else:
                lClear = 2*atLevel(level - 1, infected) - lInfected

            lTests    = lInfected + lClear
            testsNeeded += lTests

        return testsNeeded

    # Data for plotting
    population = n
    infected   = np.arange(1, population, 1)
    tests      = [nb(i) for i in infected]

    fig, ax = plt.subplots()
    ax.plot(infected, tests)

    ax.set(
        xlabel='Infected subjects',
        ylabel='Tests needed',
        title='Worst case tests needed using binary distribution on '
                + str(population)
                + ' subjects'
    )
    ax.grid()
    fig.savefig("plots/worst-case-binary.png")
    plt.show()

worst_case_binary(1024)
