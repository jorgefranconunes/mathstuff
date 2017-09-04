# -*- coding: utf-8 -*-
#
#
import matplotlib.pyplot as plt
import numpy as np
import sys


#
#
#
def main(args):

    plt.rc('font', family='serif')
    plt.rc('text', usetex=True)

    startX = -8
    endX = 8
    pointCount = 256

    x = np.linspace(startX, endX, pointCount)
    y = np.sin(x)

    plotAndSave(
        x, y,
        xlabel = r'Angle [rad]',
        ylabel = r'$\sin x$',
        title = r'Just a function plot',
        file = "MyDemoPlot.svg")


def plotAndSave(xs, ys, **params):

    figure, axes = plt.subplots(1,1)

    axes.set_xlabel(params["xlabel"])
    axes.set_ylabel(params["ylabel"])
    axes.set_title(params["title"])
    axes.grid(True)
    axes.plot(xs, ys)
    figure.savefig(params["file"], format="svg")



#
#
#
if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        sys.exit(1)
