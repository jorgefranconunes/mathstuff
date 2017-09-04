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

    mu, sigma = 0, 1.0
    pointCount = 1024

    xs, ys = gaussianNoise(mu, sigma, pointCount)
    sampleCumSum = np.cumsum(ys)

    plotAndSave(
        ys,
        ylabel = r'Noise',
        title = r'Gaussian noise',
        file = "MyRandomPlot.svg")

    plot2AndSave(
        xs, ys, sampleCumSum,
        ylabelTop = r'Noise',
        ylabelBottom = r'Brownian process',
        title = r'Gaussian noise',
        file = "MyBrownianProcess.svg")


def gaussianNoise(mu, sigma, pointCount):

    sigmaPerSample = np.sqrt(sigma**2 / (pointCount-1))
    samples = np.random.normal(mu, sigmaPerSample, pointCount)
    xs = np.linspace(0.0, 1.0, pointCount)

    return (xs, samples)


def plotAndSave(ys, **params):

    figure, axes = plt.subplots(1, 1)

    axes.set_ylabel(params["ylabel"])
    axes.set_title(params["title"])
    axes.grid(True)
    axes.plot(ys)
    figure.savefig(params["file"], format="svg")


def plot2AndSave(xs, y1s, y2s, **params):

    figure, [axesTop, axesBottom] = plt.subplots(2, 1, sharex=True)

    axesTop.set_ylabel(params["ylabelTop"])
    axesTop.set_title(params["title"])
    axesTop.grid(True)
    axesTop.plot(xs, y1s)

    axesBottom.set_ylabel(params["ylabelBottom"])
    axesBottom.grid(True)
    axesBottom.plot(xs, y2s)

    figure.savefig(params["file"], format="svg")



#
#
#
if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        sys.exit(1)
