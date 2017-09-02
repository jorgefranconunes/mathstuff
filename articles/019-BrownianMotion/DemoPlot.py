#
#
#
import matplotlib.pyplot as plt
import numpy as np
import sys


#
#
#
def main(args):

    startX = -2*np.pi;
    endX = 2*np.pi;
    pointCount = 256;

    x = np.linspace(startX, endX, pointCount)
    y = np.sin(x)

    #plt.axis('tight')
    plt.xlabel("Angle [rad]")
    plt.ylabel(r"$\sin x$")
    plt.title("Just a function plot")

    plt.plot(x, y)
    plt.show();


#
#
#
if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        sys.exit(1)
