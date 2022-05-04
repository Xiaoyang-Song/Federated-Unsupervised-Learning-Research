from matplotlib import pyplot as plt
# from matplotlib.lines import _LineStyle
import numpy as np
from icecream import ic
import sys
from pathlib import Path

ROOT_PATH = "plots/Rasch_plot/"

def plt_surr_loss__perturbation(fname = "plt1"):
    N = np.array([200, 500, 1000, 1500, 2000, 2500, 3000])
    cmirt = np.array([1.48699, 0.59634, 0.31882,
                     0.20567, 0.16459, 0.12434, 0.09603])
    mle = np.array(
        [1.67656, 0.71255, 0.37733, 0.23985, 0.19077, 0.14916, 0.12225])
    plt.plot(N, cmirt, marker='o', label="MIRT Estimator")
    plt.plot(N, mle, marker='s', label="surr_loss MLE estimator")
    plt.legend()
    # plt.xlim(xmin=10)
    # plt.ylim(ymin=0.1, ymax=0.7)
    plt.xlabel("Global Sample Size")
    plt.xticks(N, N)
    plt.title("MSE vs. Global Sample Size")
    plt.savefig(ROOT_PATH + fname + ".png")
    plt.close()


if __name__ == "__main__":
    ic(f"Current executing file: {Path(__file__).name}")
    plt_surr_loss__perturbation("0504")
