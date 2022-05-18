from matplotlib import pyplot as plt
# from matplotlib.lines import _LineStyle
import numpy as np
from icecream import ic
import sys
from pathlib import Path

ROOT_PATH = "plots/Rasch_plot/"


def plt_surr_loss__perturbation(fname="plt1"):
    # mu = np.array([-0.05, -0.04, -0.03, -0.02, -0.01,
    #               0, 0.01, 0.02, 0.03, 0.04, 0.05])
    # cmirt = np.array([0.1344, 0.1024, 0.0864, 0.0785, 0.08012, 0.078703, 0.07871, 0.078421, 0.097792,
    #                  0.11339, 0.11342])
    # mle = np.array(
    #     [0.1734, 0.1452, 0.0976, 0.0835, 0.0821, 0.08398, 0.08384, 0.083745, 0.10341, 0.13887, 0.16234])
    mu = np.array([-0.10, -0.05, -0.04, -0.03, -0.02, -0.01,
                  0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.10])
    mle = np.array([0.28547, 0.145494, 0.109719, 0.097011, 0.081244, 0.082174,
                   0.08398, 0.082857, 0.079835, 0.101798, 0.121524, 0.146233, 0.320385])
    cmirt = np.array([0.283453, 0.143143, 0.1044, 0.095617, 0.076075, 0.074297,
                     0.078703,  0.074191, 0.076098, 0.096886, 0.108359, 0.144578, 0.31932])
    delta = mle - cmirt
    plt.plot(mu[1:-1], cmirt[1:-1], marker='o', label="MIRT Estimator")
    plt.plot(mu[1:-1], mle[1:-1], marker='s', label="surr_loss MLE estimator")
    # plt.plot(mu[1:-1], delta[1:-1], marker="o",
    #          label="Gap between MIRT and surr_loss estimator")
    # y_line = np.zeros_like(mu[1:-1])
    # plt.plot(mu[1:-1], y_line)
    plt.legend()
    # plt.xlim(xmin=10)
    # plt.ylim(ymin=0.1, ymax=0.7)
    plt.xlabel("Mean")
    plt.xticks(mu[1:-1], mu[1:-1])
    plt.title("MSE vs. Mean")
    plt.savefig(ROOT_PATH + fname + ".png")
    plt.close()


if __name__ == "__main__":
    ic(f"Current executing file: {Path(__file__).name}")
    plt_surr_loss__perturbation("Rasch-Sensitivity-Analysis")
