from matplotlib import pyplot as plt
# from matplotlib.lines import _LineStyle
import numpy as np
from icecream import ic
import sys
from pathlib import Path

ROOT_PATH = "plots/Rasch_plot/"


def plt_surr_loss__perturbation(fname="plt1"):
    mu = np.array([-0.05, -0.04, -0.03, -0.02, -0.01,
                  0, 0.01, 0.02, 0.03, 0.04, 0.05])
    cmirt = np.array([0.1344, 0.1024, 0.0864, 0.0785, 0.08012, 0.078703, 0.07871, 0.078421, 0.097792,
                     0.11339, 0.11342])
    mle = np.array(
        [0.1734, 0.1452, 0.0976, 0.0835, 0.0821, 0.08398, 0.08384, 0.083745, 0.10341, 0.13887, 0.16234])
    delta = mle - cmirt
    # plt.plot(mu, cmirt, marker='o', label="MIRT Estimator")
    # plt.plot(mu, mle, marker='s', label="surr_loss MLE estimator")
    plt.plot(mu, delta, marker= "o", label="Gap between MIRT and surr_loss estimator")
    plt.legend()
    # plt.xlim(xmin=10)
    # plt.ylim(ymin=0.1, ymax=0.7)
    plt.xlabel("Mean")
    plt.xticks(mu, mu)
    plt.title("MSE vs. Mean")
    plt.savefig(ROOT_PATH + fname + ".png")
    plt.close()


if __name__ == "__main__":
    ic(f"Current executing file: {Path(__file__).name}")
    plt_surr_loss__perturbation("05042")
