from audioop import avg
from matplotlib import pyplot as plt
import numpy as np

# GLOBAL PARAMETERS
ESTIMATOR = np.array(["AVGM", "LOCAL", "CMIRT", "REBOOT", "DPCA"])
PLOT_MARKER = np.array(['o', 'x', '+', '*', '∆'])
CONFIG = {
    "path": "plots/Rasch_plot/Rasch-1000.png",
    "x_label": "Number of Local Machine (m)",
    "title": "F-Norm vs. m"
}


def plot_estimators(est_type: list, est: np.ndarray, K: list, markers: list, CONFIG: dict):
    """
    Plot all the estimators that are included in the arguments.
    """
    assert len(est_type) == len(est) == len(
        markers), "The input lists should be of the same length"
    assert len(K) == est.shape[1], "The number of elements in estimators \
        should match the number of machines in the experiments."
    for estimator, idx in enumerate(est):
        plt.plot(K, estimator, marker=markers[idx], label=est_type[idx])
    plt.legend()
    plt.xlabel(CONFIG["x_label"])
    plt.title(CONFIG["title"])
    plt.savefig(CONFIG["path"])
    plt.close()


if __name__ == "__main__":
    K = np.array([10, 20, 25, 40, 50])
    local_d = np.array([0.5316, 0.7024, 0.8468, 1.1301, 1.1702])
    avg_d = np.array([0.2624, 0.2652, 0.2703, 0.2741, 0.2819])
    reboot_d = np.array([0.2638, 0.2674, 0.2693, 0.2700, 0.2745])
    cmirt_d = np.array([0.257377] * 5)
    estimators = np.array([avg_d, local_d, cmirt_d, reboot_d])
    # TODO: Change argument types of this plotting function.
    # plot_estimators(ESTIMATOR[0][[1, 2, 3, 4]], estimators, K,
    #                 PLOT_MARKER[0][[1, 2, 3, 4]], CONFIG)
    # plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    # plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
    # plt.plot(K, avg_d, marker='+', label="Average estimator")
    # plt.plot(K, local_d, marker='*', label="Local estimator")
    # plt.legend()
    # plt.xlabel("Number of Local Machine (m)")
    # plt.title("F-Norm vs. m")
    # plt.savefig("plots/Rasch_plot/Rasch-1000.png")
    # plt.close()
