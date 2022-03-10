from audioop import avg
from matplotlib import pyplot as plt
import numpy as np

if __name__ == "__main__":
    K = np.array([5, 8, 10, 20, 25])
    # local_d = np.array([0.6752, 0.9732, 1.1607, 1.4521, 1.6572])
    avg_d = np.array([0.2265, 0.2342, 0.2473, 0.2678, 0.3033])
    reboot_d = np.array([0.2402, 0.2480, 0.2498, 0.2558, 0.2658])
    cmirt_d = np.array([0.2214] * 5)
    plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
    plt.plot(K, avg_d, marker='+', label="AVGM estimator")
    # plt.plot(K, local_d, marker='*', label="Local estimator")
    plt.legend()
    plt.xlabel("Number of Local Machine (m)")
    plt.title("L2-distance vs. m (number of machines)")
    plt.savefig("plots/Rasch_plot/Rasch-1000-1.png")
    plt.close()

    K = np.array([10, 25, 50, 100, 125])
    local_d = np.array([0.4593, 0.7362, 0.9788, 1.3842, 1.60])
    avg_d = np.array([0.1424, 0.1462, 0.1631, 0.1703, 0.1962])
    reboot_d = np.array([0.1473, 0.1488, 0.1562, 0.1587, 0.1741])
    cmirt_d = np.array([0.1316] * 5)
    plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
    plt.plot(K, avg_d, marker='+', label="AVGM estimator")
    # plt.plot(K, local_d, marker='*', label="Local estimator")
    plt.legend()
    plt.xlabel("Number of Local Machine (m)")
    plt.title("L2-distance vs. m (number of machines)")
    plt.savefig("plots/Rasch_plot/Rasch-2500-1.png")
    plt.close()
