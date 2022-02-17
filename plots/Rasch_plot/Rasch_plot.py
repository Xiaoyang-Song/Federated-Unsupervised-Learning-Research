from matplotlib import pyplot as plt
import numpy as np

K = np.array([5, 8, 10, 20, 25])
local_d = np.array([0.5316, 0.7024, 0.8468, 1.1301, 1.1702])
avg_d = np.array([0.2624, 0.2652, 0.2703, 0.2741, 0.2819])
reboot_d = np.array([0.2638, 0.2674, 0.2693, 0.2700, 0.2745])
cmirt_d = np.array([0.257377] * 5)
# plt.plot(K, local_d, marker='*', label="Local estimator")
plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
plt.plot(K, avg_d, marker='+', label="Average estimator")
plt.legend()
plt.xlabel("Number of Local Machine (m)")
plt.title("F-Norm vs. m")
plt.savefig("plots/Rasch_plot/Rasch-1000.png")
plt.close()
