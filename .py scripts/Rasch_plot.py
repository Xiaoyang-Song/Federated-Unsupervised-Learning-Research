from matplotlib import pyplot as plt
import numpy as np

K = np.array([5, 8, 10, 20, 25, 50])
local_d = np.array([])
avg_d = np.array([0.6935, 0.5803, 0.4221, 0.3803, 0.3433, 0.3228])
reboot_d = np.array([0.5343, 0.4229, 0.3176, 0.2620, 0.2381, 0.2075])
cmirt_d = np.array([0.5038, 0.3948, 0.2582, 0.2210, 0.2105, 0.1780])
plt.plot(K, local_d, marker='*', label="Local estimator")
plt.plot(K, reboot_Slopes, marker='o', label="REBOOT estimator")
plt.plot(K, cmirt_Slopes, marker='x', label="Full-sample estimator")
plt.plot(K, average_Slopes, marker='+', label="Average estimator")
plt.legend()
plt.xlabel("Number of Local Machine (m)")
plt.title("F-Norm vs. m")
plt.savefig("Rasch-1000.png")
plt.close()
