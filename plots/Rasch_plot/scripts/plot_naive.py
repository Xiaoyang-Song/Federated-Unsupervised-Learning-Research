from matplotlib import pyplot as plt
# from matplotlib.lines import _LineStyle
import numpy as np

if __name__ == "__main__":
    # K = np.array([5, 8, 10, 20, 25])
    # # local_d = np.array([0.6752, 0.9732, 1.1607, 1.4521, 1.6572])
    # avg_d = np.array([0.2265, 0.2342, 0.2473, 0.2678, 0.3033])
    # reboot_d = np.array([0.2402, 0.2480, 0.2498, 0.2558, 0.2658])
    # cmirt_d = np.array([0.2214] * 5)
    # plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    # plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
    # plt.plot(K, avg_d, marker='+', label="AVGM estimator")
    # # plt.plot(K, local_d, marker='*', label="Local estimator")
    # plt.legend()
    # plt.xlabel("Number of Local Machine (m)")
    # plt.title("L2-distance vs. m (number of machines)")
    # plt.savefig("plots/Rasch_plot/Rasch-1000-1.png")
    # plt.close()
    # Plot: 3/9 meeting.
    # K = np.array([10, 25, 50, 100, 125])
    # local_d = np.array([0.4593, 0.7362, 0.9788, 1.3842, 1.60])
    # avg_d = np.array([0.1424, 0.1462, 0.1631, 0.1703, 0.1962])
    # reboot_d = np.array([0.1473, 0.1488, 0.1562, 0.1587, 0.1741])
    # cmirt_d = np.array([0.1316] * 5)
    # plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    # plt.plot(K, cmirt_d, marker='x', label="Full-sample estimator")
    # plt.plot(K, avg_d, marker='+', label="AVGM estimator")
    # # plt.plot(K, local_d, marker='*', label="Local estimator")
    # plt.legend()
    # plt.ylim(ymin=0)
    # plt.xlabel("Number of Local Machine (m)")
    # plt.title("L2-distance vs. m (number of machines)")
    # plt.savefig("plots/Rasch_plot/Rasch-2500-1.png")
    # plt.close()

    # PLOT 3/31 MEETING
    # K = np.array([10, 20, 40, 50, 80, 100, 125])
    # local_d = np.array(
    #     [0.5643, 0.7146, 1.0318, 1.3842, 1.6472, 1.7794, 2.1201])
    # avg_d = np.array([0.1624, 0.1742, 0.1827, 0.2111, 0.2235, 0.2478, 0.2652])
    # reboot_d = np.array(
    #     [0.1783, 0.1799, 0.1919, 0.1996, 0.2075, 0.2207, 0.2314])
    # cmirt_d = np.array([0.1625] * 7)
    # plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    # plt.plot(K, cmirt_d, marker='s', label="Full-sample estimator")
    # plt.plot(K, avg_d, marker='^', label="AVGM estimator")
    # # plt.plot(K, local_d, marker='*', label="Local estimator")
    # plt.legend()
    # # plt.xlim(xmin=10)
    # # plt.ylim(ymin=0, ymax=0.4)
    # plt.xlabel("Number of Local Machine (m)")
    # plt.xticks(K, K)
    # plt.title("L2-distance vs. m (number of machines)")
    # plt.savefig("plots/Rasch_plot/Rasch-4000.png")
    # plt.close()

    # K = np.array([5, 10, 15, 20, 25, 30, 35, 40])
    # # local_d = np.array(
    # #     [0.5643, 0.7146, 1.0318, 1.3842, 1.6472, 1.7794, 2.1201])
    # avg_d = np.array([0.5932, 0.5211, 0.4836, 0.4623,
    #                  0.4180, 0.394, 0.3698, 0.3401])
    # reboot_d = np.array(
    #     [0.6321, 0.5476, 0.5029, 0.4553, 0.3764, 0.3272, 0.2653, 0.2431])
    # cmirt_d = np.array([0.5012, 0.4887, 0.4632, 0.4226,
    #                    0.3312, 0.2843, 0.2214, 0.2034])
    # plt.plot(K, reboot_d, marker='o', label="REBOOT estimator")
    # plt.plot(K, cmirt_d, marker='s', label="Full-sample estimator")
    # plt.plot(K, avg_d, marker='^', label="AVGM estimator")
    # # plt.plot(K, local_d, marker='*', label="Local estimator")
    # plt.legend()
    # # plt.xlim(xmin=10)
    # plt.ylim(ymin=0.1, ymax=0.7)
    # plt.xlabel("Number of Local Machine (m)")
    # plt.title("L2-distance vs. m (number of machines)")
    # plt.savefig("plots/Rasch_plot/Rasch-75.png")
    # plt.close()

    # PLOT 04/14 meeting
    # N = np.array([200, 500, 1000, 1500, 2000, 2500, 3000])
    # # local_d = np.array(
    # #     [0.5643, 0.7146, 1.0318, 1.3842, 1.6472, 1.7794, 2.1201])
    # cmirt = np.array([0.29598, 0.11764, 0.06227,
    #                  0.04111, 0.03287, 0.02444, 0.01981])
    # mle = np.array(
    #     [0.33742, 0.13895, 0.07302, 0.04915, 0.03885, 0.03029, 0.02555])
    # plt.plot(N, cmirt, marker='o', label="MIRT Estimator")
    # plt.plot(N, mle, marker='s', label="surr_loss MLE estimator")
    # plt.legend()
    # # plt.xlim(xmin=10)
    # # plt.ylim(ymin=0.1, ymax=0.7)
    # plt.xlabel("Global Sample Size")
    # plt.xticks(N, N)
    # plt.title("MSE vs. Global Sample Size")
    # plt.savefig("plots/Rasch_plot/mle-10.png")
    # plt.close()
    # # pass
    # N = np.array([200, 500, 1000, 1500, 2000, 2500, 3000])
    # cmirt = np.array([0.58937, 0.24064, 0.12496,
    #                  0.08352, 0.06146, 0.05049, 0.0381])
    # mle = np.array(
    #     [0.66540, 0.28375, 0.14785, 0.09904, 0.07494, 0.06128, 0.04892])
    # plt.plot(N, cmirt, marker='o', label="MIRT Estimator")
    # plt.plot(N, mle, marker='s', label="surr_loss MLE estimator")
    # plt.legend()
    # # plt.xlim(xmin=10)
    # # plt.ylim(ymin=0.1, ymax=0.7)
    # plt.xlabel("Global Sample Size")
    # plt.xticks(N, N)
    # plt.title("MSE vs. Global Sample Size")
    # plt.savefig("plots/Rasch_plot/mle-20.png")
    # plt.close()

    # N = np.array([200, 500, 1000, 1500, 2000, 2500, 3000])
    # cmirt = np.array([0.12341, 0.08762, 0.07412, 0.05809,
    #                  0.05231, 0.04741, 0.03871])
    # cmirt_10 = np.array(
    #     [0.2842, 0.2234, 0.1659, 0.1290, 0.100, 0.0854, 0.0443])
    # mle_10 = np.array([0.3197, 0.2488, 0.2046, 0.1417, 0.123, 0.098, 0.0661])
    # # # mle = np.array(
    # # #     [1.67656, 0.71255, 0.37733, 0.23985, 0.19077, 0.14916, 0.12225])
    # mle = np.array([0.14539, 0.10745, 0.08253,
    #                0.06975, 0.06152, 0.04952, 0.04097])
    # plt.plot(N, cmirt, marker='o', label="MIRT Estimator")
    # plt.plot(N, mle, marker='s', label="surr_loss MLE estimator")
    # plt.legend()
    # # plt.xlim(xmin=10)
    # # plt.ylim(ymin=0.1, ymax=0.7)
    # plt.xlabel("Global Sample Size")
    # plt.xticks(N, N)
    # plt.title("MSE vs. Global Sample Size")
    # plt.savefig("plots/Rasch_plot/mirt-mle-5.png")
    # plt.close()

    # N = np.array([200, 500, 1000, 1500, 2000, 2500, 3000])
    N = np.array([50, 100, 200, 300, 400, 500, 750, 1000])

    mhrm_err = np.array([1.0717, 0.9133, 0.6539, 0.5189,
                        0.493352, 0.3858, 0.317907, 0.2998])
    cjmle_err = np.array(
        [0.7238, 0.5196, 0.4389, 0.3963, 0.39406, 0.4167, 0.3931, 0.3484])
    # cmirt = np.array([0.7302, 0.4387, 0.3205, 0.2613,
    #                  0.2266, 0.2021, 0.1867])
    # cmirt_10 = np.array(
    #     [0.2842, 0.2234, 0.1659, 0.1290, 0.100, 0.0854, 0.0443])
    # mle_10 = np.array([0.3197, 0.2488, 0.2046, 0.1417, 0.123, 0.098, 0.0661])
    # # mle = np.array(
    # #     [1.67656, 0.71255, 0.37733, 0.23985, 0.19077, 0.14916, 0.12225])
    # mle = np.array([1.1703, 1.1532, 1.2643, 1.2198, 1.1265, 1.1176, 1.1536])
    plt.plot(N, mhrm_err, marker='o', label="MH-RM")
    plt.plot(N, cjmle_err, marker='s', label="CJMLE")
    plt.legend()
    # plt.xlim(xmin=10)
    # plt.ylim(ymin=0.1, ymax=0.7)
    plt.xlabel("Global Sample Size")
    plt.xticks(N, N)
    plt.title("SinTheta Distances vs. Global Sample Size")
    plt.savefig("plots/CJMLE/mhrm-cjmle.png")
    plt.close()
