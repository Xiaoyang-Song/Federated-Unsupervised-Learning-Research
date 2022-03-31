# Experimental results of the simulation study on Rasch model.

### Rasch Simulation study results: (Sanity check of implementation)

Ground truth: [0, -0.5, 0.1, -0.4, 5, -0.3, 0.3, -0.2, 0.4, -0.1]

(Note that in most cases, the responses of item5 will be all from the same category)

[1] "Full-Sample Estimator:"\
[1] 0.2934478\
[1] "Local Estimator:"\
[1] 0.7649827\
[1] "Average Estimator:"\
[1] 0.3232812\
[1] "Reboot Estimator:"\
[1] 0.3075252

### Rasch Simulation with ground truth from uniform distribution:

#### Ground truth parameters:

Ground truth: [-0.1945, 0.1705, -0.0439, 0.0978, 0.1674, -0.1462, 0.07, -0.1645, 0.124, -0.04]

#### Results:

Global Sample size: N = 1000

| m      | 10     | 20     | 25     | 40     | 50     |
| ------ | ------ | ------ | ------ | ------ | ------ |
| LOCAL  | 0.6752 | 0.9732 | 1.1607 | 1.4521 | 1.6572 |
| AVGM   | 0.2265 | 0.2342 | 0.2473 | 0.2678 | 0.3033 |
| REBOOT | 0.2402 | 0.2480 | 0.2498 | 0.2528 | 0.2658 |
| CMIRT  | 0.2214 | 0.2214 | 0.2214 | 0.2214 | 0.2214 |

Global Sample size: N = 2500

| m      | 10     | 25     | 50     | 100    | 125    |
| ------ | ------ | ------ | ------ | ------ | ------ |
| LOCAL  | 0.4593 | 0.7362 | 0.9788 | 1.3842 | 1.60   |
| AVGM   | 0.1424 | 0.1462 | 0.1631 | 0.1703 | 0.1962 |
| REBOOT | 0.1473 | 0.1488 | 0.1587 | 0.1462 | 0.1741 |
| CMIRT  | 0.1316 | 0.1316 | 0.1316 | 0.1316 | 0.1316 |

### Rasch Simulation Study (2) (Updated on Mar 30)

Regime I: Fixed local sample size = 75. (this is obtained by local inspection and experiments)
Hidden Dimension: 50.

Number of local machines (Global sample size): m = 5, 10, 15, 20, 25, 30, 35, 40.

The experiments are run via the GreakLake supercomputing platform:

Output file name: <num*dimension>*<num*local_machine>*<num_local_sample_size>

Regime II: Fixed global sample size = 4000.

Hidden Dimension: 20

Number of local machines (m): 10, 20, 40, 50, 80, 100

Ground Truth Parameters:

[0.016 0.008 -0.050 0.003 0.082 -0.177 0.076 0.119 0.135 0.053 \
-0.099 -0.045 0.140 -0.124 -0.138 -0.187 -0.143 -0.025 0.049 0.042 \
-0.195 -0.127 0.161 0.073 0.002 -0.181 -0.130 0.143 -0.022 -0.033 \
-0.115 -0.115 0.150 -0.046 0.129 0.012 -0.131 -0.133 0.196 -0.135 \
-0.108 -0.020 0.180 -0.073 -0.088 0.132 -0.041 0.118 0.097 -0.112]

### Experimental Results:

#### Dimension = 20. Regime I: N = 4000.

| m      | 10     | 20     | 40     | 50     | 80     | 100    | 125    |
| ------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ |
| LOCAL  | 0.5643 | 0.7146 | 1.0318 | 1.3842 | 1.6472 | 1.7794 | 2.1201 |
| AVGM   | 0.1624 | 0.1742 | 0.1827 | 0.1703 | 0.2235 | 0.2478 | 0.2652 |
| REBOOT | 0.1783 | 0.1799 | 0.1919 | 0.1462 | 0.2038 | 0.2207 | 0.2314 |
| CMIRT  | 0.1625 | 0.1625 | 0.1625 | 0.1625 | 0.1625 | 0.1625 | 0.1625 |

Remark: the experiments of m=10, m=100 and m = 125 on greatlake is problematic because of warning. The result is rerun on local computer for inspection. Some iterations of the MH-RM algorithm do not converge, which lead to strange result.

#### Dimensiion = 50. Regime II: n = 75.

| m      | 5      | 10     | 15     | 20     | 25     | 30     | 35     | 40     |
| ------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ |
| AVGM   | 0.5932 | 0.5211 | 0.4836 | 0.4623 | 0.4180 | 0.394  | 0.3698 | 0.3401 |
| REBOOT | 0.6321 | 0.5476 | 0.5029 | 0.4553 | 0.3764 | 0.3272 | 0.2653 | 0.2431 |
| CMIRT  | 0.5012 | 0.4887 | 0.4632 | 0.4226 | 0.3312 | 0.2843 | 0.2214 | 0.2034 |

Note that all experiments for regime II are run on local computer with 25 Monte Carlo replications. Plan to run this on Greatlake with 100 Monte Carlo iterations later.
