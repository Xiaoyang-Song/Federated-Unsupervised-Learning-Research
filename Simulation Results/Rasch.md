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

Regime I: Fixed local sample size = 100.
Hidden Dimension: 50.

Number of local machines (Global sample size): m = 5 (500), 10(1000), 15(1500), 20(2000), 25(2500), 30(3000).

The experiments are run via the GreakLake supercomputing platform:

Output file name: <num*dimension>*<num*local_machine>*<num_local_sample_size>

Regime II: Fixed global sample size = 4000.

Hidden Dimension: 20.

Number of local machines: 10, 20, 40, 50, 80, 100
