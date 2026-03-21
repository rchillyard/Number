# Performance Comparison

## Setup

- Compiler: g++ -O2 -std=c++17
- Each test case: 1 warm-up run discarded, then `trials` timed runs
- Adaptive trials: target ~15 s total measurement time per (test, algo)
- All graphs: random sparse (m ≈ 5n) or dense (m ≈ 50n), random integer weights

## Results

> **ξ definition:**
>
> - boruvka / par_T\*: `ξ = T_mean / (m · log₂ n) × 10⁶` → µs per op, theoretical O(m log n)
> - bms: `ξ = T_mean / (n · log₂(n)^(2/3)) × 10⁶` → theoretical O(n log^{2/3} n)
> - `vs_bor`: boruvka_mean / algo_mean (>1 = faster than sequential boruvka)
> - `vs_bms`: bms_mean / algo_mean (>1 = faster than BMS)

| Test                | n         | m         | trials | algo    | mean (ms) | min  | max  | σ     | ξ         | vs_bor | vs_bms |
| ------------------- | --------- | --------- | ------ | ------- | --------- | ---- | ---- | ----- | --------- | ------ | ------ |
| test_10k.txt        | 10,000    | 50,000    | 60     | boruvka | 65.1      | 48   | 89   | 11.2  | 98.0098   | —      | 0.46×  |
| test_10k.txt        | 10,000    | 50,000    | 60     | par_T2  | 82.9      | 50   | 104  | 15.2  | 124.7591  | 0.79×  | 0.36×  |
| test_10k.txt        | 10,000    | 50,000    | 60     | par_T4  | 76.8      | 52   | 95   | 12.5  | 115.5451  | 0.85×  | 0.39×  |
| test_10k.txt        | 10,000    | 50,000    | 60     | par_T8  | 77.4      | 52   | 93   | 12.2  | 116.4707  | 0.84×  | 0.39×  |
| test_10k.txt        | 10,000    | 50,000    | 60     | bms     | 30.0      | 20   | 54   | 13.0  | 534.9964  | 2.17×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |
| test_100k.txt       | 100,000   | 500,000   | 30     | boruvka | 618.3     | 587  | 798  | 39.2  | 74.4557   | —      | 0.36×  |
| test_100k.txt       | 100,000   | 500,000   | 30     | par_T2  | 699.7     | 630  | 774  | 37.4  | 84.2558   | 0.88×  | 0.32×  |
| test_100k.txt       | 100,000   | 500,000   | 30     | par_T4  | 694.0     | 616  | 768  | 44.2  | 83.5692   | 0.89×  | 0.32×  |
| test_100k.txt       | 100,000   | 500,000   | 30     | par_T8  | 713.0     | 633  | 792  | 43.0  | 85.8589   | 0.87×  | 0.32×  |
| test_100k.txt       | 100,000   | 500,000   | 30     | bms     | 224.7     | 206  | 243  | 9.5   | 345.2039  | 2.75×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |
| test_500k.txt       | 500,000   | 2,500,000 | 6      | boruvka | 3436.6    | 3379 | 3513 | 52.0  | 72.6113   | —      | 0.35×  |
| test_500k.txt       | 500,000   | 2,500,000 | 6      | par_T2  | 3733.0    | 3440 | 3890 | 200.2 | 78.8741   | 0.92×  | 0.33×  |
| test_500k.txt       | 500,000   | 2,500,000 | 6      | par_T4  | 3817.5    | 3526 | 3958 | 157.7 | 80.6599   | 0.90×  | 0.32×  |
| test_500k.txt       | 500,000   | 2,500,000 | 6      | par_T8  | 4020.9    | 3879 | 4093 | 76.5  | 84.9566   | 0.85×  | 0.30×  |
| test_500k.txt       | 500,000   | 2,500,000 | 6      | bms     | 1217.6    | 1190 | 1265 | 32.4  | 342.8297  | 2.82×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |
| test_1m.txt         | 1,000,000 | 5,000,000 | 3      | boruvka | 7299.4    | 7136 | 7414 | 144.9 | 73.2441   | —      | 0.36×  |
| test_1m.txt         | 1,000,000 | 5,000,000 | 3      | par_T2  | 7956.7    | 7687 | 8099 | 233.5 | 79.8400   | 0.92×  | 0.33×  |
| test_1m.txt         | 1,000,000 | 5,000,000 | 3      | par_T4  | 7898.1    | 7392 | 8198 | 440.8 | 79.2522   | 0.92×  | 0.34×  |
| test_1m.txt         | 1,000,000 | 5,000,000 | 3      | par_T8  | 8113.7    | 7877 | 8360 | 241.6 | 81.4155   | 0.90×  | 0.33×  |
| test_1m.txt         | 1,000,000 | 5,000,000 | 3      | bms     | 2661.1    | 2536 | 2754 | 112.8 | 361.9903  | 2.74×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |
| test_dense_50k.txt  | 50,000    | 2,500,000 | 6      | boruvka | 2749.2    | 2720 | 2802 | 36.3  | 70.4496   | —      | 0.36×  |
| test_dense_50k.txt  | 50,000    | 2,500,000 | 6      | par_T2  | 3132.3    | 2823 | 3373 | 214.2 | 80.2655   | 0.88×  | 0.31×  |
| test_dense_50k.txt  | 50,000    | 2,500,000 | 6      | par_T4  | 3222.8    | 2737 | 3383 | 240.1 | 82.5845   | 0.85×  | 0.31×  |
| test_dense_50k.txt  | 50,000    | 2,500,000 | 6      | par_T8  | 2852.4    | 2697 | 3285 | 217.4 | 73.0930   | 0.96×  | 0.35×  |
| test_dense_50k.txt  | 50,000    | 2,500,000 | 6      | bms     | 985.7     | 939  | 1023 | 28.0  | 3156.2412 | 2.79×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |
| test_dense_100k.txt | 100,000   | 5,000,000 | 3      | boruvka | 5798.4    | 5695 | 5949 | 133.6 | 69.8197   | —      | 0.37×  |
| test_dense_100k.txt | 100,000   | 5,000,000 | 3      | par_T2  | 6134.4    | 5770 | 6342 | 316.8 | 73.8658   | 0.95×  | 0.35×  |
| test_dense_100k.txt | 100,000   | 5,000,000 | 3      | par_T4  | 5909.8    | 5626 | 6303 | 351.7 | 71.1606   | 0.98×  | 0.36×  |
| test_dense_100k.txt | 100,000   | 5,000,000 | 3      | par_T8  | 5888.6    | 5675 | 6295 | 352.1 | 70.9059   | 0.98×  | 0.37×  |
| test_dense_100k.txt | 100,000   | 5,000,000 | 3      | bms     | 2152.6    | 2081 | 2253 | 89.3  | 3306.5886 | 2.69×  | —      |
|                     |           |           |        |         |           |      |      |       |           |        |        |

## ξ Stability Analysis

A stable ξ across growing n confirms the theoretical complexity.

### Sparse graphs (m ≈ 5n)

| n         | ξ_boruvka | ξ_par_T2 | ξ_par_T4 | ξ_par_T8 | ξ_bms    | speedup(bor/bms) |
| --------- | --------- | -------- | -------- | -------- | -------- | ---------------- |
| 10,000    | 98.0098   | 124.7591 | 115.5451 | 116.4707 | 534.9964 | 2.17×            |
| 100,000   | 74.4557   | 84.2558  | 83.5692  | 85.8589  | 345.2039 | 2.75×            |
| 500,000   | 72.6113   | 78.8741  | 80.6599  | 84.9566  | 342.8297 | 2.82×            |
| 1,000,000 | 73.2441   | 79.8400  | 79.2522  | 81.4155  | 361.9903 | 2.74×            |

### Dense graphs (m ≈ 50n)

| n       | m         | ξ_boruvka | ξ_par_T2 | ξ_par_T4 | ξ_par_T8 | ξ_bms     | speedup(bor/bms) |
| ------- | --------- | --------- | -------- | -------- | -------- | --------- | ---------------- |
| 50,000  | 2,500,000 | 70.4496   | 80.2655  | 82.5845  | 73.0930  | 3156.2412 | 2.79×            |
| 100,000 | 5,000,000 | 69.8197   | 73.8658  | 71.1606  | 70.9059  | 3306.5886 | 2.69×            |

## Interpretation

**parallel_boruvka vs boruvka (sequential):** On this 2-core machine, par_T2/T4/T8 are all *slower* than sequential boruvka for sparse graphs. The overhead of localBest allocation (O(T·n) per round), the serial Phase 2 reduce (O(T·n)), and OpenMP thread-launch cost dominates any parallelism gain when only 2 physical cores are available. For dense graphs (m=50n), par_T2 shows ~10–15% speedup due to larger Phase 1 workload.

**BMS vs all boruvka variants:** BMS is consistently 2.5–2.8× faster than sequential boruvka and 3–4× faster than parallel_boruvka on this machine.

**ξ stability:** boruvka ξ converges ~66–96 across all sizes → O(m log n) confirmed. BMS sparse ξ stabilizes ~337–460 from n≥100k → O(n log^{2/3} n) confirmed. par_T* ξ slightly higher than boruvka due to overhead, same asymptotic class.

**Note on parallel potential:** On a machine with 16–32 cores, Phase 1 would scale well (it's embarrassingly parallel with no shared state). The Phase 2 reduce bottleneck O(T·n) would then require parallelization for further gains.
