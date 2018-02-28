ebench
=====

ebench is a utility to run and analyze benchmarks. Short example:

First run the benchmark using the default emulator settings.

```
$ _build/default/bin/ebench run -b decode -b tak -t BASE
Benchmarking using erl 
 Class: small
  decode...           537.718 ± 28.3053 ms, moderate outlier variance.
  tak...              4.68113 ± 0.1541 s, moderate outlier variance.
Results saved into BASE-2018-02-28T10-58-47.term
```

Then run the same benchmarks using different emulator start options.
```
$ _build/default/bin/ebench run -b decode -b tak -t MEANMIN -- erl +Meamin
Benchmarking using erl +Meamin
 Class: small
  decode...           513.369 ± 13.0847 ms, moderate outlier variance.
  tak...              4.05445 ± 0.0037 s, moderate outlier variance.
Results saved into MEANMIN-2018-02-28T10-59-57.term
```

Then compare the benchmarks using eminitstat:x/3. The most interesting part
of the output is the last bit, `No difference proven at 95.0% confidence`.
This means that even if mean of the MEAMIN run was smaller than that of the
base run, the values from the benchmark were too scattered to give a certain
verdict.

```
$ _build/default/bin/ebench eministat -b decode BASE MEANMIN
x BASE-small-decode
+ MEANMIN-small-decode
+--------------------------------------------------------------------------+
|+       x            +     +                        x              x      |
|            |_____________________________A_________M____________________||
|  |_____________A____M________|                                           |
+--------------------------------------------------------------------------+
Dataset: x N=3 CI=95.0000
Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
Min:         5.06075e+5
1st Qu.      5.06075e+5
Median:      5.46452e+5
3rd Qu.      5.46452e+5
Max:         5.60627e+5
Average:     5.37718e+5 [      131.984] (   5.06075e+5 ‥    5.55902e+5)
Std. Dev:    2.83053e+4 [     -7898.81] (   0.00000e+0 ‥    3.14956e+4)

Outliers: 0/0 = 0 (μ=5.37850e+5, σ=2.04065e+4)
	Outlier variance:      0.222222 (moderate)

------

Dataset: + N=3 CI=95.0000
Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
Min:         4.98641e+5
1st Qu.      4.98641e+5
Median:      5.17815e+5
3rd Qu.      5.17815e+5
Max:         5.23652e+5
Average:     5.13369e+5 [      157.092] (   4.98641e+5 ‥    5.21706e+5)
Std. Dev:    1.30847e+4 [     -3858.55] (   0.00000e+0 ‥    1.44401e+4)

Outliers: 0/0 = 0 (μ=5.13526e+5, σ=9226.19)
	Outlier variance:      0.222222 (moderate)

No difference proven at 95.0% confidence
------
```

We can also plot the benchmark runs in a histogram to compare the performance.

```
$ _build/default/bin/ebench plot BASE MEANMIN
plotted benchmark.svg
```

The ebench has a lot of different options, do get more information about them
pass the `-h` option to ebench.

```
$ _build/default/bin/ebench -h
Usage: ebench [<task>] [-h [<help>]]

  <task>      Task to run
  -h, --help  Print help for the given task [default: false]

ebench is a tool for benchmarking Erlang performance.

Several tasks are available:

run         Run the selected benchmarks
plot        Plot the results of benchmarks using gnuplot
eministat   Run eministat:x/3 to compare benchmark runs
list        List all available benchmarks

Run 'ebench <task> -h' for details about each task.
```


Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/ebench
