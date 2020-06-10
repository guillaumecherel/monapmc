# Introduction 

This repository contains the numerical experiments to test the algorithm MonAPMC
[REF]. This algorithm is an attempt to implement an approximate Bayesian
computation (ABC) algorithm that scales well with the number of computing cores
available to perform simulations and the complexity of the simulation model. It
is essentially the algorithm APMC [REF] equipped with a specific parallelization
scheme.

The next section explains how to rerun the simulation experiments and reproduce
the figures described in the rest of the document. 

The ABC algorithms used in these experiments and the statistical computations
are written in haskell, under the directory <./formulas>. The figures are
generated with [gnuplot](http://www.gnuplot.info/) and relevant gnuplot scripts
are located in <./report>. One set of simulation that is particularly demanding
computationally (see `Run Comp LHS` below) is run on the European Grid
Infrastructure through the virtual organization `vo.complex-systems.eu`, using
the software [OpenMOLE](https://openmole.org). The openmole script is the file
`openmole/lhs.oms`.

A consistent naming scheme accross this descriptive document, the haskell code,
gnuplot script files and the `Makefile` helps keeping track of where the
various definitions below are defined in code and data files. All the
definitions found below are associated to a code file when they are defined in
code and to data files when they stored as data (input or output, e.g.
simulation outputs or statistics). For example, the statistics `Stats Comp` is
defined in haskell code in <./formulas/src/Experiment.hs> as the function
`statsComp` and `Stats Comp LHS` is stored as data in
<./output/formulas/figure_data/stats_comp_lhs>.

The figures are also named consistently with their names in this document and
stored in <./output/report>.


# Usage: how to reproduce the results?

Dependencies:
- stack (https://tech.fpcomplete.com/haskell/get-started/)
- openmole (https://openmole.org/), version 11, git commit e56207450dd8640edf0a25465a05527a1bcb46b7

Both executables need to be on the path.

The repository uses `make` to run the simulations, compute statistics and generate the figures automatically.

To run the simulation experiment defined by the OpenMOLE script
`openmole/lhs.oms`, you will need either to have credentials to use the European
Grid Infrastructure (EGI) through the virtual organization
`vo.complex-systems.eu` or use another execution environment by changing the
`env` value in the script.

You may want to run openmole manually rather than through make to access
openmole's web interface, through which you can watch the simulation's
progression on the distant computing cluster. To do that, ensure that all the
prerequisites are built with `make openmole/formulas.container.tgz`, then run
the script <./openmole/lhs.oms> yourself using openmole.

The resulting figures are to be found in <./output/report>.


# Definitions 

Here is a list of definitions given in the companion paper. This lists will help
the reader make the correspondance between the paper and this repository by
specifying where each definition is defined as code, stored as data or as an
image file when appropriate.

- Comp
  - Code <./formulas/src/Experiment.hs>

For a first check of the previous claims, we plot the evolution of L2 against
time for replicated runs of a pair `(apmc, mon-apmc)` of comparable algorithms APMC and MonAPMC,
with different parallelism levels and model run time variance 

    (apmc, mon-apmc) = comp(nGen, nAlpha, pAccMin, parallel, stepMax, biasFactor, meanRunTime, varRunTime)
    
where

    nGen = 4000, 
    nAlpha = 500,
    pAccMin = 0.01,
    parallel = 4 and 100,
    stepMax = 100,
    biasFactor = 1,
    meanRunTime = 1
    varRunTime = 0.1, 1 and 10

- Fig L2 vs time K V
  - Image file: <./output/report/fig_l2_vs_time_k_v.png> 
- Fig Time Bias
  - Histograms showing the posterior sample for both algorithms.
- LHS
  - Code: <./openmole/lhs.oms>.
- Run Comp LHS
  - Code: <./openmole/lhs.oms>.
  - Data files: One file per pair `(apmc, mon_apmc)`, one per simulation, in 
    <./output/formulas/run_comp_lhs/>. Each file is the 
    corresponding haskell value `(comp, (run_apmc, run_mon_apmc))` formatted 
    via its Show instance, such that it can be read again easily in haskell.
- L2 Ratio
  - Code def: <./formulas/src/Experiment.hs>.
- Time Ratio
  - Code def: <./formulas/src/Experiment.hs>.
- Stats Comp
  - Code def: <./formulas/src/Experiment.hs> 
- Stats Comp LHS 
  - Data files: <./output/formulas/figure_data/stats_comp_lhs>. 
    A table where each line corresponds to a point in the LHS, 7 columns give
    the values for each parameter to comp and six additionnal columns the l2
    error and runtime for each algorithm and l2 ratio and time ratio.  The
    first line gives the columns labels.
- Fig L2 vs biasFactor
  - Code: Gnuplot 
  - Image file: <./output/report/l2_vs_bias_factor.png>.
- Fig Scatter L2 Time LHS
  - Code: <./report/l2_time_lhs.gnuplot> 
  - Image file: <./output/report/l2_time_lhs.png>. 
- Fig L2 Effects LHS
  - Code: <./report/fig_l2_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_l2_effects_lhs.png>
- Fig L2 Ratio Effects LHS
  - Code: <./report/fig_l2_ratio_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_l2_ratio_effects_lhs.png>
- Fig Time Effects LHS
  - Code: <./report/fig_time_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_time_effects_lhs.png>
- Fig Time Ratio Effects LHS
  - Code: <./report/fig_time_ratio_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_time_ratio_effects_lhs.png>
- Test Cases
  - Code: <./Makefile>
- Stats Comp Test Cases
  - Code: <./formulas/src/Experiment.hs>
  - Data file: <./output/formulas/figure_data/stats_comp_test_cases>
- Fig Comp Test Cases
  - Code: <./report/fig_comp_test_cases.gnuplot>
  - Image file: <./output/report/fig_comp_test_cases.png>

# Directory structure

- input/
  - simu: simulation specifications for all the experiments
- output/
  - blocks/: results of the simulation implemented in scala.
  - easyABC/: results of the simulations from the R package EasyABC.
  - formulas/: results of the simulations and statistics implemented in haskell.
    - figure_data: input data for figures
    - run: simulation results
    - steps: simulation steps
    - repli/ 
      - run: replicated simulation results
      - steps: replicated simulation steps
  - report/: generated figures.
- report/: scripts generating the figures.
- sentinel/: sentinel files (used in the makefile).
- formulas/: haskell code implementing the algorithms, simulations and statistics.
- blocks/: scala implementation of the algorithms and simulations (currently not used).
- util/: utility scripts.


# Targets: 

FigSteps: histogram of posterior sample at each steps for an example run of APMC and MonAPMC

FigL2VSNSimu: L2 vs nSimus aggregated over replications (mean and std) of APMC and MonAPMC each with varying pAccMin, n, nAlpha

Fig0APMC: Series of L2 vs real time for some replications of APMC with varying K and default values for other parameters

Fig0MonAPMC: same as Fig0APMC with MonAPMC

Fig1K (K=1,2,5): scatter of T MonApmc / T Apmc vs K - r for replications of APMC and MonAPMC, varying N, Nalpha and pAccMin.
T X is the time it takes for algorithm X to reach the desired L2 threshold.

Fig3: Like Fig1K but scatter of T MonAPMC  / T Apmc vs Average Job Time / K.

Fig4: Like Fig1K but T MonAPMC / T Apmc vs Variance Job Time.


# Desired paths:

   (Simu apmc Toy, Simu monApmc Toy)
-> (Steps apmc Toy, Steps monApmc Toy)
-> (Histo Steps apmc, Histo Steps monApmc)
-> FigSteps

   (Simu Apmc1 Toy, ..., Simu MonApmc1 Toy, ...)
-> (Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...)
-> (Repli L2VsNSimu Apmc1, ..., Repli L2VsNSimu MonApmc1, ...)
-> (MeanStd L2VsNSimu Apmc1, ..., MeanStd L2VsNSimu MonApmc1, ...)
-> FigL2VSNSimu

   (Simu MonApmc(K=k) Toy | k=1,2,4)
-> (Repli Steps Res MonApmc(K=k) | k=1,2,4)
-> (Repli Steps L2VsRealTime MonApmc(K=k) | k=1,2,4)
-> Fig0Apmc

   (Apmc(K,N,Nalpha,pAccMin), MonApmc(K,N,Nalpha,pAccMin) 
   | Varying N, Nalpha, pAccMin)
-> (Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
   | Varying N, Nalpha, pAccMin)
-> (Repli (T MonApmc(K,N,Nalpha,pAccMin) / T Apmc(K,N,Nalpha,pAccMin) vs K - r)
   | Varying N, Nalpha, pAccMin)
-> Fig1K


Objectif: 
Construct each path to realise the corresponding numerical experiments and produce the desired figures. Since we want simulation results and final figures to be materialized as files on disk, we will will transform these paths into paths of the File category below, where objects are files on disk.

- all simulation results,
- all statistics used as input for figures
- all figures
 
 
# Categories

## Experiment Category

The experiment category is an abstract representation of the numerical experiments we want to realise. 

The only necessary objects and morphisms in the Experiment category are those appearing in the desired paths.

A morphism `f: A -> B` represents the step of the experiments that constructs object B. Alternatively, it means that `B` can be constructed if we have `A`. Morphism composition like `f . g` means that step g precedes step f.

Objects are the abstract elements whith which we construct our experiments:

- Figures: 
  - FigSteps, 
  - FigL2VSNSimu,
  - Fig0Apmc,
  - Fig1K.
- Algorithms: 
  - APMC n nAlpha pAccMin k, 
  - MonApmc n nAlpha pAccMin sss k.
- Models: 
  - Toy
- Simulations specification: 
  - Simu algo model
- Simulation results (final step): 
  - Res algo model, 
  - ResT algo Toy (res with L2 threshols)
- Simulation steps (final step): 
  - Steps algo model
- Stats: 
  - L2 simu,
  - NSimu simu,
  - RealTime simu,
  - T simu,
  - Histo Res simu,
  - Histo Steps simu.
- Replications: 
  - Repli a for any object a 
- Sequences: 
  - (a1, a1, ...) for any sequence of n objects

Morphisms:

- Simulation
  - simuToy: a -> Simu a Toy for any algorithm a
  - run: Simu x m -> Res x m for any algo x and model m
  - runT: Simu x m -> ResT x m for any algo x and model m
  - steps: Simu x m -> Steps x m
  - stepsT: Simu x m -> StepsT x m
- Statistics:
  - l2: Res x Toy -> L2 for any algo x
  - histo: Res x m -> Histo Res x m
  - l2VsNSimu: Res x Toy -> L2vsNSimu x
  - l2VsRealTime: Res x Toy -> L2VsRealTime x
  - meanStdL2VsNSimu: Repli L2vsNSimu x -> MeanStd (L2 vs NSimu) x
  - ResT x -> T x
  - Repli (ResT x1 Toy, ResT x2 Toy) -> T x1 / T x2 vs K - r
- Replications
  - 
- repliRun: Simu x m -> Repli Run x m
- repliSteps: Simu x m -> Repli Steps x m
- histoSteps: Steps x m -> Histo Steps x m





## Haskell code Category

In *our* Haskell category, an object is a value (not a type!). A morphism between two values a and b is a function applied to a that returns the value b. A single haskell function applied to two different values thus gives two different morphisms. 

Composition. Given two morphism `g: b -> c` and `f: a -> b` that respectively represent the haskell function composition `g(b)` and `f(a)`, the morphism `g . f` represents the haskell expression `(g . f) a`.


## Files category
 
In our Files category, an object is a file or a set of files or all files in a directory tree on disk . A morphism `f: A -> B` is an executable command that reads the file or files designated by object `A` and writes to the file(s) designated by `B`. It also writes the intermediate files x1, ..., xn.

Composition. Given two morphisms `g: A -> B` and `f: B -> C`, that respectively represent two commands `exe-g` and `exe-f`, the composition `h = f . g : A -> C` represent the consecutive execution of `exe-g` and `exe-f`.
 
We can easily construct morphisms operating on tuples or lists by combining other morphisms. For example a morphism `g : (A1, A2) -> (B1, B2)` can be obtained from the mophisms `f1 : A1 -> B1` and `f2 : A2 -> B2` simply by executing the two corresponding executables. We call this zipping and write `g = zip(f1, f2)`.


# Functors

To materialise the desired paths in the Experiment category, we construct below the functor `EF` that transform them into paths in the Files category. As a result, all objects will be materialized as files on disk and steps performed using executables.

Some of the morphisms or subpaths are easier to implement or already implemented in Haskell. We construct the functor `F` below to make use of them in the Files category.


## HF: Experiment -> File

Transform a morphism in the Experiment category into a morphism in the File category.

Objects. Each object in the Experiment category is associated to an object in the File category (a file or set of files) by formatting the appropriate data as text files such as CSV.

Morphism. Each morphism in the Experiment category is associated to an executable command, i.e. an object in the File category.

To construct this functor, we simply need to write a command for each morphism in the Experiment category desired paths. These are:

The simulation morphisms: 

```
simSteps:
   (Simu Apmc Toy, Simu MonApmc Toy)
-> (Steps Apmc Toy, Steps MonApmc Toy)
```

```
simL2
   (Simu Apmc1 Toy, ..., Simu MonApmc1 Toy, ...)
-> (Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...)
```

```
simMonApmcK
   (Simu MonApmc(K=k) Toy | k=1,2,4)
-> (Repli Steps Res MonApmc(K=k) | k=1,2,4)
```

```
simNAP
   (Apmc(K,N,Nalpha,pAccMin), MonApmc(K,N,Nalpha,pAccMin) 
   | Varying N, Nalpha, pAccMin)
-> (Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
   | Varying N, Nalpha, pAccMin)
```

These morphisms run simulations. Each of them run several independant simulations. We can assemble these morphisms from the morphisms running individual simulations by zipping them together, as described in the File category section. This grant more flexibility to run them in parallel using make. The individual simulation morphisms well be coded in haskell and imported as File morphisms with the F functor.

Statistics:

```
histoSteps:
   (Steps Apmc Toy, Steps MonApmc Toy)
-> (Histo Steps APMC, Histo Steps MonAPMC)
```

```
meanStdL2VsNSimus:
   (Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...)
-> (MeanStd L2VsNSimu Apmc1, ..., MeanStd L2VsNSimu MonApmc1, ...)
```

```
repliL2VsRealTime:
   (Repli Steps Res MonApmc(K=k) | k=1,2,4)
-> (Repli Steps L2VsRealTime MonApmc(K=k) | k=1,2,4)
```

```
repliTvsKR:
   (Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
   | Varying N, Nalpha, pAccMin)
-> (Repli (T MonApmc(K,N,Nalpha,pAccMin) / T Apmc(K,N,Nalpha,pAccMin) vs K - r)
   | Varying N, Nalpha, pAccMin)
```

These morphisms are coded in haskell and imported via the F functor.

Figure creation:

```
   (Histo Steps APMC, Histo Steps MonAPMC)
-> FigSteps
```

```
   (MeanStd L2VsNSimu Apmc1, ..., MeanStd L2VsNSimu MonApmc1, ...)
-> FigL2VSNSimu
```

```
   (Repli Steps L2VsRealTime MonApmc(K=k) | k=1,2,4)
-> Fig0Apmc
```

```
   (Repli (T MonApmc(K,N,Nalpha,pAccMin) / T Apmc(K,N,Nalpha,pAccMin) vs K - r)
   | Varying N, Nalpha, pAccMin)
-> Fig1K
```

The figure creation morphism will be implemented using gnuplot.


## F: Haskell code -> File.

In *our* Haskell category, an object is a value (not a type!).

A Haskell morphism is a function applied to the source object that gives as value the target object. We transform a Haskell morphism `h : A -> B` into a File morphism `f: F(A) -> F(B)` by: 

- writing a haskell function `readA: [FilePath] -> IO (Type of A)` that transforms text contained in the file(s) F(A) and returns a value A that has the appropriate data type.
- writing a haskell function `writeB: [FilePath] -> Type of B -> IO ()` that transforms a haskell value B into text and write it to the file(s) F(B).
- generating a haskell executable that evaluates `writeB outFiles . h . readA inFiles`. 

The executable is the morphism in the File category. For practical reason, we will generate a single executable "haskFile" that will take as parameter a name designating the appropriate haskell function to run and the input and output paths.

Given the uses of the F functor specified in the description of the functor `Experiment -> File`, we need to import the Haskell morphisms corresponding to individual simulations:

- `run(a,m): Simu a m -> Res a m` for any algorithm a and m,
- `steps(a,m): Simu a m -> Steps a m` for any algorithm a and m,
- `repliRun(a, m): Simu a m -> Repli Res a m`
- `repliSteps(a, m): Simu a m -> Repli Steps a m`

as well as the statistics computation morphisms:

```
histoSteps:
   (Steps Apmc Toy, Steps MonApmc Toy)
-> (Histo Steps APMC, Histo Steps MonAPMC)
```

```
MeanStdL2VsNSimus:
   (Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...)
-> (MeanStd L2VsNSimu Apmc1, ..., MeanStd L2VsNSimu MonApmc1, ...)
```

```
repliL2VsRealTime:
   (Repli Steps Res MonApmc(K=k) | k=1,2,4)
-> (Repli Steps L2VsRealTime MonApmc(K=k) | k=1,2,4)
```

```
repliTvsKR:
   (Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
   | Varying N, Nalpha, pAccMin)
-> (Repli (T MonApmc(K,N,Nalpha,pAccMin) / T Apmc(K,N,Nalpha,pAccMin) vs K - r)
   | Varying N, Nalpha, pAccMin)
```

As a consequence, we need read functions for the simulation specification objects (Simu ...) and for the simulation result objects (Res, Steps, Repli Res, Repli Steps). We also need write functions for the simulation results and the statistics.

The objects representing simulation specifications and simulation results can be written and read with haskell's Show and Read instances, because they will only be reread to be fed to the Haskell code that computes the statistics:

- `Simu a Toy` for any algorithm a
- `Steps a Toy`
- `Repli Res a Toy` 
- `Repli Steps a Toy`
- `Simu a_k Toy` for some MonAPMC algorithm where K varies 1,2,4
- `Repli Steps Res a_k`
- `a` for any algorithm a with varying K, N, Nalpha, pAccMin
- `Repli Res a` for any algorithm a with varying K, N, Nalpha, pAccMin

The Statistics morphisms take as input lists of simulation results:

- `(Steps Apmc Toy, Steps MonApmc Toy)`
- `(Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...)`
- `(Repli Steps Res MonApmc(K=k) | k=1,2,4)`
- `(Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)  | Varying N, Nalpha, pAccMin)`
 
We need read functions for those also based on haskell's Read instances. Since simulation results are written to independent files, these read functions must take in a list of file paths and read each file individually.

The Statistics objects (targets of the statistics morphisms) must be written with gnuplot's data format:

- `(Histo Steps APMC, Histo Steps MonAPMC)`: One file per algorithm. For each file contains one dataset per step, and eath dataset contains the histogram for the corresponding step. A record gives the location of the corresponding bar and its height.
- `(MeanStd L2VsNSimu Apmc1, ..., MeanStd L2VsNSimu MonApmc1, ...)`: Single file. One data set per distinct algorithm type (Apmc, MonApmc) and value of N and pAccMin. One record in a data set per value of Nalpha. Each record has the form `mean(l2) mean(NSimu) std(l2) std(nsimu)`
- `(Repli Steps L2VsRealTime MonApmc(K=k) | k=1,2,4)`: Single file. One algorithm with parameter values per dataset. One replication per record.
- `(Repli (T MonApmc(K,N,Nalpha,pAccMin) / T Apmc(K,N,Nalpha,pAccMin) vs K - r) | Varying N, Nalpha, pAccMin)`: Single file. One value of (N, Nalpha, pAccMin) per data set. One replication per record.



# Automation

To automate the running of the experiment and creation of figures, we use Make.
A morphism `f: A -> B` representing the executable `exe-f` can be materialized into the following make rule:

```Makefile
B: A
	exe-f
```

The composition of the morphisms `g: A -> B` and `f: B -> C` can be obtained simply by including both rules in the same Makefile.

```Makefile
B: A
	exe-f

C: B 
        exe-g
```

Zipping in a makefile is straightforward. The following Makefile implicitly provides the morphism `zip(f1, f2)`, such that we can then add morphisms that has as source the object `(B1, B2)`:

```Makefile
B1 : A1
	exe-f1
	
B2 : A2
	exe-f2
```



# Objectives

Write the read and write functions specified in the `F` functor section. Generate the `haskFile` executable with its subcommands that transform the desired Haskell morphisms into Files morphisms.

Construct all morphisms specified in the `Experiment -> File` functor section.

Put all File morphisms in a Makefile.


