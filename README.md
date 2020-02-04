This repository contains the numerical experiments to test the algorithm MonAPMC. This algorithm is an attempt to implement an APMC algorithm that scales well with the number of computing cores available to perform simulations and the complexity of the simulation model. It is essentially the algorithm APMC equipped with a specific parallelization scheme.

The initial problem is that a straightforward parallelization scheme like map-reduce is that at each iterations, as the simulations terminate one after an other, some computing cores become idle while we wait for the remaining simulation to complete before we can proceed to the next iteration of the algorithm and run a new batch of simulations. This induces a waste of computing power.

The potential loss grows with the number of computing cores available. The more core we have, the more we can leave some idle. With only one core, there is no loss possible: simulations are simply run one after another without any gap. With two cores, if we have only 2 simulations to run, the one that finishes first leaves its core idle until the other finishes. The problem keeps going as the number of cores grows.

The loss is potentially dampened if there are more simulations to run at each iteration than computing units. For example, if there are 10 cores and a 100 simulations, as the first 10 simulations terminate, the released core can be filled with the remaining simulations. The loss appears only at the 10 last simulations. Thus, the potential loss is not only affected by the number of cores, but by the ratio of the number of simulations and the number of cores. The worse case is when the number of simulation is equal to the number of cores and both are high.

The loss is also aggravated when some simulations take longer than others. An extreme case is when we have thousands of computing cores, most simulations run very quickly and a few take very long. The algorithm will have to wait for the longer simulations and the computing power not properly be harnessed.

We designed the algorithm MonAPMC to answer to this loss problem. We thus need to test that it is effectively more efficient than an easy to implement parallelisation scheme as the number of computing cores grows, as the ratio of number of simulations over the number of cores shrinks, and as the variance of simulation duration grows.

For a first check of these three claims, we plot the evolution L2 against time for a replicated run of both APMC and MonAPMC, varying some variables while keeping the others fixed at the following default values: n=5000, nAlpha=500, pAccMin=0.01, parallel=1, stepSize=1, stopSampleSize=4500. 

Fig L2 vs time K

:   A two column grid of plots showing L2 vs time for replicated runs. 
    Plots on the left column are runs of the APMC algorithm, plots on the right
    are runs of MonAPMC. Rows correspond to different values of parallelism K: 
    1, 2, 4, 10.

The first set of plots (`l2_vs_realtime_k`) shows this evolution for both algorithm and different values of K in 1, 2, 4, 10. These plots illustrate that:

- MonAPMC and APMC have a similar results in terms of time and quality,
- There both benefit from additionnal cores.
- They remain similar in quality and speed with more cores. 

MonAPMC offers no advantage here. It is expected from the absence of variance in the model run time and the fact that the number of simulation per iteration is a multiple of the number of cores. As a consequence the loss mentionned above should be absent: no cpu ever idles in APMC. The two following sets of figures will illustrate what happens when the number of simulations per run does not divide into the number of cores, and when the variance of the model run time increases.

In the second set, K takes the values 4 and 10 and N varies such that N / K takes the values 1, 1/2, 1/10. These figures illustrate that:

- The advantage of MonAPMC is not considerable with few (4) cores
- The advantage of MonAPMC is significant when the number of simulations per iteration is close to the number of cores. (On the grid with K = 100)

In the third set of figures, only the variance of simulation duration (V) varies taking the values 0, 1, 10. It illustrates that 

- The advantage of MonAPMC is more important when there are simulation that take longer than others.



At the core of MonAPMC's design is that we don't have to wait for the longer simulations to finish before going on with the algorithm. This is likely to introdue a bias in the posterior sample where the faster simulations will be more represented. We check for this bias by plotting a histogram of the posterior sample with the toy model where the higher values of theta lead to a higher simulation time.

The previous results gave us a first check that MonAPMC is more efficient than APMC with a simple parallelisation scheme with some default parameter values but we would like to build up a more comprehensive understanding of the conditions in which it is more efficient. 

To measure the efficiency of the parallelization scheme, we compare the algorithm to APMC with a simple parallelisation scheme and measure the ratio of the time required for each algorithm to reach an inferred posterior distribution is considered close enough to the theoretical posterior: 

```
T(ParApmc, ParMonApmc) = Ta(ParApmc) / Tm(ParMonApmc)
```

We want to check that this quantity 

- grows with the number of computing cores,
- shrinks with the ratio of the number of simulations over the number of cores,
- grows with the variance of simulation duration
- is greater than 1 for a reasonnable set of these values (realized in practice, corresponding to needs that exist, check with a few examples)

For a first check of the first three claims, we will plot the evolution of `T(ParApmc, ParMonApmc)` against the number of computing cores (K), the ratio of the number of simulations over the number of cores (N / K) and the variance of simulation duration (V)., for a fixed set of default settings parameter values.
We will sample the parameters K, N and V and with a latin hypercube sampling.



# Algorithm MonAPMC

```
split: S -> (S, S)
split empty = (empty, empty)
split s = (s, s with t0 is given the value of t)

merge (S, S) -> S
merge (s, s') = 
  let
    (s1, s2) = if (t0(s) <= s0(s')) 
      then (s, s') 
      else (s', s)
    -- Eliminate from s2 the particles that are duplicates from s1
    selected = particles from s1 and s2, filtering out from s2 those whose t value is <= t0(s2), meaning that they are duplicates from s1. Keep only the nAlpha resulting particles with the lowest rho values.
  in 
    S with p = p(s1)
           t0 = t0(s1)
           t = t(s1) + t(s2) - t0(s2)
           particles = selected
           epsilon = max rho(p) over all p in selected
           pAcc = number of particles kept from s2 * pAcc(s2) / number of particles in s2

step: S -> S
step empty = 
  let 
    thetas = sample (n - nAlpha) from prior sample
    xs = f(theta) for each theta in thetas
    rhos = euclidean distance between each x in xs and the observed data
    particles = (theta, x, rho, weight, ts) where theta, x and rho correspond to the values in the previous arrays and weight = 1 and ts = 1
    selected = the nAlpha particles with the lowest rho values 
  in
    S with t0 = 0
           t = 1
           particles = selected
           epsilon = max rho value from selected
           pAcc = 1
step s =
  if the number of particles in s < nAlpha
    then 
      merge s (step empty)
    else 
      let 
        thetas' = sample (n - nAlpha) particles from the weighted sample of particles in s
        sigmaSquared = 2 * weightedCovariance thetas(s)
        newThetas(i) = thetas'(i) + sample from normal(mean=0, var=sigmaSquared)
        newXs = f(theta) for all theta in newThetas
        newRhos = euclidean distance between each x in newXs and the observed data
        weight(theta) = sum over all theta' in thetas(s) of
          weight(theta')
          / (sum of weight(theta'') over all theta'' in thetas(s)) 
          * 1 / sqrt(det(2 * Pi * inverse(sigmaSquared)))
          * exp(-0.5 * transpose(theta - theta') * inverse(sigmaSquared) * (theta - thtea'))
        newParticles = (theta, x, rho, weight(theta), ts) where theta, x, rho correspond to the values in the previous arrays and ts = t(s) + 1
        selected = the nAlpha particles from s and newParticles with the lowest rho values 
      in
        S with t0 = t0(s)
               t = t(s) + 1
               particles = selected
               epsilon = max rho value from selected
               pAcc = number of particles kept from newParticles / (n - nAlpha)

stop: S -> Boolean
stop(s) = 
  -- if the state is empty or if the number of simulations performed is smaller than sss
  if s == empty or t * (n - nAlpha) < sss 
    then False
  else 
    pAccMin >= proportion the sss most recent simulations that were accepted

stepSize: Int
parallel: Int

start: [Job]
start = [runStep(s1), runStep(s2), ..., runStep(s_parallel)]
  where (s1, s1') = split empty
        (s2, s2') = split s1'
        ...
        (s_(parallel - 1), s_parallel') = split s_(parallel - 2)'

runStep : Int -> S -> Job
runStep stepSize s = step . step . ... . step 
// (compose the function step stepSize times)

waitForNext: [Job] -> (S, [Job])
waitForNext jobs = wait for the next job to finish and return its result along with the remaining jobs.

go: S -> [S] -> S
go current running = 
  let 
    (newest, stillRunning) = waitForNext(running)
    newState = merge current newest
  in
    if stop(newState)
      then newState
      else 
        let 
          -- split newState, keep the left part as the current state and add the right part to the list of running simulations and recurse.
          (new1, new2) = split newState
          go new1 (stillRunning ++ runStep(new2))

run :: S
run = go empty start
```


# Usage:

First, run `make setup` to populate the simultaion specification directory `input/simu/` with the simulations defined in `util/populate_simu_specs.sh` and create the required directories.

Then, run `make` to run the simulations, compute statistics and generate the figures automatically.


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


