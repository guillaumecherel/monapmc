# Directories

- input/
  - simu: simulation specifications for all the experiments
- output/formulas/
  - figure_data: input data for figures
  - run: simulation results
  - steps: simulation steps
  - repli/ 
    - run: replicated simulation results
    - run: replicated simulation steps

# Targets: 

FigSteps: histogram of posterior sample at each steps for an example run of APMC and MonAPMC

FigL2VSNSimu: L2 vs nSimus aggregated over replications (mean and std) of APMC and MonAPMC each with varying pAccMin, n, nAlpha

Fig0APMC: Series of L2 vs real time for some replications of APMC with varying K and a few samples of the other parameters

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


