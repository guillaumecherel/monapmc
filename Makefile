reports: report/5steps.png report/L2_vs_nsimus.png report/L2_vs_alpha.png report/nsimus_vs_alpha.png

report/5steps.png: report/5steps.gnuplot bin/formulas $(wildcard output/blocks/5steps/*.csv) $(wildcard output/easyABC/lenormand2012_*.csv)
	cd report; gnuplot -c 5steps.gnuplot

report/L2_vs_nsimus.png: report/L2_vs_nsimus.gnuplot bin/formulas $(wildcard output/blocks/param_sampling/lenormand2012_*.csv) $(wildcard output/blocks/param_sampling/steadyState_*.csv)
	cd report; gnuplot -c L2_vs_nsimus.gnuplot

report/L2_vs_alpha.png: report/L2_vs_alpha.gnuplot bin/formulas $(wildcard output/blocks/param_sampling/lenormand2012_*.csv) $(wildcard output/blocks/param_sampling/steadyState_*.csv)
	cd report; gnuplot -c L2_vs_alpha.gnuplot

report/nsimus_vs_alpha.png: report/nsimus_vs_alpha.gnuplot bin/formulas $(wildcard output/blocks/param_sampling/lenormand2012_*.csv) $(wildcard output/blocks/param_sampling/steadyState_*.csv)
	cd report; gnuplot -c nsimus_vs_alpha.gnuplot

$(shell ls -v output/blocks/param_sampling/lenormand2012_*.csv | head -n 1): blocks-simulations/src/main/scala/ToyModel.scala blocks-simulations/src/main/scala/SimulationSampleParamsLenormand2012.scala
	@echo "Simulation results for parameter sampling lenormand2012 are outdated. Please compile and run the simulations."

$(shell ls -v output/blocks/param_sampling/steadyState_*.csv | head -n 1): blocks-simulations/src/main/scala/ToyModel.scala blocks-simulations/src/main/scala/SimulationSampleParamsSteadyState.scala
# 	@echo "Simulation results for parameter sampling steadyState are outdated. Please compile and run the simulations."

bin/formulas: formulas/src/Main.hs
	cd formulas; stack build
