reports: report/5steps.png report/L2_vs_nsimus.png

report/5steps.png: report/5steps.gnuplot bin/formulas
	cd report; gnuplot -c 5steps.gnuplot

report/L2_vs_nsimus.png: report/L2.gnuplot bin/formulas
	cd report; gnuplot -c L2.gnuplot
