bins = 300
lowerBound = -10
upperBound = 10

set xrange [-4:4]
set yrange [0:3]

set samples 500
set isosamples 500

normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)
posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)

set multiplot layout 4,5 columnsfirst
do for [i in "0 1 2 3 6"] {
  set title "lenormand2012 step ".i
  print "< ../bin/formulas toy_scaled_histogram  ../output/blocks/lenormand2012_5000_0.1_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins
  plot "< ../bin/formulas toy_scaled_histogram  ../output/blocks/lenormand2012_5000_0.1_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  set title "lenormand2012 easyABC step ".i
  print "< ../bin/formulas toy_scaled_histogram  ../output/easyABC/lenormand2012_5000_0.1_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins
  plot "< ../bin/formulas toy_scaled_histogram  ../output/easyABC/lenormand2012_5000_0.1_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  set title "beaumont2009 easyABC step ".i
  print "< ../bin/formulas toy_scaled_histogram  ../output/easyABC/beaumont2009_5000_2.0_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins
  plot "< ../bin/formulas toy_scaled_histogram  ../output/easyABC/beaumont2009_5000_2.0_0.01_".i."_1.csv ".lowerBound." ".upperBound." ".bins w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  iSteadyState = 5000 * (i + 1)
  set title "steadyState step ".iSteadyState
  print "< ../bin/formulas toy_scaled_histogram  ../output/blocks/steadyState_5000_0.1_0.01_1_".iSteadyState."_1.csv ".lowerBound." ".upperBound." ".bins
  plot "< ../bin/formulas toy_scaled_histogram  ../output/blocks/steadyState_5000_0.1_0.01_1_".iSteadyState."_1.csv ".lowerBound." ".upperBound." ".bins w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"
}
unset multiplot
