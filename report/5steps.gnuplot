# To use this script, set the variables 
#
# - outputPath
# - formulas_lenormand2012_1, ..., formulas_lenormand2012_5,
# - formulas_steadyState_1, ..., formulas_steadyState_5
# - easyABC_lenormand2012_1, ... easyABC_lenormand2012_5
# - easyABC_beaumont2009_1, ... easyABC_beaumont2009_5
# 
# interactively or from the command line with
# 'gnuplot -e "formulas_lenormand2012_1=..."'

cmToPix(cm, dpi) = dpi * cm * 2.54
fontScale(dpi) = sprintf("$d",dpi / 72.0)
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output outputPath

bins = 300
lowerBound = -10
upperBound = 10

set xrange [-4:4]
set yrange [0:3]

set samples 500
set isosamples 500

normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)
posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)

set multiplot layout 5,5 rowsfirst

set title "lenormand2012 step 1"
plot formulas_lenormand2012_1 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 step 2"
plot formulas_lenormand2012_2 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 step 3"
plot formulas_lenormand2012_3 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 step 4"
plot formulas_lenormand2012_4 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 step 5"
plot formulas_lenormand2012_5 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"

set title "monAPMCSeq step 1"
plot formulas_monAPMCSeq_1 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "monAPMCSeq step 2"
plot formulas_monAPMCSeq_2 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "monAPMCSeq step 3"
plot formulas_monAPMCSeq_3 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "monAPMCSeq step 4"
plot formulas_monAPMCSeq_4 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "monAPMCSeq step 5"
plot formulas_monAPMCSeq_5 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"

iSteadyState = 5000 
set title "steadyState step ".iSteadyState
plot formulas_steadyState_1 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
iSteadyState = 10000 
set title "steadyState step ".iSteadyState
plot formulas_steadyState_2 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
iSteadyState = 15000 
set title "steadyState step ".iSteadyState
plot formulas_steadyState_3 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
iSteadyState = 20000 
set title "steadyState step ".iSteadyState
plot formulas_steadyState_4 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
iSteadyState = 25000 
set title "steadyState step ".iSteadyState
plot formulas_steadyState_5 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"

set title "lenormand2012 easyABC step 1"
plot easyABC_lenormand2012_1 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 easyABC step 2"
plot easyABC_lenormand2012_2 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 easyABC step 3"
plot easyABC_lenormand2012_3 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 easyABC step 4"
plot easyABC_lenormand2012_4 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "lenormand2012 easyABC step 5"
plot easyABC_lenormand2012_5 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"

set title "beaumont2009 easyABC step 1"
plot easyABC_beaumont2009_1 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "beaumont2009 easyABC step 2"
plot easyABC_beaumont2009_2 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "beaumont2009 easyABC step 3"
plot easyABC_beaumont2009_3 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "beaumont2009 easyABC step 4"
plot easyABC_beaumont2009_4 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
set title "beaumont2009 easyABC step 5"
plot easyABC_beaumont2009_5 w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"

unset multiplot
