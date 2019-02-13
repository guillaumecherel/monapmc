# To use this script, set the variables 
#
# - outputPath
# - formulas_lenormand2012
# - ...
# 
# interactively or from the command line with
# 'gnuplot -e "outputPath=..."'

rows = 6
columns = 5

cmToPix(cm, dpi) = dpi * cm * 2.54
fontScale(dpi) = sprintf("$d",dpi / 72.0)
set terminal png truecolor size cmToPix(7, 72),cmToPix(7, 72) font ",12"
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

set multiplot layout rows,columns rowsfirst

do for [i = 1:columns] {
  set title "lenormand2012 step ".i
  plot formulas_lenormand2012 index (i - 1) w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"
}

do for [i = 1:columns] {
set title "monAPMC step ".i."\nstepSize=1 parallel=2"
plot formulas_monAPMC index (i - 1) w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
}

do for [i = 1:columns] {
iSteadyState = 5000 * i
set title "steadyState step ".iSteadyState
plot formulas_steadyState index (i - 1) w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
}

do for [i = 1:columns] {
set title "lenormand2012 easyABC step ".i
plot easyABC_lenormand2012 index (i - 1) w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
}

do for [i = 1:columns] {
set title "beaumont2009 easyABC step ".i
plot easyABC_beaumont2009 index (i - 1) w boxes t "" fs solid, \
     posterior(x) t "" w l lc "black"
}

unset multiplot
