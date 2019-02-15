# To use this script, set the variables 
#
# - outputPath
# - lenormand2012
# - monAPMC_<par>

cmToPix(cm, dpi) = dpi * cm * 2.54
set terminal png truecolor size cmToPix(4, 72),cmToPix(5, 72) font ",12"
set output outputPath

set yrange [0:*]
# set ytics 0,.05,.2
# set xrange [0.2e5:33e5]

plot lenormand2012 u 2:3 lc 1 w lp, \
     monAPMC_1 u 2:3 lc 2 w lp, \
     monAPMC_2 u 2:3 lc 2 w lp, \
     monAPMC_3 u 2:3 lc 2 w lp, \
     monAPMC_4 u 2:3 lc 2 w lp


