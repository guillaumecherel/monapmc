# To use this script, set the variables 
#
# - outputPath
# - lenormand2012
# - monAPMC
 
cmToPix(cm, dpi) = dpi * cm * 2.54
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output outputPath

# set yrange [0:.25]
set ytics 0,.05,.2
# set xrange [0.2e5:33e5]
set logscale x 2
set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)

plot lenormand2012 index 0 with xyerrorbars lc 1 , \
     lenormand2012 index 1 with xyerrorbars lc 1 , \
     lenormand2012 index 2 with xyerrorbars lc 1 , \
     lenormand2012 index 3 with xyerrorbars lc 1 , \
     monAPMC index 0 with xyerrorbars lc 2 , \
     monAPMC index 1 with xyerrorbars lc 2 , \
     monAPMC index 2 with xyerrorbars lc 2 , \
     monAPMC index 3 with xyerrorbars lc 2

