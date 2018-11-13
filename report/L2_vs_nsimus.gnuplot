# To use this script, set the variables 
#
# - outputPath
# - lenormand2012
# - steadyState
 
cmToPix(cm, dpi) = dpi * cm * 2.54
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output outputPath

plot lenormand2012 index 0 with xyerrorbars lc 1 , \
     lenormand2012 index 1 with xyerrorbars lc 2 , \
     lenormand2012 index 2 with xyerrorbars lc 3 , \
     lenormand2012 index 3 with xyerrorbars lc 4 , \
     steadyState index 0 with xyerrorbars lc 5 , \
     steadyState index 1 with xyerrorbars lc 6 , \
     steadyState index 2 with xyerrorbars lc 7 , \
     steadyState index 3 with xyerrorbars lc 8

