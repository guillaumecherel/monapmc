# To use this script, set the variables 
#
# - outputPath
# - lenormand2012
# - steadyState
 
cmToPix(cm, dpi) = dpi * cm * 2.54
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output outputPath

plot lenormand2012 w p lc 1, \
     steadyState w p lc 2

