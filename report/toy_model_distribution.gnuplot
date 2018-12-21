# To use this script, set the variables 
#
# - outputPath
# - density
# - cdf
 
cmToPix(cm, dpi) = dpi * cm * 2.54
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output outputPath

plot density u 1:2 with lines, \
     cdf u 1:2 with lines


