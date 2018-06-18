cmToPix(cm, dpi) = dpi * cm * 2.54
fontScale(dpi) = sprintf("$d",dpi / 72.0)
set terminal png truecolor size cmToPix(7, 72),cmToPix(5, 72) font ",12"
set output "5steps.png"

bins = 300
lowerBound = -10
upperBound = 10

set xrange [-4:4]
set yrange [0:3]

set samples 500
set isosamples 500

normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)
posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)

set multiplot layout 3,5 columnsfirst
do for [i in "0 1 2 3 4"] {
  set title "lenormand2012 step ".i
  plot "../output/formulas/scaledHistogram/toy/lenormand2012_5000_0.10_0.01_".i."_1.csv" w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  set title "lenormand2012 easyABC step ".i
  plot "../output/easyABC/scaledHistogram/toy/lenormand2012_5000_0.10_0.01_".i."_1.csv" w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  set title "beaumont2009 easyABC step ".i
  plot "../output/easyABC/scaledHistogram/toy/beaumont2009_5000_2.00_0.01_".i."_1.csv" w boxes t "" fs solid, \
       posterior(x) t "" w l lc "black"

  #iSteadyState = 5000 * (i + 1)
  #set title "steadyState step ".iSteadyState
}
unset multiplot
