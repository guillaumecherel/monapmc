cmToPix(cm, dpi) = dpi * cm / 2.54
set terminal png truecolor size cmToPix(7, 300),cmToPix(5, 300) font ",12"
set output "L2_vs_nsimus.png"

array alpha[9] = ["0.10", "0.20", "0.30", "0.40", "0.50", "0.60", "0.70", "0.80", "0.90"]
array pAccMin[4] = ["0.01", "0.05", "0.10", "0.20"]

plot for [i=1:4] "../output/formulas/l2_vs_nsimus/toy/lenormand2012.csv" w p lt i lc 1, \
     for [i=1:4] "../output/formulas/l2_vs_nsimus/toy/steadyState.csv" w p lt i lc 2

