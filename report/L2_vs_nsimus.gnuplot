cmToPix(cm, dpi) = dpi * cm / 2.54
set terminal png truecolor size cmToPix(7, 300),cmToPix(5, 300) font ",12"
set output "L2_vs_nsimus.png"

plot "../output/formulas/l2_vs_nsimus/toy/lenormand2012.csv" w p lc 1, \
     "../output/formulas/l2_vs_nsimus/toy/steadyState.csv" w p lc 2

