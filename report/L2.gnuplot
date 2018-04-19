cmToPix(cm, dpi) = dpi * cm / 2.54
set terminal png truecolor size cmToPix(7, 300),cmToPix(5, 300) font ",12"
set output "L2_vs_nsimus.png"

array alpha[9] = ["0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9"]
array pAccMin[4] = ["0.01", "0.05", "0.10", "0.20"]

plot for [i=1:4] "< ../bin/formulas L2_vs_number_simulations ../output/blocks/param_sampling/lenormand2012_5000_?.?_".pAccMin[i]."_*" t "lenormand2012 pAccMin=".pAccMin[i] w p lt i lc 1, \
     for [i=1:4] "< ../bin/formulas L2_vs_number_simulations ../output/blocks/param_sampling/steadyState_5000_?.?_".pAccMin[i]."_*" t "steadyState pAccMin=".pAccMin[i] w p lt i lc 2
