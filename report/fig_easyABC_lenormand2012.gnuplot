binwidth = 0.1
bin(val) = binwidth * floor(val / binwidth)

set yrange [0:2]
set xrange [-50:50]

set samples 500
set isosamples 500

normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)
posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)

plot "../../output/easyABC/lenormand2012.csv" u (bin(column(1))):(1.0) smooth fnormal w boxes t "", \
     posterior(x) t ""
