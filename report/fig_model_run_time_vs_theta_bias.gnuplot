output_path=ARG1

set output output_path

set key on left top

set style circle radius 0.03
set style fill solid  

set ylabel "Model run time"
set xlabel "θ"

set xtics 1

set xrange [-5:5]
set yrange [0:6]

set xtics 2 

set clip two 

bias_threshold = 0
mean_run_time = 1
var_run_time = 2 

expected_run_time(theta, bias_factor, mean) = \
  theta < bias_threshold ? mean : (bias_factor * mean)

std_run_time(theta, bias_factor, var) = \
  theta < bias_threshold ? sqrt(var) : sqrt(bias_factor ** 2 * var)
  
print expected_run_time(-1, 1, mean_run_time)
print std_run_time(-1, 1, var_run_time)
print std_run_time(0, 1, var_run_time)
print std_run_time(1, 1, var_run_time)
print std_run_time(2, 1, var_run_time)

set multiplot layout 1,3

set title "biasFactor = 1"
plot expected_run_time(x, 1, mean_run_time) lc 3 t "biasFactor = 1", \
     expected_run_time(x, 1, mean_run_time) - 0.5 * std_run_time(x, 1, var_run_time) lc 3 dt 2, \
     expected_run_time(x, 1, mean_run_time) + 0.5 * std_run_time(x, 1, var_run_time) lc 3 dt 2

set title "biasFactor = 2"
plot expected_run_time(x, 2, mean_run_time) lc 3 t "biasFactor = 2", \
     expected_run_time(x, 2, mean_run_time) - 0.5 * std_run_time(x, 2, var_run_time) lc 3 dt 2, \
     expected_run_time(x, 2, mean_run_time) + 0.5 * std_run_time(x, 2, var_run_time) lc 3 dt 2

set title "biasFactor = 3"
plot expected_run_time(x, 3, mean_run_time) lc 3 t "biasFactor = 3", \
     expected_run_time(x, 3, mean_run_time) - 0.5 * std_run_time(x, 3, var_run_time) lc 3 dt 2, \
     expected_run_time(x, 3, mean_run_time) + 0.5 * std_run_time(x, 3, var_run_time) lc 3 dt 2

unset multiplot
