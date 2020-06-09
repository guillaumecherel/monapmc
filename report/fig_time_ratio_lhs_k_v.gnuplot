output_path=ARG1
data=ARG2 

set terminal pngcairo truecolor size 600,300 font ',12'
set output output_path

set ylabel "Parallelism"
set xlabel "Model run time variance"
set zlabel "Time ratio"

set multiplot layout 1,2 columnsfirst

set view map
plot data using 1:2:3 with image 

plot data using 1:2:4 with image 

unset multiplot

