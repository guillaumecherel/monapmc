output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 400,400 font ',12'
set output output_path
# set xrange [-4:4]
# set yrange [0:3]
# set samples 500
# set isosamples 500

set key off

set xlabel "Bias factor"
set ylabel "L2 error"

set style circle radius 0.5 
set style fill solid

plot datafile using "biasFactor":"compL2MonApmc" with circles lc 3
