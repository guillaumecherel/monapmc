output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 1500,500 font ',12'
set output output_path

set key on

#set style circle radius 2
#set style fill solid noborder 
#set key on right center opaque nobox

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set xlabel "sqrt(varRunTime) / meanRunTime"

set xtics rotate by -60
set log x
set xtics format "%g"

set multiplot layout 1,3
plot datafile using (column("varRunTime")):(column("parallel") == 8 ? column("compTimeRatio") : 1/0) with points lc 1
plot datafile using (column("varRunTime")):(column("parallel") == 80 ? column("compTimeRatio") : 1/0) with points lc 2
plot datafile using (sqrt(column("varRunTime")) / column("meanRunTime")):(column("parallel") == 1000 ?  column("compTimeRatio") : 1/0) with points lc 3 
unset multiplot

