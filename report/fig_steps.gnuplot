output_path=ARG1
data_apmc=ARG2
data_mon_apmc=ARG3

set xrange [-3:3]
set yrange [0:2.2]
# set samples 500
# set isosamples 500
set style fill solid 1.0

set key off

posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)
normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)

set output output_path

COLUMNS=5
ROWS=2
LEFTMARGIN=0.1
RIGHTMARGIN=0.02
PLOTMARGINH=0.0
PLOTWIDTH=(1 - (COLUMNS - 1) * PLOTMARGINH - LEFTMARGIN - RIGHTMARGIN) / COLUMNS

TOPMARGIN=0.1
BOTTOMMARGIN=0.15
PLOTMARGINV=0.01
PLOTHEIGHT=(1 - (ROWS - 1) * PLOTMARGINV - TOPMARGIN - BOTTOMMARGIN) / ROWS

set multiplot layout ROWS,COLUMNS rowsfirst

set grid
set ytics 0, 1, 2 scale 0
set xtics -2, 2, 2  scale 0

set format y "%.0f"
set format x ""

set ylabel "APMC\np(θ∣y=yObs)"
set xlabel ''

set tmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINV) * 1)
set bmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINV) * 1 + PLOTHEIGHT)

do for [i in "1 2 3 4 5"] {
  set lmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINH) * (i - 1))
  set rmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINH) * (i - 1) + PLOTWIDTH)
  set title "Step ".i
  plot \
    data_apmc \
      index (i - 1) using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
    posterior(x) linecolor black dashtype 1 with line
  set format y ""
  set ylabel ""

}

set format y "%.0f"
set format x "%.0f"

set ylabel "MonAPMC\np(θ∣y=yObs)"
set xlabel "θ"

set tmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINV) * 0)
set bmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINV) * 0 + PLOTHEIGHT)

do for [i in "1 2 3 4 5"] {
  set lmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINH) * (i - 1))
  set rmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINH) * (i - 1) + PLOTWIDTH)
  set title ""
  plot \
    data_mon_apmc \
      index (i - 0) using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
    posterior(x) linecolor black dashtype 1 with line
  set format y ""
  set ylabel ""
}

unset multiplot
