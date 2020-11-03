# This script requires the following variables:
# output_path
# apmc_kLow_vLow
# apmc_kLow_vHigh
# apmc_kHigh_vLow
# apmc_kHigh_vHigh
# monApmc_kLow_vLow
# monApmc_kLow_vHigh
# monApmc_kHigh_vLow
# monApmc_kHigh_vHigh

set output output_path

# set xrange [0:*]
set yrange [0:.3]

set style line 81 lt 0 lc rgbcolor "#000000"
set grid xtics ytics linestyle 81

set ylabel "L2 error"
set xlabel "Total run time"

COLUMNS=3
ROWS=2
TOPMARGIN=0.1
BOTTOMMARGIN=0.12
LEFTMARGIN=0.12
RIGHTMARGIN=0.05
PLOTMARGINHORIZ=0.005
PLOTMARGINVERT=0.2
PLOTWIDTH=(1 - (COLUMNS - 1) * PLOTMARGINHORIZ - LEFTMARGIN - RIGHTMARGIN) / COLUMNS 
PLOTHEIGHT=(1 - (ROWS - 1) * PLOTMARGINVERT - TOPMARGIN - BOTTOMMARGIN) / ROWS

set multiplot layout 2,3 rowsfirst

SETMARGINS = " \
    set bmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINVERT) * (row - 1)); \
    set tmargin at screen (BOTTOMMARGIN + (PLOTHEIGHT + PLOTMARGINVERT) * (row - 1) + PLOTHEIGHT); \
    set lmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINHORIZ) * (col - 1)); \
    set rmargin at screen (LEFTMARGIN + (PLOTWIDTH + PLOTMARGINHORIZ) * (col - 1) + PLOTWIDTH)"

col = 1
row = 2
@SETMARGINS
set ylabel "L2 error"
set format y "%.2f"
set xrange [0:17000]
set xtics 5000
set xlabel ""
set title "K = 4\nvarRunTime = 0.1"
set key off
set grid xtics ytics linestyle 81
plot apmc_kLow_vLow u 3:4 with lines t 'APMC', \
     monApmc_kLow_vLow u 3:4 with lines t 'MonAPMC'

col = 2
row = 2
@SETMARGINS
set ylabel ""
set format y ""
set xlabel ""
set xrange [0:17000]
set xtics 5000
set title "K = 4\nvarRunTime = 1"
set key off
set grid xtics ytics linestyle 81
plot apmc_kLow_vHigh u 3:4 with lines t 'APMC', \
     monApmc_kLow_vHigh u 3:4 with lines t 'MonAPMC'

col = 3
row = 2
@SETMARGINS
set ylabel ""
set format y ""
set xlabel ""
set xrange [0:17000]
set xtics 5000
set key on
set title "K = 4\nvarRunTime = 10"
set grid xtics ytics linestyle 81
plot apmc_kLow_vBias u 3:4 with lines t 'APMC', \
     monApmc_kLow_vBias u 3:4 with lines t 'MonAPMC'

col = 1
row = 1
@SETMARGINS
set ylabel "L2 error"
set format y "%.2f"
set xlabel "Total run time"
set xrange [0:1200]
set xtics 250
set title "K = 100,\nvarRunTime = 0.1"
set key off
set grid xtics ytics linestyle 81
plot apmc_kHigh_vLow u 3:4 with lines t 'APMC', \
     monApmc_kHigh_vLow u 3:4 with lines t 'MonAPMC'
     
col = 2
row = 1
@SETMARGINS
set ylabel ""
set format y ""
set xlabel "Total run time"
set xrange [0:1200]
set xtics 250
set title "K = 100,\nvarRunTime = 1"
set key off
set grid xtics ytics linestyle 81
plot apmc_kHigh_vHigh u 3:4 with lines t 'APMC', \
     monApmc_kHigh_vHigh u 3:4 with lines t 'MonAPMC'
     

col = 3
row = 1
@SETMARGINS
set ylabel ""
set format y ""
set xlabel "Total run time"
set xrange [0:1200]
set xtics 250
set title "K = 100,\nvarRunTime = 10"
set key off
set grid xtics ytics linestyle 81
plot apmc_kHigh_vBias u 3:4 with lines t 'APMC', \
     monApmc_kHigh_vBias u 3:4 with lines t 'MonAPMC', \

unset multiplot

