# Input file looks like this:
#
# Benchmarks BASE BASE-min BASE-max JIT JIT-min JIT-max
# 2018-03-08T16:59:43 3054964.000000 2963310.000000 3266618.000000 - - -
# 2018-03-08T16:59:65 - - - 1466650.000000 1462846.000000 1474103.000000
#
# The number of columns is sent into the program using the tags variable.

set terminal svg mouse standalone size 800,600

set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S"
set datafile missing "-"

set yrange [0:*]

set output out

plot for [i=0:tags-1] \
     data u 1:i*3+2:i*3+3:i*3+4 w errorbars ls i+1 notitle,\
     for [i=0:tags-1] \
     data u 1:2+i*3 w lines ls i+1 ti column(2 + i*3)