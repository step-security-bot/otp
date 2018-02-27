# Input file looks like this:
#
# Benchmarks BASE BASE-min BASE-max JIT JIT-min JIT-max
# small/aprettypr 3054964.000000 2963310.000000 3266618.000000 1782311.000000 1712167.000000 2203619.000000
# small/barnes 1268330.000000 1265391.000000 1272765.000000 1466650.000000 1462846.000000 1474103.000000
#
# The number of columns is sent into the program using the tags variable.

set terminal svg mouse standalone size 800,600

set output out
set grid ytics
set boxwidth 0.9 absolute
set style fill solid 1.00 border -1
set xtics nomirror rotate by -45
set yrange [0:1.5<*<3]
set style histogram errorbars gap 1 lw -1

plot for [i=0:tags-1] "<awk -f gnuplot_scripts/calc_average.awk ".data \
     u ($2/column(2+i*3)):($2/column(3+i*3)):($2/column(4+i*3)):xtic(1) \
     w hist ti column(2+i*3)
