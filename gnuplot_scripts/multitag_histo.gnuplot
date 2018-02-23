set terminal pngcairo  transparent noenhanced font "arial,10" fontscale 1.0
set output out
set boxwidth 0.9 absolute
set style fill solid 1.00 border -1
set yrange [0:]
set style histogram errorbars gap 1 lw -1
plot for [i=0:tags-1] data u (column(2+i*3)/$2):(column(3+i*3)/$2):(column(4+i*3)/$2):xtic(1) w hist ti column(2+i*3)