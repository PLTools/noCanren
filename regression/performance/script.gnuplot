set terminal pdf enhanced
set output 'graph.pdf'

set style data histogram
set style histogram cluster gap 1

set style fill solid border rgb "black"
set auto x
set logscale y
set yrange [0.01:*]
plot 'data.gnuplot' using 2:xtic(1) title col \
      , '' using 3:xtic(1) title col \
      , '' using 4:xtic(1) title col \
      , '' using 5:xtic(1) title col
